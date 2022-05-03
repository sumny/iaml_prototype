#' @title Multi-objective Hyperparameter Optimization, Feature Selection and Interaction and Monotonicity Constraints
#'
#' @name mlr_tuner_iaml_ea
#'
#' @description
#' Performs joint multi-objective optimization of hyperparameters, feature selection and interaction and monotonicity
#' constraints of a suitable [mlr3::Learner].
#'
#' This requires an appropriate [mlr3::Learner], that allows for selecting features, and setting interaction and
#' monotonicity constraints, e.g., xgboost.
#'
#' @templateVar id iaml_ea
#' @template section_dictionary_tuners
#'
#' @section Parameters:
#' \describe{
#' \item{`select_id`}{`character(1)`\cr
#' ID of param in Learner that selects features.}
#' \item{`interaction_id`}{`character(1)`\cr
#' ID of param in Learner that sets interaction constraints.}
#' \item{`monotone_id`}{`character(1)`\cr
#' ID of param in Learner that sets monotonicity constraints.}
#' \item{`batch_size`}{`integer(1)`\cr
#' Maximum number of points to try in a batch.}
#' }
#'
#' @template section_progress_bars
#' @template section_logging
#'
#' @family Tuner
#'
#' @export
TunerIAMLEA = R6Class("TunerIAMLEA",
  inherit = mlr3tuning::Tuner,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        select_id = p_uty(tags = "required"),
        interaction_id = p_uty(tags = "required"),
        monotone_id = p_uty(tags = "required"),
        lambda = p_int(default = 10L, tags = "required"),
        mu = p_int(default = 100L, tags = "required")
      )
      param_set$values = list(select_id = "select.selector", interaction_id = "classif.xgboost.interaction_constraints", monotone_id = "classif.xgboost.monotone_constraints", lambda = 10L, mu = 100L)
      super$initialize(
        param_set = param_set,
        param_classes = c("ParamDbl", "ParamFct", "ParamInt", "ParamLgl", "ParamUty"),
        properties = "multi-crit",
        #packages = "iaml",
        label = "FIXME:",
        man = "iaml::mlr_tuners_iaml_ea"
      )
    }
  ),

  private = list(
    .optimize = function(inst) {
      # mu + lambda
      # parent selection: bbotk::nds_selection
      lambda = self$param_set$values$lambda  # FIXME: must be even and >= 2
      mu = self$param_set$values$mu
      select_id = self$param_set$values$select_id
      interaction_id = self$param_set$values$interaction_id
      monotone_id = self$param_set$values$monotone_id
      param_ids = setdiff(inst$search_space$ids(), c(select_id, interaction_id, monotone_id))
      param_space = ParamSet$new(inst$search_space$params[param_ids])
      param_space$trafo = inst$search_space$trafo
      param_space$deps = inst$search_space$deps
      sds = map(names(which(param_space$is_number)), function(param_id_number) (1 / 12) * (param_space$params[[param_id_number]]$upper - param_space$params[[param_id_number]]$lower) ^ 2)  # FIXME: log scale
      names(sds) = names(which(param_space$is_number))

      task = inst$objective$task
      features = task$feature_names
      n_features = length(task$feature_names)

      # initial population
      population = generate_design_random(param_space, n = mu)$data  # param_space

      sIm = map_dtr(seq_len(mu), function(i) {  # sIm space
        n_selected = sample(seq_len(n_features), size = 1L)
        selected_features = sample(features, size = n_selected, replace = FALSE)
        interactions = sample_interactions_random(selected_features)
        eqcs = get_eqcs(interactions)
        s = selector_name(selected_features)
        attr(s, "s_bit") = as.integer(features %in% selected_features)
        attr(s, "n_selected") = n_selected
        attr(s, "n_selected_total") = n_features
        I = list(I = get_matrix(eqcs), classes = map(eqcs, function(x) match(x, selected_features)))
        m = sample_m(I)
        interaction_constraints = I$classes
        n_interactions = sum(I$I)
        n_interactions_total = nrow(I$I) ^ 2L
        I = map(interaction_constraints, function(x) x - 1L)
        attr(I, "I_representation") = interactions
        attr(I, "n_interactions") = n_interactions
        attr(I, "n_interactions_total") = n_interactions_total
        n_non_monotone = sum(m == 0)
        n_non_monotone_total = length(m)
        attr(m, "n_non_monotone") = n_non_monotone
        attr(m, "n_non_monotone_total") = n_non_monotone_total
        data.table(s = list(s), I = list(I), m = list(m))
      })
      colnames(sIm) = c(select_id, interaction_id, monotone_id)

      population = cbind(population, sIm)
      gen = 0
      population[, generation := gen]
      population[, status := "alive"]

      # FIXME: bug in mlr3? learner param set is not pertained correctly, therefore values are always the same, therefore fix this in Tuners for now
      for (i in seq_len(nrow(population))) {
        inst$eval_batch(population[i, ])
      }

      repeat {  # iterate until we have an exception from eval_batch
        gen = gen + 1
        all_data = inst$archive$data[status == "alive"]
        data = all_data[, inst$archive$cols_y, with = FALSE]
        stopifnot(colnames(data) == inst$objective$codomain$ids())
        data[, id := seq_len(.N)]
        ys = t(t(data[, - "id"]) * mult_max_to_min(inst$objective$codomain))
        nadir = apply(ys, MARGIN = 2L, FUN = function(x) max(x) + 1)

        children = map_dtr(seq_len(lambda / 2), function(i) {
          candidate_ids = sample(nrow(ys), size = lambda, replace = FALSE)
          candidate_ys = ys[candidate_ids, ]
          parent_ids = candidate_ids[nds_selection(t(candidate_ys), n_select = 2L, ref_point = nadir, minimize = TRUE)]
          parents = all_data[parent_ids, inst$archive$cols_x, with = FALSE]

          # param_space
          # Gaussian or uniform discrete mutation for HPs
          for (j in seq_len(nrow(parents))) {
            for(param_id in param_ids) {
              parents[j, eval(param_id) := mutate(get(param_id), param = param_space$params[[param_id]], sdx = sds[[param_id]])]
            }
          }

          # Uniform crossover for HPS; p could be individual for each HP
          p_param_space_cross = 0.2  # FIXME: HP
          crossover_ps = runif(length(param_ids), min = 0, max = 1)
          param_ids_to_cross = param_ids[which(crossover_ps <= p_param_space_cross)]
          tmp = parents[1L, ]
          for (param_id in param_ids_to_cross) {
            parents[1L, eval(param_id) := parents[2L, ][[param_id]]]
            parents[2L, eval(param_id) := tmp[[param_id]]]
          }

          # sIm space
          # if we do s, we also do I and m, if we do I, we also do m; due to hierarchies
          to_do = sample(c("s", "I", "m", "skip"), size = 1L, prob = c(0.2, 0.15, 0.1, 0.55))
          if (to_do == "s") {
            # s mutation
            for (j in seq_len(nrow(parents))) {
              parents[j, eval(select_id) := mutate_s(get(select_id), features = features)]
            }
            # s crossover
            crossovers = crossover_s(parents[1L, ][[select_id]], parents[2L, ][[select_id]], features = features)
            parents[1L, ][[select_id]] = crossovers[1L]
            parents[2L, ][[select_id]] = crossovers[2L]

            # fix I, m
            selected_features1 = features[as.logical(attr(parents[1L, ][[select_id]][[1L]], "s_bit"))]
            selected_features2 = features[as.logical(attr(parents[2L, ][[select_id]][[1L]], "s_bit"))]
            interactions1 = sample_interactions_random(selected_features1)
            interactions2 = sample_interactions_random(selected_features2)
            eqcs1 = get_eqcs(interactions1)
            eqcs2 = get_eqcs(interactions2)
            I1 = list(I = get_matrix(eqcs1), classes = map(eqcs1, function(x) match(x, selected_features1)))
            I2 = list(I = get_matrix(eqcs2), classes = map(eqcs2, function(x) match(x, selected_features2)))
            m1 = sample_m(I1)
            m2 = sample_m(I2)

            interaction_constraints1 = I1$classes
            n_interactions1 = sum(I1$I)
            n_interactions_total = nrow(I1$I) ^ 2L
            I1 = map(interaction_constraints1, function(x) x - 1L)
            attr(I1, "I_representation") = interactions1
            attr(I1, "n_interactions") = n_interactions1
            attr(I1, "n_interactions_total") = n_interactions_total

            interaction_constraints2 = I2$classes
            n_interactions2 = sum(I2$I)
            I2 = map(interaction_constraints2, function(x) x - 1L)
            attr(I2, "I_representation") = interactions2
            attr(I2, "n_interactions") = n_interactions2
            attr(I2, "n_interactions_total") = n_interactions_total

            n_non_monotone1 = sum(m1 == 0)
            n_non_monotone_total = length(m1)
            attr(m1, "n_non_monotone") = n_non_monotone1
            attr(m1, "n_non_monotone_total") = n_non_monotone_total

            n_non_monotone2 = sum(m2 == 0)
            attr(m2, "n_non_monotone") = n_non_monotone2
            attr(m2, "n_non_monotone_total") = n_non_monotone_total

            parents[1L, ][[interaction_id]] = list(I1)
            parents[2L, ][[interaction_id]] = list(I2)

            parents[1L, ][[monotone_id]] = list(m2)
            parents[2L, ][[monotone_id]] = list(m2)
          }
          # FIXME: I, m

          parents
        })
        stopifnot(nrow(children) == lambda)
        children[, generation := gen]
        
        # see FIXME: above
        for (i in seq_len(nrow(children))) {
          inst$eval_batch(children[i, ])
        }
        ys = t(t(inst$archive$data[, inst$archive$cols_y, with = FALSE]) * mult_max_to_min(inst$objective$codomain))
        nadir = apply(ys, MARGIN = 2L, FUN = function(x) max(x) + 1)
        alive_ids = nds_selection(t(ys), n_select = mu, ref_point = nadir, minimize = TRUE)
        inst$archive$data[, status := "dead"]
        inst$archive$data[alive_ids, status := "alive"]
      }

      inst
    }
  )
)

mutate = function(value, param, sdx) {
  # FIXME: p, sigma HPs of Tuner; p could be individual for each HP
  p = 0.2
  sigma = 1
  stopifnot(param$class %in% c("ParamDbl", "ParamFct", "ParamInt", "ParamLgl"))
  if (runif(1L, min = 0, max = 1) >= p) {
    return(value)  # early exit
  }
  if (param$class %in% c("ParamDbl", "ParamInt")) {
    value = value + rnorm(1L, mean = 0, sd = sigma * sdx)
    if (param$class == "ParamInt") {
      value = round(value, 0L)
    }
    value = min(max(value, param$lower), param$upper)
  } else if (param$class %in% c("ParamFct", "ParamLgl")) {
    value = sample(param$levels, size = 1L)
  }
  value
}

mutate_s = function(value, features) {
  # FIXME: p HP of Tuner
  p = 0.2
  value = value[[1L]]
  s_bit = attr(value, "s_bit")
  to_mutate = runif(length(s_bit), min = 0, max = 1) <= p
  s_bit[to_mutate] = sample(c(0L, 1L), size = sum(to_mutate), prob = c(0.5, 0.5), replace = TRUE)
  selected_features = features[as.logical(s_bit)]
  s = selector_name(selected_features)
  attr(s, "s_bit") = s_bit
  attr(s, "n_selected") = sum(s_bit)
  attr(s, "n_selected_total") = attr(value, "n_selected_total")
  list(s)
}

crossover_s = function(value1, value2, features) {
  # FIXME: p HP of Tuner
  p = 0.2
  value1 = value1[[1L]]
  value2 = value2[[1L]]
  s_bit1 = attr(value1, "s_bit")
  s_bit2 = attr(value2, "s_bit")
  to_crossover = runif(length(s_bit1), min = 0, max = 1) <= p
  tmp = s_bit1
  s_bit1[to_crossover] = s_bit2[to_crossover]
  s_bit2[to_crossover] = tmp[to_crossover]
  selected_features1 = features[as.logical(s_bit1)]
  selected_features2 = features[as.logical(s_bit2)]
  s1 = selector_name(selected_features1)
  s2 = selector_name(selected_features2)
  attr(s1, "s_bit") = s_bit1
  attr(s1, "n_selected") = sum(s_bit1)
  attr(s1, "n_selected_total") = attr(value1, "n_selected_total")
  attr(s2, "s_bit") = s_bit2
  attr(s2, "n_selected") = sum(s_bit2)
  attr(s2, "n_selected_total") = attr(value1, "n_selected_total")
  list(s1, s2)
}



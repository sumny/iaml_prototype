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
        mu = p_int(default = 10L, tags = "required")
      )
      param_set$values = list(select_id = "select.selector", interaction_id = "classif.xgboost.interaction_constraints", monotone_id = "classif.xgboost.monotone_constraints", lambda = 10L, mu = 10L)
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
      lambda = self$param_set$values$lambda  # must be even >= 2
      mu = self$param_set$values$mu
      select_id = self$param_set$values$select_id
      interaction_id = self$param_set$values$interaction_id
      monotone_id = self$param_set$values$monotone_id
      param_ids = setdiff(inst$search_space$ids(), c(select_id, interaction_id, monotone_id))
      param_space = ParamSet$new(inst$search_space$params[param_ids])
      param_space$trafo = inst$search_space$trafo
      param_space$deps = inst$search_space$deps
      sds = map(names(which(param_space$is_number)), function(param_id_number) (1 / 12) * (param_space$params[[param_id_number]]$upper - param_space$params[[param_id_number]]$lower) ^ 2)
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
        attr(s, "n_selected") = n_selected
        attr(s, "n_selected_total") = n_features
        I = list(I = get_matrix(eqcs), classes = map(eqcs, function(x) match(x, selected_features)))
        m = sample_m(I)
        interaction_constraints = I$classes
        n_interactions = sum(I$I)
        n_interactions_total = nrow(I$I) ^ 2L
        I = map(interaction_constraints, function(x) x - 1L)
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

          # Gaussian or uniform discrete mutation for HPs
          for (j in seq_len(nrow(parents))) {
            for(param_id in param_ids) {
              parents[j, eval(param_id) := mutate(get(param_id), param = param_space$params[[param_id]], sdx = sds[[param_id]])]
            }
          }

          # Uniform crossover for HPS
          p = 0.2  # FIXME: HP
          crossover_ps = runif(length(param_ids), min = 0, max = 1)
          param_ids_to_cross = param_ids[which(crossover_ps <= p)]
          tmp = parents[1L, ]
          for (param_id in param_ids_to_cross) {
            parents[1L, eval(param_id) := parents[2L, ][[param_id]]]
            parents[2L, eval(param_id) := tmp[[param_id]]]
          }

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
  # p, sigma HPs of Tuner
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

mutate_sIm = function(value, param_id) {
  # FIXME:
}






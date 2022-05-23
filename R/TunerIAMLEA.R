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

      # initial population
      population = generate_design_random(param_space, n = mu)$data  # param_space
      sIm = map_dtr(seq_len(mu), function(i) {  # sIm space
        iaml = IAMLPoint$new(task)
        data.table(iaml = list(iaml),
                   s = list(iaml$create_selector()),
                   I = list(iaml$create_interaction_constraints()),
                   m = list(iaml$create_monotonicity_constraints()))
      })
      colnames(sIm) = c("iaml", select_id, interaction_id, monotone_id)

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
          parents = transpose_list(all_data[parent_ids, c("iaml", inst$archive$cols_x), with = FALSE])

          # param_space
          # Gaussian or uniform discrete mutation for HPs
          for (j in 1:2) {
            for(param_id in param_ids) {
              parents[[j]][[param_id]] = mutate(parents[[j]][[param_id]], param = param_space$params[[param_id]], sdx = sds[[param_id]])
            }
          }
          # Uniform crossover for HPS; p could be individual for each HP
          p_param_space_cross = 0.2  # FIXME: HP
          crossover_ps = runif(length(param_ids), min = 0, max = 1)
          param_ids_to_cross = param_ids[which(crossover_ps <= p_param_space_cross)]
          tmp = parents[[1L]]
          for (param_id in param_ids_to_cross) {
            parents[[1L]][[param_id]] = parents[[2L]][[param_id]]
            parents[[2L]][[param_id]] = tmp[[param_id]]
          }

          # sIm space
          iaml1 = parents[[1L]][["iaml"]]$clone(deep = TRUE)
          iaml2 = parents[[2L]][["iaml"]]$clone(deep = TRUE)

          iaml1$mutate()
          iaml2$mutate()

          crossing_sections = iaml1$get_crossing_sections(iaml2)
          tmp = iaml1$clone(deep = TRUE)
          iaml1$crossover(iaml2, crossing_sections = crossing_sections)
          iaml2$crossover(tmp, crossing_sections = rev(crossing_sections))

          parents[[1L]][["iaml"]] = iaml1
          parents[[2L]][["iaml"]] = iaml2

          as.data.table(transpose_list(parents))
        })
        stopifnot(nrow(children) == lambda)
        children[, generation := gen]
        
        # see FIXME: above
        for (i in seq_len(nrow(children))) {
          inst$eval_batch(children[i, ])
        }

        # NSGA-II stuff
        # could do this only based on gen and gen - 1L
        ys = t(t(inst$archive$data[, inst$archive$cols_y, with = FALSE]) * mult_max_to_min(inst$objective$codomain))
        rankings = emoa::nds_rank(t(ys))  # non-dominated fronts
        cds = map_dtr(unique(rankings), function(ranking) {  # crowding distances
          ids = which(rankings == ranking)
          data.table(id = ids, cd = emoa::crowding_distance(t(ys[ids, ])))
        })
        setorderv(cds, "id")
        alive_ids = integer(mu)
        current_front = 0L
        while(sum(alive_ids == 0L) != 0L) {
          current_front = current_front + 1L
          candidate_ids = which(rankings == current_front)
          to_insert = sum(alive_ids == 0L)
          if (length(candidate_ids) <= to_insert) {
            alive_ids[alive_ids == 0][seq_along(candidate_ids)] = candidate_ids
          } else {
            alive_ids[alive_ids == 0] = candidate_ids[order(cds[candidate_ids, ]$cd)][seq_len(to_insert)]
          }
        }
        stopifnot(length(unique(alive_ids)) == length(alive_ids))
        inst$archive$data[, status := "dead"]
        inst$archive$data[alive_ids, status := "alive"]
      }

      inst
    }
  )
)

mutate = function(value, param, sdx) {
  # FIXME: log scale
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


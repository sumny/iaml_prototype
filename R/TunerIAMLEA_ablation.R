#' @title Multi-objective Hyperparameter Optimization, Feature Selection and Interaction and Monotonicity Constraints
#'
#' @name mlr_tuner_iaml_eax
#'
#' @description
#' Performs joint multi-objective optimization of hyperparameters, feature selection and interaction and monotonicity
#' constraints of a suitable [mlr3::Learner].
#'
#' This requires an appropriate [mlr3::Learner], that allows for selecting features, and setting interaction and
#' monotonicity constraints, e.g., xgboost.
#'
#' @templateVar id iaml_eax
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
#' \item{`mu`}{`integer(1)`\cr
#' Population size.}
#' \item{`lambda`}{`integer(1)`\cr
#' Offspring size of each generation.}}
#'
#' @template section_progress_bars
#' @template section_logging
#'
#' @family Tuner
#'
#' @export
TunerIAMLEAX = R6Class("TunerIAMLEAX",
  inherit = mlr3tuning::Tuner,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        select_id = p_uty(tags = "required"),
        interaction_id = p_uty(tags = "required"),
        monotone_id = p_uty(tags = "required"),
        mu = p_int(default = 30L, tags = "required"),
        lambda = p_int(default = 20L, tags = "required"),
        crossover = p_lgl(default = TRUE, tags = "required"),  # only affects GGA
        mutation = p_lgl(default = TRUE, tags = "required"),   # only affects GGA
        random = p_lgl(default = FALSE, tags = "required"),    # affects both params and sIm
        detectors = p_lgl(default = TRUE, tags = "required")   # all detectors
      )
      param_set$values = list(select_id = "select.selector", interaction_id = "classif.xgboost.interaction_constraints", monotone_id = "classif.xgboost.monotone_constraints", crossover = TRUE, mutation = TRUE, random = FALSE, detectors = TRUE)
      super$initialize(
        param_set = param_set,
        param_classes = c("ParamDbl", "ParamFct", "ParamInt", "ParamLgl", "ParamUty"),
        properties = "multi-crit",
        packages = "iaml",
        label = "Joint HPO and Optimization of Feature Selection, Interaction Constraints and Monotonicity Constraints",
        man = "iaml::mlr_tuners_iaml_eax"
      )
    }
  ),

  private = list(
    .optimize = function(inst) {
      select_id = self$param_set$values$select_id
      interaction_id = self$param_set$values$interaction_id
      monotone_id = self$param_set$values$monotone_id
      mu = self$param_set$values$mu
      lambda = self$param_set$values$lambda
      # split param space from sIm space
      param_ids = setdiff(inst$search_space$ids(), c(select_id, interaction_id, monotone_id))
      param_space = ParamSet$new(inst$search_space$params[param_ids])
      param_space$trafo = inst$search_space$trafo
      param_space$deps = inst$search_space$deps
      # get ranges of numeric params for mutation
      ranges = map(names(which(param_space$is_number)), function(param_id_number) param_space$params[[param_id_number]]$upper - param_space$params[[param_id_number]]$lower)
      names(ranges) = names(which(param_space$is_number))

      task = inst$objective$task

      # initial population
      population = map_dtr(seq_len(mu), function(i) param_space$default)  # NOTE: for now, we use the initial design not random but defaults
      # we then mutate the initial design like during the GA but with p = 0.5 for each param
      for (j in seq_len(nrow(population))) {
        for(param_id in param_ids) {
          population[j, ][[param_id]] = mutate(population[j, ][[param_id]], param = param_space$params[[param_id]], range = ranges[[param_id]], p = 0.5)
        }
      }

      n_selected_prob = 1 / get_n_selected_rpart(task)
      n_selected = replicate(mu, sample_from_truncated_geom(n_selected_prob, lower = 1L, upper = length(task$feature_names)))  # number of selected features sampled from truncated geometric distribution
      # FIXME: detectors could and should be used outside
      if (self$param_set$values$detectors) {
        filter = FilterInformationGain$new()  # NOTE: can use any other Filter or use a custom FilterEnsemble
        scores = as.data.table(filter$calculate(task))  # filter scores are used to weight probabilities of inclusion in IAMLPointNEWX
        scores[, score := score / sum(score)]
        scores[score < .Machine$double.eps, score := .Machine$double.eps]
        interaction_detector = InteractionDetector$new(task)  # interaction detection
        interaction_detector$compute_best_rss()
        monotonicity_detector = MonotonicityDetector$new(task)  # monotonicity detection
        monotonicity_detector$compute_aics()
        monotonicity_detector$compute_unconstrained_weights()
        unconstrained_weight_table = monotonicity_detector$unconstrained_weight_table
        switch_sign_affected = monotonicity_detector$aic_table[aic_decreasing < aic_increasing][["feature_name"]]
        inst$objective$learner$param_set$values$colapply.affect_columns = selector_name(switch_sign_affected)
      } else {
        scores = NULL
        interaction_detector = NULL
        monotonicity_detector = NULL
        unconstrained_weight_table = NULL
      }

      sIm = map_dtr(seq_len(mu - 1), function(i) {  # sIm space
        iaml = IAMLPointNEWX$new(task, n_selected = n_selected[i], scores = scores, interaction_detector = interaction_detector, unconstrained_weight_table = unconstrained_weight_table, use_detectors = self$param_set$values$detectors)
        data.table(iaml = list(iaml),
                   s = list(iaml$create_selector()),
                   I = list(iaml$create_interaction_constraints()),
                   m = list(iaml$create_monotonicity_constraints()))
      })
      # add the unconstrained sIm point (to make sure we also have the most complex sIm)
      iaml_unconstrained = IAMLPointNEWX$new(task, n_selected = length(task$feature_names), scores = scores, interaction_detector = interaction_detector, unconstrained_weight_table = unconstrained_weight_table, unconstrained = TRUE, use_detectors = self$param_set$values$detectors)
      sIm_unconstrained = data.table(iaml = list(iaml_unconstrained),
                                     s = list(iaml_unconstrained$create_selector()),
                                     I = list(iaml_unconstrained$create_interaction_constraints()),
                                     m = list(iaml_unconstrained$create_monotonicity_constraints()))
      sIm = rbind(sIm, sIm_unconstrained)
      colnames(sIm) = c("iaml", select_id, interaction_id, monotone_id)

      population = cbind(population, sIm)
      gen = 0
      population[, generation := gen]
      population[, status := "alive"]

      # evaluate initial population
      # proxy measures for selected features, interactions and non monotone are evaluated here
      learner_for_measures = inst$objective$learner$clone(deep = TRUE)
      orig_pvs = learner_for_measures$param_set$values
      for (i in seq_len(nrow(population))) {
        inst$eval_batch(population[i, ])
        # NOTE: cannot use i but we can use inst$archive$n_batch (due to synchronous evaluation)
        j = inst$archive$n_batch

        # NOTE: this messes with logging (proxy measures are logged and the updated ones are not logged)
        # actually evaluate the proxy measures
        proxy_measures = calculate_proxy_measures(learner_for_measures, task = task, orig_pvs = orig_pvs, xdt = inst$archive$data[j, inst$archive$cols_x, with = FALSE], search_space = inst$search_space)
        inst$archive$data[j, iaml_selected_features_proxy := proxy_measures$n_selected]
        inst$archive$data[j, iaml_selected_interactions_proxy := proxy_measures$n_interactions]
        inst$archive$data[j, iaml_selected_non_monotone_proxy := proxy_measures$n_non_monotone]

        # update the iaml point (x space)
        iaml_point = inst$archive$data[j, ][["iaml"]][[1L]]$clone(deep = TRUE)
        iaml_point_orig = iaml_point$clone(deep = TRUE)
        inst$archive$data[j, iaml_orig := list(iaml_point_orig)]
        iaml_point = update_sIm(iaml_point, used = proxy_measures$used, belonging = proxy_measures$belonging)

        inst$archive$data[j, ][["iaml"]][[1L]] = iaml_point
        inst$archive$data[j, ][[select_id]][[1L]] = iaml_point$create_selector()
        inst$archive$data[j, ][[interaction_id]][[1L]] = iaml_point$create_interaction_constraints()
        inst$archive$data[j, ][[monotone_id]][[1L]] = iaml_point$create_monotonicity_constraints()
      }

      # iaml_points in the population with zero selection are killed
      zero_selected = map_lgl(inst$archive$data[[select_id]], function(selector) get_number_of_selected_features_from_selector(selector, task = task) == 0L)
      inst$archive$data[zero_selected, status := "dead"]

      repeat {  # iterate until we have an exception from eval_batch

        zero_selected = map_lgl(inst$archive$data[[select_id]], function(selector) get_number_of_selected_features_from_selector(selector, task = task) == 0L)
        stopifnot(all(inst$archive$data[zero_selected, ][["status"]] == "dead"))

        # new gen, new nadir point, new individuals that are still alive
        gen = gen + 1
        data = inst$archive$data[, inst$archive$cols_y, with = FALSE]
        stopifnot(colnames(data) == inst$objective$codomain$ids())
        ys = t(t(data) * mult_max_to_min(inst$objective$codomain))
        nadir = apply(ys, MARGIN = 2L, FUN = function(x) max(x) + 1)
        alive_ids = which(inst$archive$data$status == "alive")

        # create children
        # binary tournament selection of parents
        # FIXME: if alive ids is < 1 (due to zero selection being killed) simply generate children at random but this is unlikely to happen
        children = map_dtr(seq_len(ceiling(lambda / 2)), function(i) {
          if (self$param_set$values$random) {
            random_children = map_dtr(seq_len(2L), function(child) {
              x_child = generate_design_random(param_space, n = 1L)$data
              n_selected_child = sample_from_truncated_geom(n_selected_prob, lower = 1L, upper = length(task$feature_names))
              iaml_child = IAMLPointNEWX$new(task, n_selected = n_selected_child, scores = scores, interaction_detector = interaction_detector, unconstrained_weight_table = unconstrained_weight_table, use_detectors = self$param_set$values$detectors)
              sIm_child = data.table(iaml = list(iaml_child),
                                     s = list(iaml_child$create_selector()),
                                     I = list(iaml_child$create_interaction_constraints()),
                                     m = list(iaml_child$create_monotonicity_constraints()))
              colnames(sIm_child) = c("iaml", select_id, interaction_id, monotone_id)
              cbind(x_child, sIm_child)

            })
            return(random_children)
          }

          parent_id1 = binary_tournament(ys, alive_ids, nadir)
          parent_id2 = binary_tournament(ys, alive_ids, nadir)
          parents = transpose_list(copy(inst$archive$data[c(parent_id1, parent_id2), c("iaml", inst$archive$cols_x), with = FALSE]))

          # param_space
          # Uniform crossover for HPS; p could be individual for each HP
          p_param_space_cross = 0.2  # FIXME: HP of Tuner
          crossover_ps = runif(length(param_ids), min = 0, max = 1)
          param_ids_to_cross = param_ids[which(crossover_ps <= p_param_space_cross)]
          tmp = parents[[1L]]
          for (param_id in param_ids_to_cross) {
            parents[[1L]][[param_id]] = parents[[2L]][[param_id]]
            parents[[2L]][[param_id]] = tmp[[param_id]]
          }
          # Gaussian or uniform discrete mutation for HPs
          for (j in 1:2) {
            for(param_id in param_ids) {
              parents[[j]][[param_id]] = mutate(parents[[j]][[param_id]], param = param_space$params[[param_id]], range = ranges[[param_id]])
            }
          }

          # sIm space
          # crossover and mutation via GGA
          iaml1 = parents[[1L]][["iaml"]]$clone(deep = TRUE)
          iaml2 = parents[[2L]][["iaml"]]$clone(deep = TRUE)

          if (self$param_set$values$crossover) {
            crossing_sections = iaml1$get_crossing_sections(iaml2)
            tmp = iaml1$clone(deep = TRUE)
            iaml1$crossover(iaml2, crossing_sections = crossing_sections)
            iaml2$crossover(tmp, crossing_sections = rev(crossing_sections))
          }

          if (self$param_set$values$mutation) {
            iaml1$mutate()
            iaml2$mutate()
          }

          parents[[1L]][["iaml"]] = iaml1
          parents[[1L]][[select_id]] = iaml1$create_selector()
          parents[[1L]][[interaction_id]] = iaml1$create_interaction_constraints()
          parents[[1L]][[monotone_id]] = iaml1$create_monotonicity_constraints()

          parents[[2L]][["iaml"]] = iaml2
          parents[[2L]][[select_id]] = iaml2$create_selector()
          parents[[2L]][[interaction_id]] = iaml2$create_interaction_constraints()
          parents[[2L]][[monotone_id]] = iaml2$create_monotonicity_constraints()

          as.data.table(transpose_list(parents))
        })
        children[, generation := gen]
        children = children[seq_len(lambda), ]  # restrict to lambda children

        # evaluate children 
        for (i in seq_len(nrow(children))) {
          inst$eval_batch(children[i, ])
          # NOTE: cannot use i but we can use inst$archive$n_batch (due to synchronous evaluation)
          j = inst$archive$n_batch

          # actually evaluate the proxy measures
          # learner_for_measures and orig_pvs have been defined above (eval of initial pop)
          proxy_measures = calculate_proxy_measures(learner_for_measures, task = task, orig_pvs = orig_pvs, xdt = inst$archive$data[j, inst$archive$cols_x, with = FALSE], search_space = inst$search_space)
          inst$archive$data[j, iaml_selected_features_proxy := proxy_measures$n_selected]
          inst$archive$data[j, iaml_selected_interactions_proxy := proxy_measures$n_interactions]
          inst$archive$data[j, iaml_selected_non_monotone_proxy := proxy_measures$n_non_monotone]

          # update the iaml point (x space)
          iaml_point = inst$archive$data[j, ][["iaml"]][[1L]]$clone(deep = TRUE)
          iaml_point_orig = iaml_point$clone(deep = TRUE)
          inst$archive$data[j, iaml_orig := list(iaml_point_orig)]
          iaml_point = update_sIm(iaml_point, used = proxy_measures$used, belonging = proxy_measures$belonging)

          inst$archive$data[j, ][["iaml"]][[1L]] = iaml_point
          inst$archive$data[j, ][[select_id]][[1L]] = iaml_point$create_selector()
          inst$archive$data[j, ][[interaction_id]][[1L]] = iaml_point$create_interaction_constraints()
          inst$archive$data[j, ][[monotone_id]][[1L]] = iaml_point$create_monotonicity_constraints()
        }

        # NSGA-II stuff for survival
        all_ids = seq_len(nrow(inst$archive$data))
        # iaml_points with zero selection are not allowed to be selected
        zero_selected = map_lgl(inst$archive$data[[select_id]], function(selector) get_number_of_selected_features_from_selector(selector, task = task) == 0L)
        considered_ids = all_ids[!zero_selected]
        if (length(considered_ids) <= mu) {
          inst$archive$data[, status := "dead"]
          inst$archive$data[considered_ids, status := "alive"]
        } else {
          ys = t(t(inst$archive$data[!zero_selected, inst$archive$cols_y, with = FALSE]) * mult_max_to_min(inst$objective$codomain))
          alive_ids = emoa::nds_cd_selection(t(ys), n = mu)
          #rankings = emoa::nds_rank(t(ys))  # non-dominated fronts
          #cds = map_dtr(unique(rankings), function(ranking) {  # crowding distances
          #  ids = which(rankings == ranking)
          #  data.table(id = ids, cd = emoa::crowding_distance(t(ys[ids, , drop = FALSE])))
          #})
          #setorderv(cds, "id")
          #stopifnot(setequal(cds$id, seq_len(nrow(cds))))
          #alive_ids = integer(mu)
          #current_front = 0L
          #used_ids = integer()
          #while(sum(alive_ids == 0L) != 0) {
          #  current_front = current_front + 1L
          #  candidate_ids = which(rankings == current_front)
          #  to_insert = sum(alive_ids == 0L)
          #  if (length(candidate_ids) <= to_insert) {
          #    alive_ids[alive_ids == 0][seq_along(candidate_ids)] = candidate_ids
          #  } else {
          #    alive_ids[alive_ids == 0] = candidate_ids[order(cds[candidate_ids, ]$cd)][seq_len(to_insert)]
          #  }
          #}
          stopifnot(length(unique(alive_ids)) == length(alive_ids))
          inst$archive$data[, status := "dead"]
          inst$archive$data[considered_ids[alive_ids], status := "alive"]  # alive_ids was determined on the ys that did not carry the considered_ids info

        }
      }

      inst
    }
  )
)


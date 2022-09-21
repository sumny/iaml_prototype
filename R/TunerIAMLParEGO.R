#' @title Multi-objective Hyperparameter Optimization, Feature Selection and Interaction and Monotonicity Constraints
#'
#' @name mlr_tuner_iaml_parego
#'
#' @description
#' Performs joint multi-objective optimization of hyperparameters, feature selection and interaction and monotonicity
#' constraints of a suitable [mlr3::Learner].
#'
#' This requires an appropriate [mlr3::Learner], that allows for selecting features, and setting interaction and
#' monotonicity constraints, e.g., xgboost.
#'
#' @templateVar id iaml_parego
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
TunerIAMLParEGO = R6Class("TunerIAMLParEGO",
  inherit = mlr3tuning::Tuner,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        select_id = p_uty(tags = "required"),
        interaction_id = p_uty(tags = "required"),
        monotone_id = p_uty(tags = "required"),
        mu = p_int(default = 30L, tags = "required"),  # initial design
        s = p_int(lower = 1, default = 100L),
        rho = p_dbl(lower = 0, upper = 1, default = 0.05)
      )
      # FIXME: defaults
      param_set$values = list(select_id = "select.selector", interaction_id = "classif.xgboost.interaction_constraints", monotone_id = "classif.xgboost.monotone_constraints")
      super$initialize(
        param_set = param_set,
        param_classes = c("ParamDbl", "ParamFct", "ParamInt", "ParamLgl", "ParamUty"),
        properties = "multi-crit",
        packages = "iaml",
        label = "Joint HPO and Optimization of Feature Selection, Interaction Constraints and Monotonicity Constraints",
        man = "iaml::mlr_tuners_iaml_parego"
      )
    },

    surrogate = NULL
  ),

  private = list(
    .optimize = function(inst) {
      select_id = self$param_set$values$select_id
      interaction_id = self$param_set$values$interaction_id
      monotone_id = self$param_set$values$monotone_id
      mu = self$param_set$values$mu
      s = self$param_set$values$s
      rho = self$param_set$values$rho
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
      filter = FilterInformationGain$new()  # NOTE: can use any other Filter or use a custom FilterEnsemble
      scores = as.data.table(filter$calculate(task))  # filter scores are used to weight probabilities of inclusion in IAMLPointNEW
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

      sIm = map_dtr(seq_len(mu - 1), function(i) {  # sIm space
        iaml = IAMLPointNEW$new(task, n_selected = n_selected[i], scores = scores, interaction_detector = interaction_detector, unconstrained_weight_table = unconstrained_weight_table)
        data.table(iaml = list(iaml),
                   s = list(iaml$create_selector()),
                   I = list(iaml$create_interaction_constraints()),
                   m = list(iaml$create_monotonicity_constraints()))
      })
      # add the unconstrained sIm point (to make sure we also have the most complex sIm)
      iaml_unconstrained = IAMLPointNEW$new(task, n_selected = length(task$feature_names), scores = scores, interaction_detector = interaction_detector, unconstrained_weight_table = unconstrained_weight_table, unconstrained = TRUE)
      sIm_unconstrained = data.table(iaml = list(iaml_unconstrained),
                                     s = list(iaml_unconstrained$create_selector()),
                                     I = list(iaml_unconstrained$create_interaction_constraints()),
                                     m = list(iaml_unconstrained$create_monotonicity_constraints()))
      sIm = rbind(sIm, sIm_unconstrained)
      colnames(sIm) = c("iaml", select_id, interaction_id, monotone_id)

      population = cbind(population, sIm)

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

        # FIXME: get feature names from task and assure that order is always same, also see below children loop
        features = get_features_from_groups(iaml_point)
        feature_names = names(features)
        inst$archive$data[j, (feature_names) := features]
      }
      archive = inst$archive
      feature_params = setNames(map(feature_names, function(feature_name) ParamInt$new(feature_name)), nm = feature_names)
      domain = ParamSet$new(c(inst$search_space$params, ParamUty$new("iaml"), feature_params))
      domain$deps = param_space$deps
      # FIXME: trafo for acqf is hacky, we do not want to have the selector and constraints or iaml point
      domain$trafo = function(x, param_set) {
        x[[select_id]] = NULL
        x[[interaction_id]] = NULL
        x[[monotone_id]] = NULL
        x[["iaml"]] = NULL
        x
      }

      k = length(archive$cols_y)  # codomain can hold non targets since #08116aa02204980f87c8c08841176ae8f664980a
      surrogate = SurrogateLearner$new(lrn("regr.ranger", num.trees = 500L, keep.inbag = TRUE))
      acq_function = AcqFunctionEI$new()
      acq_optimizer = AcqOptimizer$new(OptimizerAcqGAGGA$new(select_id = select_id, archive = archive, param_ids = param_ids, param_space = param_space, ranges = ranges), terminator = trm("evals", n_evals = 1000L))
      surrogate$x_cols = c(param_space$ids(), feature_names)
      surrogate$y_cols = "y_scal"
      surrogate$archive = archive
      acq_function$surrogate = surrogate
      acq_optimizer$acq_function = acq_function
      acq_function$domain = domain

      lambdas = calculate_parego_weights(s, d = k)

      repeat {  # iterate until we have an exception from eval_batch

        data = archive$data
        ydt = data[, archive$cols_y, with = FALSE]
        # FIXME: use inplace operations
        ydt = Map("*", ydt, mult_max_to_min(archive$codomain))  # we always assume minimization
        ydt = Map(function(y) (y - min(y, na.rm = TRUE)) / diff(range(y, na.rm = TRUE)), ydt)  # scale y to [0, 1]

        xdt = {
          # scalarize y
          lambda = lambdas[sample.int(nrow(lambdas), 1L), , drop = TRUE]
          mult = Map("*", ydt, lambda)
          yscal = Reduce("+", mult)
          yscal = do.call(pmax, mult) + rho * yscal  # augmented Tchebycheff function
          data[, y_scal := yscal]  # need to name it yscal due to data.table's behavior

          tryCatch({
            acq_function$surrogate$update()
            acq_function$update()
            acq_optimizer$optimize()
          }, mbo_error = function(mbo_error_condition) {
            lg$info("Proposing a randomly sampled point")
            x_child = generate_design_random(param_space, n = 1L)$data
            n_selected_child = sample_from_truncated_geom(n_selected_prob, lower = 1L, upper = length(task$feature_names))
            iaml_child = IAMLPointNEW$new(task, n_selected = n_selected_child, scores = scores, interaction_detector = interaction_detector, unconstrained_weight_table = unconstrained_weight_table)
            sIm_child = data.table(iaml = list(iaml_child),
                                   s = list(iaml_child$create_selector()),
                                   I = list(iaml_child$create_interaction_constraints()),
                                   m = list(iaml_child$create_monotonicity_constraints()))
            colnames(sIm_child) = c("iaml", select_id, interaction_id, monotone_id)
            cbind(x_child, sIm_child)
          })
        }

        for (i in seq_len(nrow(xdt))) {  #FIXME: currently only single batch (q = 1)
          inst$eval_batch(xdt[i, ])
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
          
          # FIXME: get feature names from task and assure that order is always same, also see below children loop
          features = get_features_from_groups(iaml_point)
          feature_names = names(features)
          inst$archive$data[j, (feature_names) := features]
        }
      }
      self$surrogate = surrogate

      inst
    }
  )
)

# calculate all possible weights (lambdas) for given s parameter and dimensionality d taken von mlrMBO
calculate_parego_weights = function(s, d) {
  fun = function(s, d) {
    if (d == 1L)
      list(s)
    else
      unlist(lapply(0:s, function(i) Map(c, i, fun(s - i, d - 1L))), recursive = FALSE)
  }
  matrix(unlist(fun(s, d)), ncol = d, byrow = TRUE) / s
}

OptimizerAcqGAGGA = R6Class("OptimizerAcqGAGGA",
  inherit = Optimizer,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(archive, select_id, param_ids, param_space, ranges) {
      param_set = ps(children_batch_size = p_int(lower = 1L, default = 50L))
      self$archive = assert_r6(archive, classes = "Archive")
      self$select_id = assert_string(select_id)
      self$param_ids = assert_character(param_ids)
      self$param_space = assert_r6(param_space, classes = "ParamSet")
      self$ranges = assert_list(ranges)

      super$initialize(
        param_set = param_set,
        param_classes = c("ParamDbl", "ParamFct", "ParamInt", "ParamLgl", "ParamUty"),
        properties = c("single-crit")
      )
    },

    archive = NULL,
    select_id = NULL,
    param_ids = NULL,
    param_space = NULL,
    ranges = NULL
  ),

  private = list(
    # FIXME: cell_hash comparison is hacky
    .optimize = function(inst) {
      archive = self$archive
      select_id = self$select_id
      param_ids = self$param_ids
      param_space = self$param_space
      ranges = self$ranges
      children_batch_size = self$param_set$values$children_batch_size %??% 50L

      data_types = map_chr(archive$data, typeof)[c("iaml", archive$cols_x)]
      fix_data_types = names(data_types[data_types != "list"])

      zero_selected = map_lgl(archive$data[[select_id]], function(selector) get_number_of_selected_features_from_selector(selector, task = task) == 0L)

      data = archive$data[, archive$cols_y, with = FALSE]
      stopifnot(colnames(data) == archive$codomain$ids())
      ys = t(t(data) * mult_max_to_min(archive$codomain))
      nadir = apply(ys, MARGIN = 2L, FUN = function(x) max(x) + 1)
      alive_ids = which(!is_dominated(t(ys)) & !zero_selected)

      repeat {
        # do GAGGA: parent via tournament, crossover, mutate, then get iaml 
        children = map_dtr(seq_len(children_batch_size), function(i) {
          parent_id1 = binary_tournament(ys, alive_ids, nadir)
          parent_id2 = binary_tournament(ys, alive_ids, nadir)
          parents = transpose_list(copy(archive$data[c(parent_id1, parent_id2), c("iaml", archive$cols_x), with = FALSE]))

          # param_space
          # FIXME: ps changed from 0.2 to 0.5
          # Uniform crossover for HPS; p could be individual for each HP
          p_param_space_cross = 0.5  # FIXME: HP of Tuner
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
              parents[[j]][[param_id]] = mutate(parents[[j]][[param_id]], param = param_space$params[[param_id]], range = ranges[[param_id]], p = 0.5)
            }
          }

          # sIm space
          # crossover and mutation via GGA
          iaml1 = parents[[1L]][["iaml"]]$clone(deep = TRUE)
          iaml2 = parents[[2L]][["iaml"]]$clone(deep = TRUE)

          crossing_sections = iaml1$get_crossing_sections(iaml2)
          tmp = iaml1$clone(deep = TRUE)
          iaml1$crossover(iaml2, crossing_sections = crossing_sections)
          iaml2$crossover(tmp, crossing_sections = rev(crossing_sections))

          iaml1$mutate()
          iaml2$mutate()

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

        # FIXME: make sure that we only propose unqiue children and not stuff that has been already evaluated


        # FIXME: this is needed due to the transpose list stuff
        # also put this in all other optimizers
        children[, (fix_data_types) := lapply(.SD, unlist), .SDcols = fix_data_types]

        features = map_dtr(children$iaml, get_features_from_groups)
        children = cbind(children, features)

        # drop duplicated children or children already present in the archive
        duplicated_ids = which(duplicated(children[, c(param_ids, colnames(features)), with = FALSE]))
        archive_data = copy(archive$data[, c(param_ids, colnames(features)), with = FALSE])
        children_data = copy(children[, c(param_ids, colnames(features)), with = FALSE])
        duplicated_ids = which(duplicated(children[, c(param_ids, colnames(features)), with = FALSE]))
        children_data[, overlap := FALSE][archive_data, overlap := TRUE, on = c(param_ids, colnames(features))]
        already_evaluated_ids = which(children_data$overlap)
        already_evaluated_acq_ids = if (nrow(inst$archive$data) > 0L) {
          archive_acq_data = copy(inst$archive$data[, c(param_ids, colnames(features)), with = FALSE])
          children_data[, overlap := FALSE][archive_acq_data, overlap := TRUE, on = c(param_ids, colnames(features))]
          which(children_data$overlap)
        } else {
          integer()
        }
        drop_ids = unique(c(duplicated_ids, already_evaluated_ids, already_evaluated_acq_ids))
        if (length(drop_ids)) {
          children = children[- drop_ids, ]
        }
        inst$eval_batch(children)
      }
    }
  )
)

get_features_from_groups = function(iaml_point) {
  features = iaml_point$groups$features
  value = iaml_point$groups$belonging - 1L  # start from 0
  monotonicity = iaml_point$monotone_features$monotonicity
  monotonicity[monotonicity == 1L] = -1L  # constrained get -1L
  monotonicity[is.na(monotonicity) | monotonicity == 0L] = 1L  # unconstrained gets 1L
  value = value * monotonicity
  names(value) = features
  as.list(value)
}

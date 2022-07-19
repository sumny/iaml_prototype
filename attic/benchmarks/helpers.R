# for the outer resampling (holdout):
#   1. construct the train task (optim set based on outer resampling) and test task (test set based on outer resampling) and initialize the inner resampling (fixed seed) (train and valid set based on optim set); see above
#   2. perform tuning using the inner resampling on the train task and obtain pareto set on valid set of train task
#   3. refit pareto set on whole train task and evaluate on test task (based on outer resampling)
#   4. calculate the dominated hypervolume based on the valid set --> dhv_valid
#   5. calculate the dominated hypervolume based on the test set --> dhv_test
nested_resampling_gagga = function(task_train, task_test, resampling_inner, n_evals = 430L, secs = 5L * 24L * 3600L) {
  reference_point = c(classif.ce = 1, selected_features = 1, selected_interactions = 1, selected_non_monotone = 1)
  mu = 30L
  lambda = 20L

  learner = as_learner(po("colapply") %>>% po("select") %>>% po("sortfeatures") %>>% lrn("classif.xgboost"))
  learner$param_set$values$classif.xgboost.booster = "gbtree"
  learner$param_set$values$classif.xgboost.tree_method = "exact"
  learner$param_set$values$colapply.applicator = function(x) - x

  measures = list(msr("classif.ce"),
                  msr("iaml_selected_features_proxy"),
                  msr("iaml_selected_interactions_proxy"),
                  msr("iaml_selected_non_monotone_proxy"))

  terminator = trm("combo", list(trm("evals", n_evals = n_evals), trm("run_time", secs = secs)))

  search_space = get_xgboost_search_space_eawm()

  instance = TuningInstanceMultiCrit$new(
    task = task_train,
    learner = learner,
    resampling = resampling_inner, 
    measures = measures,
    terminator = terminator,
    search_space = search_space,
    store_models = TRUE
  )

  tuner = tnr("iaml_ea_new", mu = mu, lambda = lambda)
  tuner$optimize(instance)

  tuning_data = copy(instance$archive$data)
  tuning_pareto = copy(instance$archive$best())

  pareto = copy(tuning_pareto)
  learner_on_test = instance$objective$learner$clone(deep = TRUE)
  orig_pvs = instance$objective$learner$param_set$values
  for (p in seq_len(NROW(pareto))) {
    iaml_point = pareto[p, ][["iaml_orig"]][[1L]]
    xdt = copy(pareto[p, ])
    xdt[["iaml"]][[1L]] = iaml_point
    xdt[[tuner$param_set$values$select_id]][[1L]] = iaml_point$create_selector()
    xdt[[tuner$param_set$values$interaction_id]][[1L]] = iaml_point$create_interaction_constraints()
    xdt[[tuner$param_set$values$monotone_id]][[1L]] = iaml_point$create_monotonicity_constraints()
    xss = transform_xdt_to_xss(xdt, search_space = instance$search_space)[[1L]]
    xss = insert_named(orig_pvs, xss)
    learner_on_test$param_set$values = xss
    learner_on_test$train(task_train)
    pareto[p, ce_test := learner_on_test$predict(task_test)$score(msr("classif.ce"))]
    # proxy measures must not be updated because they were already determined on the task_train during tuning
  }

  # validation dominated hypervolume
  y_val = tuning_pareto[, c("classif.ce", "iaml_selected_features_proxy", "iaml_selected_interactions_proxy", "iaml_selected_non_monotone_proxy")]
  dhv_val = emoa::dominated_hypervolume(t(y_val), ref = reference_point)

  # test dominated hypervolume
  y_test = pareto[, c("ce_test", "iaml_selected_features_proxy", "iaml_selected_interactions_proxy", "iaml_selected_non_monotone_proxy")]
  colnames(y_test) = c("classif.ce", "selected_features", "selected_interactions", "selected_non_monotone")
  dhv_test = emoa::dominated_hypervolume(t(y_test), ref = reference_point)

  # validation dominated hypervolume anytime
  dhv_val_anytime = map_dtr(seq_len(NROW(tuning_data)), function(p) {
    start = tuning_data[1L, ]$timestamp
    stop = tuning_data[p, ]$timestamp
    non_dominated = !is_dominated(t(tuning_data[seq_len(p), c("classif.ce", "iaml_selected_features_proxy", "iaml_selected_interactions_proxy", "iaml_selected_non_monotone_proxy")]))
    y_val = tuning_data[which(non_dominated), c("classif.ce", "iaml_selected_features_proxy", "iaml_selected_interactions_proxy", "iaml_selected_non_monotone_proxy")]
    dhv_val = emoa::dominated_hypervolume(t(y_val), ref = reference_point)
    data.table(iteration = p, dhv_val = dhv_val, runtime = as.numeric(stop - start, units = "secs"))
  })

  data.table(tuning_data = list(tuning_data), tuning_pareto = list(tuning_pareto), pareto = list(pareto), dhv_val = dhv_val, dhv_test = dhv_test, dhv_val_anytime = list(dhv_val_anytime), best_val = min(tuning_pareto$classif.ce), best_test = min(pareto$ce_test))
}

# FIXME: xgboost ParEGO MO custom; RS MP custom
# bbotk: ce on val + resampling_inner, proxy measures on whole val
nested_resampling_xgboost = function(task_train, task_test, resampling_inner, n_evals = 430L, secs = 5L * 24L * 3600L) {
  learner = as_learner(po("sortfeatures") %>>% lrn("classif.xgboost"))
  learner$param_set$values$classif.xgboost.booster = "gbtree"
  learner$param_set$values$classif.xgboost.tree_method = "exact"

  measure = msr("classif.ce")

  terminator = trm("combo", list(trm("evals", n_evals = n_evals), trm("run_time", secs = secs)))

  search_space = get_xgboost_search_space()

  instance = TuningInstanceSingleCrit$new(
    task = task_train,
    learner = learner,
    resampling = resampling_inner, 
    measure = measure,
    terminator = terminator,
    search_space = search_space,
    store_models = TRUE
  )

  surrogate = SurrogateLearner$new(lrn("regr.ranger", num.trees = 500L, keep.inbag = TRUE))
  acq_function = AcqFunctionEI$new()
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 10000L), terminator = trm("evals", n_evals = 10000L))
  tuner = tnr("mbo", surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer)
  tuner$optimize(instance)

  tuning_data = copy(instance$archive$data)
  learner_on_train = instance$objective$learner$clone(deep = TRUE)
  orig_pvs = instance$objective$learner$param_set$values

  best = instance$archive$best()
  learner_on_train$param_set$values = insert_named(orig_pvs, transform_xdt_to_xss(best, search_space = instance$search_space)[[1L]])

  learner_on_train$train(task_train)
  ce = learner_on_train$predict(task_test)$score(measure)

  model = learner_on_train$model$classif.xgboost$model
  features = model$feature_names  # pre-selected based on selector
  stopifnot(all(features == sort(features)))  # if internal xgboost feature representation does not match the alphabetically ordered one something is really messed up
  n_selected_total = length(task_train$feature_names)  # all
  tmp = tryCatch(xgb_model_dt_tree(features, model = model), error = function(ec) {
    NULL
  })
  used = if (is.null(tmp)) {
    features
  } else {
    sort(unique(tmp$Feature[tmp$Feature != "Leaf"]))  # alphabetical order
  }
  n_selected = length(used)
  n_selected = n_selected / n_selected_total  # normalize

  n_interactions_total = (n_selected_total * (n_selected_total - 1L)) / 2L
  pairs = tryCatch(interactions(model, option = "pairs"), error = function(ec) {
    NULL
  })
  if (is.null(pairs)) {
    n_interactions = n_interactions_total
    belonging = rep(0L, length(task_train$feature_names))
    names(belonging) = sort(task_train$feature_names)
    belonging[match(names(used), names(belonging))] = 1L
    belonging = belonging + 1L
  } else {
    tmp = get_actual_interactions(used, pairs)
    n_interactions = tmp$n_interactions
    belonging = rep(0L, length(task_train$feature_names))
    names(belonging) = sort(task_train$feature_names)
    belonging[match(names(tmp$belonging), names(belonging))] = tmp$belonging
    belonging = belonging + 1L
  }
  n_interactions = n_interactions / n_interactions_total
  if (n_interactions_total == 0) {
    n_interactions = 0L
  }

  n_non_monotone_total = n_selected_total
  n_non_monotone = n_selected_total
  n_non_monotone = n_non_monotone / n_non_monotone_total

  data.table(tuning_data = list(tuning_data), best = list(best), ce.test = ce, iaml_selected_features_proxy = n_selected, iaml_selected_interactions_proxy = n_interactions, iaml_selected_non_monotone_proxy = n_non_monotone)
}

nested_resampling_ebm = function(task_train, task_test, resampling_inner, n_evals = 430L, secs = 5L * 24L * 3600L) {
  n_features = length(task_train$col_roles$feature)

  learner = lrn("classif.ebm")

  measure = msr("classif.ce")

  terminator = trm("combo", list(trm("evals", n_evals = n_evals), trm("run_time", secs = secs)))

  search_space = get_ebm_search_space(n_features)

  instance = TuningInstanceSingleCrit$new(
    task = task_train,
    learner = learner,
    resampling = resampling_inner, 
    measure = measure,
    terminator = terminator,
    search_space = search_space,
    store_models = TRUE
  )

  # we generate the design as in mbo but also include EBM's defaults
  design = generate_design_random(instance$search_space, n = 4L * length(instance$search_space$params))$data
  design[1L, ] = as.data.table(instance$search_space$default)
  instance$eval_batch(design)

  surrogate = SurrogateLearner$new(lrn("regr.ranger", num.trees = 500L, keep.inbag = TRUE))
  acq_function = AcqFunctionEI$new()
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 10000L), terminator = trm("evals", n_evals = 10000L))
  tuner = tnr("mbo", surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer)
  tuner$optimize(instance)

  tuning_data = copy(instance$archive$data)
  learner_on_train = instance$objective$learner$clone(deep = TRUE)
  orig_pvs = instance$objective$learner$param_set$values

  best = instance$archive$best()
  learner_on_train$param_set$values = insert_named(orig_pvs, transform_xdt_to_xss(best, search_space = instance$search_space)[[1L]])

  learner_on_train$train(task_train)
  ce = learner_on_train$predict(task_test)$score(measure)

  data.table(tuning_data = list(tuning_data), best = list(best), ce.test = ce, iaml_selected_features_proxy = n_features, iaml_selected_interactions_proxy = min(c(best$interactions, n_features * (n_features - 1L) / 2L)), iaml_selected_non_monotone_proxy = n_features)
}

nested_resampling_glmnet = function(task_train, task_test, resampling_inner, n_evals = 430L, secs = 5L * 24L * 3600L) {
  learner = lrn("classif.glmnet")

  measure = msr("classif.ce")

  terminator = trm("combo", list(trm("evals", n_evals = n_evals), trm("run_time", secs = secs)))

  search_space = get_glmnet_search_space()

  instance = TuningInstanceSingleCrit$new(
    task = task_train,
    learner = learner,
    resampling = resampling_inner, 
    measure = measure,
    terminator = terminator,
    search_space = search_space,
    store_models = TRUE
  )

  surrogate = SurrogateLearner$new(lrn("regr.ranger", num.trees = 500L, keep.inbag = TRUE))
  acq_function = AcqFunctionEI$new()
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 10000L), terminator = trm("evals", n_evals = 10000L))
  tuner = tnr("mbo", surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer)
  tuner$optimize(instance)

  tuning_data = copy(instance$archive$data)
  learner_on_train = instance$objective$learner$clone(deep = TRUE)
  orig_pvs = instance$objective$learner$param_set$values

  best = instance$archive$best()
  xss = transform_xdt_to_xss(best, search_space = instance$search_space)[[1L]]
  learner_on_train$param_set$values = insert_named(orig_pvs, xss)

  learner_on_train$train(task_train)
  xss$lambda = mlr3learners:::glmnet_get_lambda(learner_on_train, pv = xss)
  learner_on_train$param_set$values = insert_named(orig_pvs, xss)
  learner_on_train$train(task_train)
  ce = learner_on_train$predict(task_test)$score(measure)

  data.table(tuning_data = list(tuning_data), best = list(best), ce.test = ce, iaml_selected_features_proxy = get_n_selected_glmnet(learner_on_train, task = task_train), iaml_selected_interactions_proxy = 0L, iaml_selected_non_monotone_proxy = 0L)
}

random_forest = function(task_train, task_test, ...) {
  learner = as_learner(po("sortfeatures") %>>% lrn("classif.xgboost"))
  xss = list(classif.xgboost.booster = "gbtree", classif.xgboost.tree_method = "exact", classif.xgboost.subsample = 0.632, classif.xgboost.colsample_bynode = 0.632, classif.xgboost.num_parallel_tree = 500L, classif.xgboost.nrounds = 1L, classif.xgboost.eta = 1)

  measure = msr("classif.ce")

  learner$train(task_train)
  ce = learner$predict(task_test)$score(measure)

  model = learner$model$classif.xgboost$model
  features = model$feature_names  # pre-selected based on selector
  stopifnot(all(features == sort(features)))  # if internal xgboost feature representation does not match the alphabetically ordered one something is really messed up
  n_selected_total = length(task_train$feature_names)  # all
  tmp = tryCatch(xgb_model_dt_tree(features, model = model), error = function(ec) {
    NULL
  })
  used = if (is.null(tmp)) {
    features
  } else {
    sort(unique(tmp$Feature[tmp$Feature != "Leaf"]))  # alphabetical order
  }
  n_selected = length(used)
  n_selected = n_selected / n_selected_total  # normalize

  n_interactions_total = (n_selected_total * (n_selected_total - 1L)) / 2L
  pairs = tryCatch(interactions(model, option = "pairs"), error = function(ec) {
    NULL
  })
  if (is.null(pairs)) {
    n_interactions = n_interactions_total
    belonging = rep(0L, length(task_train$feature_names))
    names(belonging) = sort(task_train$feature_names)
    belonging[match(names(used), names(belonging))] = 1L
    belonging = belonging + 1L
  } else {
    tmp = get_actual_interactions(used, pairs)
    n_interactions = tmp$n_interactions
    belonging = rep(0L, length(task_train$feature_names))
    names(belonging) = sort(task_train$feature_names)
    belonging[match(names(tmp$belonging), names(belonging))] = tmp$belonging
    belonging = belonging + 1L
  }
  n_interactions = n_interactions / n_interactions_total
  if (n_interactions_total == 0) {
    n_interactions = 0L
  }

  n_non_monotone_total = n_selected_total
  n_non_monotone = n_selected_total
  n_non_monotone = n_non_monotone / n_non_monotone_total

  data.table(tuning_data = NULL, best = NULL, ce.test = ce, iaml_selected_features_proxy = n_selected, iaml_selected_interactions_proxy = n_interactions, iaml_selected_non_monotone_proxy = n_non_monotone)
}

get_n_selected_glmnet = function(learner, task, normalize = TRUE) {
  features = task$feature_names
  n_selected_total = length(features)
  n_selected = learner$model$df
  stopifnot(length(n_selected) == 1L)
  if (normalize) {
    n_selected = n_selected / n_selected_total
  }
  n_selected
}



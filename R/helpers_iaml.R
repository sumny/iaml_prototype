# function to get the eqcs
get_eqcs = function(x) {
  # x is output like sample_random
  map(x$eqcs, function(eqc) {
    x$features[x$belonging == eqc]
  })
}

# function to get the interaction matrix based on eqcs
get_matrix = function(x) {
  # x is output like get_eqcs
  features = unique(unlist(x))
  n_features = length(features)
  I = diag(1, nrow = n_features, ncol = n_features)
  rownames(I) = colnames(I) = features
  for (i in seq_along(x)) {
    indices = match(x[[i]], features)
    # if eqc only has one member do nothing
    if (length(indices) >= 2L) {
      interactions = utils::combn(indices, m = 2L)
      I[t(interactions)] = 1
    }
  }
  I[lower.tri(I)] = t(I)[lower.tri(I)]
  I
}

# sampling features to eqcs
sample_interactions_random = function(features) {
  n_eqcs = sample(seq_along(features), size = 1L)
  belonging = sample(seq_len(n_eqcs), size = length(features), replace = TRUE)
  belonging = match(belonging, sort(unique(belonging)))  # ordering without tie breaks
  list(features = features, eqcs = unique(belonging), belonging = belonging)
}

# sampling monotonicity
sample_m = function(I) {
  m = integer(nrow(I$I))
  n_classes = length(I$classes)
  m_sample = sample(c(-1L, 0L, 1L), size = n_classes, replace = TRUE)
  for (i in seq_len(n_classes)) {
    m[I$classes[[i]]] = m_sample[i]
  }
  m
}

mult_max_to_min = function(codomain) {
  ifelse(map_lgl(codomain$tags, has_element, "minimize"), 1, -1)
}

# actual features used by a xgboost model (trained on the whole tuning task)
calculate_proxy_measures = function(learner, task, orig_pvs, xdt, search_space, xgb_model_name = "classif.xgboost", monotone_id = "classif.xgboost.monotone_constraints") {
  assert_learner(learner)
  assert_task(task)
  assert_list(orig_pvs)
  assert_data_table(xdt)
  assert_r6(search_space, classes = "ParamSet")
  assert_string(xgb_model_name)
  pvs = transform_xdt_to_xss(xdt, search_space = search_space)[[1L]]
  learner$param_set$values = insert_named(orig_pvs, pvs)
  learner$train(task)
  learner$param_set$values = orig_pvs  # reset to orig pvs

  features = learner$model[[xgb_model_name]]$model$feature_names  # pre-selected based on selector
  n_selected_total = length(task$feature_names)  # all
  tmp = tryCatch(xgb_model_dt_tree(features, learner$model[[xgb_model_name]]$model),
    error = function(ec) {
      data.table()
    })
  used = sort(unique(tmp$Feature[tmp$Feature != "Leaf"]))  # alphabetical order
  n_selected = length(used)
  n_selected  = n_selected / n_selected_total  # normalize

  n_interactions_total = (n_selected_total * (n_selected_total - 1L)) / 2L
  pairs = tryCatch(interactions(learner$model[[xgb_model_name]]$model, option = "pairs"), error = function(ec) {
    data.table()
  })
  tmp = get_actual_interactions(features, pairs)
  n_interactions = tmp$n_interactions
  n_interactions = n_interactions / n_interactions_total  # FIXME: do we scale by all or only all based on actual selected features
  if (n_interactions_total == 0) {
    n_interactions = 0
  }

  n_non_monotone_total = n_selected_total
  constraints = xdt[[monotone_id]][[1L]]
  n_non_monotone = sum(constraints[names(constraints) %in% used] == 0)
  n_non_monotone = n_non_monotone / n_non_monotone_total
  list(n_selected = n_selected, n_interactions = n_interactions, n_non_monotone = n_non_monotone, used = used, belonging = tmp$belonging)
}

update_sIm = function(iaml_point, used, belonging) {
  assert_subset(used, iaml_point$feature_names)
  old_groups = iaml_point$groups
  old_monotone_features = iaml_point$monotone_features
  old_monotone_eqcs = iaml_point$monotone_eqcs
  new_belonging = rep(0L, length(old_groups$features))  # start all unselected
  names(new_belonging) = old_groups$features
  new_belonging[old_groups$features[old_groups$features %in% names(belonging)]] = belonging[old_groups$features[old_groups$features %in% names(belonging)]]  # copy belonging from new
  new_belonging = unname(new_belonging + 1L)  # fix groups to start at 1
  new_eqcs = unique(new_belonging)
  new_groups = list(features = old_groups$features, eqcs = unique(c(1L, sort(new_eqcs))), belonging = new_belonging)

  # monotonicity of eqcs
  new_monotone_eqcs = map_dtr(new_groups$eqcs, function(eqc) {
    if (eqc == 1L) {
      return(data.table(eqcs = eqc, monotonicity = NA))
    }
    members = new_groups$features[new_groups$belonging == eqc]
    monotonicity = unique(old_monotone_features[match(members, feature), ][["monotonicity"]])
    stopifnot(length(monotonicity) == 1L)
    data.table(eqcs = eqc, monotonicity = monotonicity)
  })

  # update
  iaml_point$groups = new_groups
  iaml_point$monotone_eqcs = new_monotone_eqcs

  # FIXME: safety check features that are still used must have same monotonicity after updating
  
  iaml_point
}

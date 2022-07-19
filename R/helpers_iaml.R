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

# adapt sign of codomain
mult_max_to_min = function(codomain) {
  ifelse(map_lgl(codomain$tags, has_element, "minimize"), 1, -1)
}

# function for getting the actual interaction groups
get_actual_interactions = function(features, pairs) {
  I = matrix(0L, nrow = length(features), ncol = length(features))
  colnames(I) = rownames(I) = features
  present = unique(c(pairs$Parent, pairs$Child))
  stopifnot(all(present %in% features))

  if (length(features) == 0L) {
    return(list(n_interactions = 0, I = I, belonging = integer()))
  }

  for (f1 in present) {
    tmp = pairs[Parent == f1 | Child == f1]
    interactors = unique(c(tmp$Parent, tmp$Child))
    i = match(f1, features)
    js = match(interactors, features)
    I[i, js] = 1L
    I[js, i] = 1L
  }
  diag(I) = 1L
  rel = transitive_closure(relation(list(features, features), incidence = I))
  I = relation_incidence(rel)[features, features, drop = FALSE]
  belonging = get_class_ids_from_incidence(I)
  # NOTE: I must not be transitive; but we do consider the transitive closure to respect the cost of higher order interactions
  list(n_interactions = sum(I[upper.tri(I)]), I = I, belonging = belonging)
}

# actual features used by a xgboost model (trained on the whole tuning task)
calculate_proxy_measures = function(learner, task, orig_pvs, xdt, search_space, xgb_model_name = "classif.xgboost", monotone_id = "classif.xgboost.monotone_constraints") {
  assert_learner(learner)
  assert_task(task)
  assert_list(orig_pvs)
  assert_data_table(xdt)
  assert_r6(search_space, classes = "ParamSet")
  assert_string(xgb_model_name, null.ok = TRUE)
  assert_string(monotone_id, null.ok = TRUE)
  pvs = transform_xdt_to_xss(xdt, search_space = search_space)[[1L]]
  learner$param_set$values = insert_named(orig_pvs, pvs)
  learner$train(task)
  learner$param_set$values = orig_pvs  # reset to orig pvs

  model = if (is.null(xgb_model_name)) learner$model else learner$model[[xgb_model_name]]$model
  features = model$feature_names  # pre-selected based on selector
  stopifnot(all(features == sort(features)))  # if internal xgboost feature representation does not match the alphabetically ordered one something is really messed up
  n_selected_total = length(task$feature_names)  # all
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
    belonging = rep(0L, length(task$feature_names))
    names(belonging) = sort(task$feature_names)
    belonging[match(names(used), names(belonging))] = 1L
    belonging = belonging + 1L
  } else {
    tmp = get_actual_interactions(used, pairs)
    n_interactions = tmp$n_interactions
    belonging = rep(0L, length(task$feature_names))
    names(belonging) = sort(task$feature_names)
    belonging[match(names(tmp$belonging), names(belonging))] = tmp$belonging
    belonging = belonging + 1L
  }
  n_interactions = n_interactions / n_interactions_total
  if (n_interactions_total == 0) {
    n_interactions = 0L
  }

  n_non_monotone_total = n_selected_total
  if (is.null(monotone_id)) {
    n_non_monotone = n_selected_total
  } else {
    constraints = xdt[[monotone_id]][[1L]]
    n_non_monotone = sum(constraints[names(constraints) %in% used] == 0)
  }
  n_non_monotone = n_non_monotone / n_non_monotone_total
  list(n_selected = n_selected, n_interactions = n_interactions, n_non_monotone = n_non_monotone, used = used, belonging = belonging)
}

# check that the structure of original groups and update groups is sensible
check_actual_and_orig = function(orig_groups, actual_groups) {
  orig = get_eqcs(orig_groups)
  actual = get_eqcs(actual_groups)
  stopifnot(all(orig[[1L]] %in% actual[[1L]]))
  for (eqc in setdiff(seq_along(actual), 1L)) {
    reference_feature = actual[[eqc]][[1L]]
    matching_eqc = which(map_lgl(orig, function(groups) reference_feature %in% groups))
    stopifnot(length(matching_eqc) == 1L)
    stopifnot(all(actual[[eqc]] %in% orig[[matching_eqc]]))
  }
}

# FIXME: should be method of the class
# update a group structure via the actually used features and their groups
update_sIm = function(iaml_point, used, belonging) {
  assert_subset(used, iaml_point$feature_names)
  old_groups = iaml_point$groups
  stopifnot(all(old_groups$belonging[match(used, old_groups$features)] != 1L))  # actual used must be subset of allowed to used
  old_monotone_features = iaml_point$monotone_features
  old_monotone_eqcs = iaml_point$monotone_eqcs
  new_belonging = rep(1L, length(old_groups$features))  # init all as unselected
  names(new_belonging) = old_groups$features
  new_belonging[old_groups$features[old_groups$features %in% names(belonging)]] = belonging[old_groups$features[old_groups$features %in% names(belonging)]]  # copy belonging from new
  new_belonging = match(new_belonging, sort(unique(c(1, new_belonging))))  # make eqcs start at 1
  new_belonging = unname(new_belonging)
  new_eqcs = unique(new_belonging)
  new_groups = list(features = old_groups$features, eqcs = unique(c(1L, sort(new_eqcs))), belonging = new_belonging)

  check_actual_and_orig(old_groups, actual_groups = new_groups)

  # monotonicity of eqcs
  new_monotone_eqcs = map_dtr(new_groups$eqcs, function(eqc) {
    if (eqc == 1L) {
      return(data.table(eqcs = eqc, monotonicity = NA_integer_))
    }
    members = new_groups$features[new_groups$belonging == eqc]
    monotonicity = unique(old_monotone_features[match(members, feature), ][["monotonicity"]])
    stopifnot(length(monotonicity) == 1L)
    data.table(eqcs = eqc, monotonicity = monotonicity)
  })

  # update, allow for all unselected
  iaml_point$allow_all_unselected = TRUE
  iaml_point$groups = new_groups
  iaml_point$monotone_eqcs = new_monotone_eqcs
  iaml_point$allow_all_unselected = FALSE

  iaml_point
}

# function to get the number of features selected by a selector
get_number_of_selected_features_from_selector = function(selector, task) {
  tmp = selector(task)
  length(tmp)
}

# function to get eqc membership of elements of the domain of an equivalence relation
get_class_ids_from_incidence = function(x) {
  y = integer(nrow(x))
  c = 1L
  pos = seq_along(y)
  while (length(pos)) {
    ind = x[pos[1L], pos] == 1
    y[pos[ind]] = c
    pos = pos[!ind]
    c = c + 1L
  }
  names(y) = rownames(x)
  y
}

# function to get the relative number of features used in a decision tree
get_n_selected_rpart = function(task, repls = 100L) {
  assert_task(task)
  learner = if (task$task_type == "classif") {
    as_learner(po("subsample") %>>% lrn("classif.rpart"))
  } else if (task$task_type == "regr") {
    as_learner(po("subsample") %>>% lrn("regr.rpart"))
  }
  n_selected_repls = map_int(seq_len(repls), function(repl) {
    learner$train(task)
    pairs = get_parent_child_pairs_rpart(learner$model[[2L]]$model)
    selected = unique(c(pairs$Child, pairs$Parent))
    stopifnot(all(selected %in% task$feature_names))
    n_selected = length(selected)
    n_selected
  })
  max(1, mean(n_selected_repls))
}

# function to get the relative number of pairwise interactions used in a decision tree
get_n_interactions_rpart = function(task, repls = 100L) {
  assert_task(task)
  features = sort(task$feature_names)
  n_features = length(features)
  learner = if (task$task_type == "classif") {
    as_learner(po("subsample") %>>% lrn("classif.rpart"))
  } else if (task$task_type == "regr") {
    as_learner(po("subsample") %>>% lrn("regr.rpart"))
  }
  n_interactions_repls = map_int(seq_len(repls), function(repl) {
    learner$train(task)
    pairs = get_parent_child_pairs_rpart(learner$model[[2L]]$model)
    I = matrix(0L, nrow = n_features, ncol = n_features)
    colnames(I) = rownames(I) = features
    present = unique(c(pairs$Parent, pairs$Child))
    stopifnot(all(present %in% features))

    if (length(present) == 0L) {
      return(0L)
    }

    for (f1 in present) {
      tmp = pairs[Parent == f1 | Child == f1]
      interactors = unique(c(tmp$Parent, tmp$Child))
      i = match(f1, features)
      js = match(interactors, features)
      I[i, js] = 1L
      I[js, i] = 1L
    }
    diag(I) = 1L
    rel = transitive_closure(relation(list(features, features), incidence = I))
    I = relation_incidence(rel)[features, features, drop = FALSE]
    # NOTE: I must not be transitive; but we do consider the transitive closure to respect the cost of higher order interactions
    n_interactions = as.integer(sum(I[upper.tri(I)]))
    n_interactions
  })
  max(1, mean(n_interactions_repls))
}

# function to get parents and childs in an rpart model
get_parent_child_pairs_rpart = function(model) {
  assert_class(model, classes = "rpart")
  if (nrow(model$cptable) == 1L & model$cptable[1, "nsplit"] == 0L) {  # scenario of no split
    return(data.table(Child = character(), Parent = character()))
  }
  varnodes = subset(model$frame, var != "<leaf>", select="var")
  varnodes$var = as.character(varnodes$var)
  cp = Map(function(a, b) { varnodes$var[rownames(varnodes) %in% c(2 * b, 2 * b + 1)]}, varnodes$var, as.numeric(rownames(varnodes)))
  tmp = tryCatch(setNames(as.data.table(stack(Filter(length, cp))), c("Child", "Parent")), error = function(ec) data.table(Child = character(), Parent = character()))
  tmp[, Child := as.character(Child)]
  tmp[, Parent := as.character(Parent)]
  tmp
}

# function to sample form a truncated geometric distribution between lower and upper
sample_from_truncated_geom = function(p, lower, upper) {
  assert_number(p, lower = 0, upper = 1)
  if (p > 0.99) p = 0.99  # we forbid p = 1 to not be stuck in an infinite loop, i.e., for n_selected = 0 or n_interactions = 0
  assert_integerish(lower, lower = 1L, upper = Inf)
  assert_integerish(upper, lower = 1L, upper = Inf)
  z = rgeom(1L, prob = p)
  while(z < lower | z > upper) {
    z = rgeom(1L, prob = p)
  }
  z
}

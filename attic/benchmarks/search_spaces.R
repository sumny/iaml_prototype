get_xgboost_search_space_gagga = function() {
  ps(
    classif.xgboost.nrounds = p_dbl(lower = log(1), upper = log(5000), tags = c("int", "log"),
                                    trafo = function(x) as.integer(round(exp(x))), default = log(500)),
    classif.xgboost.eta = p_dbl(lower = log(1e-4), upper = 0, tags = "log",
                                trafo = function(x) exp(x), default = log(0.3)),
    classif.xgboost.gamma = p_dbl(lower = log(1e-4), upper = log(7), tags = "log",
                                  trafo = function(x) exp(x), default = log(1e-4)),
    classif.xgboost.lambda = p_dbl(lower = log(1e-4), upper = log(1000), tags = "log",
                                   trafo = function(x) exp(x), default = log(1)),
    classif.xgboost.alpha = p_dbl(lower = log(1e-4), upper = log(1000), tags = "log",
                                  trafo = function(x) exp(x), default = log(1e-4)),
    classif.xgboost.subsample = p_dbl(lower = 0.1, upper = 1, default = 1),
    classif.xgboost.max_depth = p_int(lower = 1L, upper = 15L, default = 6L),
    classif.xgboost.min_child_weight = p_dbl(lower = log(exp(1)), upper = log(150), tags = "log",
                                             trafo = function(x) exp(x), default = log(exp(1))),
    classif.xgboost.colsample_bytree = p_dbl(lower = 0.01, upper = 1, default = 1),
    classif.xgboost.colsample_bylevel = p_dbl(lower = 0.01, upper = 1, default = 1),
    select.selector = p_uty(),  # must be part of the search space
    classif.xgboost.interaction_constraints = p_uty(),  # must be part of the search space
    classif.xgboost.monotone_constraints = p_uty()  # must be part of the search space
  )
}

get_xgboost_search_space = function() {
  ps(
    classif.xgboost.nrounds = p_dbl(lower = log(1), upper = log(5000), tags = c("int", "log"),
                                    trafo = function(x) as.integer(round(exp(x))), default = log(500)),
    classif.xgboost.eta = p_dbl(lower = log(1e-4), upper = 0, tags = "log",
                                trafo = function(x) exp(x), default = log(0.3)),
    classif.xgboost.gamma = p_dbl(lower = log(1e-4), upper = log(7), tags = "log",
                                  trafo = function(x) exp(x), default = log(1e-4)),
    classif.xgboost.lambda = p_dbl(lower = log(1e-4), upper = log(1000), tags = "log",
                                   trafo = function(x) exp(x), default = log(1)),
    classif.xgboost.alpha = p_dbl(lower = log(1e-4), upper = log(1000), tags = "log",
                                  trafo = function(x) exp(x), default = log(1e-4)),
    classif.xgboost.subsample = p_dbl(lower = 0.1, upper = 1, default = 1),
    classif.xgboost.max_depth = p_int(lower = 1L, upper = 15L, default = 6L),
    classif.xgboost.min_child_weight = p_dbl(lower = log(exp(1)), upper = log(150), tags = "log",
                                             trafo = function(x) exp(x), default = log(exp(1))),
    classif.xgboost.colsample_bytree = p_dbl(lower = 0.01, upper = 1, default = 1),
    classif.xgboost.colsample_bylevel = p_dbl(lower = 0.01, upper = 1, default = 1)
  )
}

get_ebm_search_space = function(n_features) {
  ps(
    interactions = p_int(lower = 0L, upper = max(c(10L, ceiling(sqrt((n_features * (n_features - 1L)) / 2L)))), default = 10L),
    outer_bags = p_int(lower = 1L, upper = 10L, default = 8L),
    learning_rate = p_dbl(lower = log(1e-4), upper = 0, tags = "log",
                          trafo = function(x) exp(x), default = log(0.01)),
    max_rounds = p_dbl(lower = log(1), upper = log(5000), tags = c("int", "log"),
                       trafo = function(x) as.integer(round(exp(x))), default = log(5000)),
    min_samples_leaf = p_int(lower = 1L, upper = 100L, default = 2L),
    max_leaves = p_int(lower = 1L, upper = 5L, default = 3L)  # 2 ^ 5L is maxdepth and 30 is reasonable upper limit
  )
}

get_glmnet_search_space = function() {
  ps(
    alpha = p_dbl(lower = 0, upper = 1),
    s = p_dbl(lower = -7, upper = 7, tags = "log", trafo = function(x) exp(x))
  )
}

get_xgboost_domain = function() {
  ps(
    classif.xgboost.nrounds = p_int(lower = 1L, upper = 5000L, default = 500L),
    classif.xgboost.eta = p_dbl(lower = 1e-4, upper = 1, default = 0.3),
    classif.xgboost.gamma = p_dbl(lower = 1e-4, upper = 7, default = 1e-4),
    classif.xgboost.lambda = p_dbl(lower = 1e-4, upper = 1000, default = 1),
    classif.xgboost.alpha = p_dbl(lower = 1e-4, upper = 1000, default = 1e-4),
    classif.xgboost.subsample = p_dbl(lower = 0.1, upper = 1, default = 1),
    classif.xgboost.max_depth = p_int(lower = 1L, upper = 15L, default = 6L),
    classif.xgboost.min_child_weight = p_dbl(lower = exp(1), upper = 150, default = exp(1)),
    classif.xgboost.colsample_bytree = p_dbl(lower = 0.01, upper = 1, default = 1),
    classif.xgboost.colsample_bylevel = p_dbl(lower = 0.01, upper = 1, default = 1)
  )
}


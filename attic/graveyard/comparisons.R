library(data.table)
library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3oml)
library(mlr3misc)
library(mlr3tuning)
library(EBmlr3)
library(paradox)
library(iaml)
library(bbotk)
library(ggplot2)

set.seed(1)
task = po("encodeimpact")$train(list(tsk("oml", data_id = 40981)))[[1L]]
resampling = rsmp("holdout", ratio = 0.8)$instantiate(task)

### iaml
learner = as_learner(po("colapply") %>>% po("select") %>>% lrn("classif.xgboost"))
learner$param_set$values$classif.xgboost.booster = "gbtree"
learner$param_set$values$classif.xgboost.tree_method = "exact"
learner$param_set$values$colapply.applicator = function(x) - x
measures = list(msr("classif.ce"),
                msr("iaml_selected_features",
                    select_id = "select.selector",
                    normalize = FALSE, actually_used = TRUE),  # param id
                msr("iaml_selected_interactions",
                    interaction_id = "classif.xgboost.interaction_constraints",
                    normalize = FALSE, actually_used = TRUE),  # param id
                msr("iaml_selected_non_monotone",
                    monotone_id = "classif.xgboost.monotone_constraints",
                    normalize = FALSE, actually_used = TRUE))  # param id
terminator = trm("evals", n_evals = 500L)

search_space = ps(
  classif.xgboost.nrounds = p_dbl(lower = 1, upper = log(1000), tags = c("int", "log"),
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

instance = TuningInstanceMultiCrit$new(
  task,
  learner,
  resampling,
  measures,
  terminator,
  search_space,
  store_models = TRUE
)

tuner = tnr("iaml_ea_new", mu = 30)  # see ?TunerIAMLEA
tuner$param_set$values$select_id = "select.selector"  # param id
tuner$param_set$values$interaction_id = "classif.xgboost.interaction_constraints"  # param id
tuner$param_set$values$monotone_id = "classif.xgboost.monotone_constraints"  # param id
tuner$optimize(instance)
pareto_iaml = instance$archive$best()  # Pareto optimal solutions
saveRDS(pareto_iaml, "pareto_iaml.rds")

### xgboost (same search space, 500 iterations random search)
learner = lrn("classif.xgboost")
learner$param_set$values$booster = "gbtree"
learner$param_set$values$tree_method = "exact"
measures = list(msr("classif.ce"),
                msr("iaml_selected_features",
                    learner_id = "NULL",  # hacky
                    select_id = "#FIXME:",
                    normalize = FALSE, actually_used = TRUE),  # param id
                msr("iaml_selected_interactions",
                    learner_id = "NULL",  # hacky
                    interaction_id = "#FIXME:",
                    normalize = FALSE, actually_used = TRUE),  # param id
                msr("iaml_selected_non_monotone",
                    learner_id = "NULL",  # hacky
                    monotone_id = "#FIXME:",
                    normalize = FALSE, actually_used = TRUE))  # param id
terminator = trm("evals", n_evals = 500L)

search_space = ps(
  nrounds = p_dbl(lower = 1, upper = log(1000), tags = c("int", "log"),
                  trafo = function(x) as.integer(round(exp(x))), default = log(500)),
  eta = p_dbl(lower = log(1e-4), upper = 0, tags = "log",
              trafo = function(x) exp(x), default = log(0.3)),
  gamma = p_dbl(lower = log(1e-4), upper = log(7), tags = "log",
                trafo = function(x) exp(x), default = log(1e-4)),
  lambda = p_dbl(lower = log(1e-4), upper = log(1000), tags = "log",
                 trafo = function(x) exp(x), default = log(1)),
  alpha = p_dbl(lower = log(1e-4), upper = log(1000), tags = "log",
                trafo = function(x) exp(x), default = log(1e-4)),
  subsample = p_dbl(lower = 0.1, upper = 1, default = 1),
  max_depth = p_int(lower = 1L, upper = 15L, default = 6L),
  min_child_weight = p_dbl(lower = log(exp(1)), upper = log(150), tags = "log",
                           trafo = function(x) exp(x), default = log(exp(1))),
  colsample_bytree = p_dbl(lower = 0.01, upper = 1, default = 1),
  colsample_bylevel = p_dbl(lower = 0.01, upper = 1, default = 1)
)

instance = TuningInstanceMultiCrit$new(
  task,
  learner,
  resampling,
  measures,
  terminator,
  search_space,
  store_models = TRUE
)

tuner = tnr("random_search")
tuner$optimize(instance)
pareto_xgboost = instance$archive$best()  # Pareto optimal solutions
saveRDS(pareto_xgboost, "pareto_xgboost.rds")

### EBM (simply defaults)
learner = lrn("classif.ebm")
rr = resample(task, learner, resampling)
pareto_EBM = rr$score()
pareto_EBM[, iaml_selected_features := 22L]  # uses all in round roubin
pareto_EBM[, iaml_selected_interactions := 10L]  # default is 10
pareto_EBM[, iaml_selected_non_monotone := 22L]  # cannot guarantee any monotone
saveRDS(pareto_EBM, "pareto_EBM.rds")

### LR
learner = lrn("classif.log_reg")
rr = resample(task, learner, resampling)
pareto_lr = rr$score()
pareto_lr[, iaml_selected_features := 22L]  # default "uses" all
pareto_lr[, iaml_selected_interactions := 0L]  # no interactions in link
pareto_lr[, iaml_selected_non_monotone := 0L]  # linear, i.e., monotone in link
saveRDS(pareto_lr, "pareto_lr.rds")

### FIXME: the following only holds for Australian task
pareto_iaml = readRDS("pareto_iaml.rds")
pareto_iaml[, interpretability := (iaml_selected_features / 22) + (iaml_selected_interactions / ((22 * 21) / 2)) + (iaml_selected_non_monotone / 22)]
pareto_iaml = unique(pareto_iaml[, c("classif.ce", "interpretability", "iaml_selected_features", "iaml_selected_interactions", "iaml_selected_non_monotone")])
pareto_iaml = pareto_iaml[bbotk::is_dominated(t(pareto_iaml[, c("classif.ce", "interpretability")])), ]
pareto_iaml[, method := "ours_with_xgboost"]

pareto_xgboost = readRDS("pareto_xgboost.rds")
pareto_xgboost[, interpretability := (iaml_selected_features / 22) + (iaml_selected_interactions / ((22 * 21) / 2)) + (iaml_selected_non_monotone / 22)]
pareto_xgboost = unique(pareto_xgboost[, c("classif.ce", "interpretability", "iaml_selected_features", "iaml_selected_interactions", "iaml_selected_non_monotone")])
pareto_xgboost = pareto_xgboost[bbotk::is_dominated(t(pareto_xgboost[, c("classif.ce", "interpretability")])), ]
pareto_xgboost[, method := "xgboost_random_search_HP"]

pareto_EBM = readRDS("pareto_EBM.rds")
pareto_EBM[, interpretability := (iaml_selected_features / 22) + (iaml_selected_interactions / ((22 * 21) / 2)) + (iaml_selected_non_monotone / 22)]
pareto_EBM = unique(pareto_EBM[, c("classif.ce", "interpretability", "iaml_selected_features", "iaml_selected_interactions", "iaml_selected_non_monotone")])
pareto_EBM[, method := "EBM"]

pareto_lr = readRDS("pareto_lr.rds")
pareto_lr[, interpretability := (iaml_selected_features / 22) + (iaml_selected_interactions / ((22 * 21) / 2)) + (iaml_selected_non_monotone / 22)]
pareto_lr = unique(pareto_lr[, c("classif.ce", "interpretability", "iaml_selected_features", "iaml_selected_interactions", "iaml_selected_non_monotone")])
pareto_lr[, method := "lr"]

paretos = rbind(pareto_iaml, pareto_xgboost, pareto_EBM, pareto_lr)
g = ggplot(aes(x = interpretability, y = classif.ce, colour = method), data = paretos) +
  geom_point(size = 2) +
  geom_step(direction = "hv") +
  theme_minimal() +
  labs(x = "interpretability (lower = better)")
ggsave("australian.png", plot = g, device = "png")


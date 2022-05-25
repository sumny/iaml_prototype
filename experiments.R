#library(iaml)
library(mlr3)
library(mlr3tuning)
library(mlr3learners)
library(mlr3pipelines)
devtools::load_all()

set.seed(2906)
task = tsk("iris")
library(mlr3oml)
task = tsk("oml", data_id = 1489)
learner = as_learner(po("select") %>>% lrn("classif.xgboost"))  # GraphLearner via mlr3pipelines
resampling = rsmp("cv", folds = 3L)
resampling$instantiate(task)
measures = list(msr("classif.ce"),
                msr("iaml_selected_features",
                    select_id = "select.selector"),  # param id
                msr("iaml_selected_interactions",
                    interaction_id = "classif.xgboost.interaction_constraints"),  # param id
                msr("iaml_selected_non_monotone",
                    monotone_id = "classif.xgboost.monotone_constraints"))  # param id
terminator = trm("evals", n_evals = 400)
learner$param_set$values$classif.xgboost.nrounds = 50L

search_space = ps(
  #classif.xgboost.nrounds = p_dbl(lower = 1, upper = log(500), tags = c("int", "log"),
  #                                trafo = function(x) as.integer(round(exp(x)))),
  #classif.xgboost.eta = p_dbl(lower = log(1e-4), upper = 0, tags = "log",
  #                            trafo = function(x) exp(x)),
  #classif.xgboost.gamma = p_dbl(lower = log(1e-4), upper = log(7), tags = "log",
  #                              trafo = function(x) exp(x)),
  #classif.xgboost.lambda = p_dbl(lower = log(1e-4), upper = log(1000), tags = "log",
  #                               trafo = function(x) exp(x)),
  #classif.xgboost.alpha = p_dbl(lower = log(1e-4), upper = log(1000), tags = "log",
  #                              trafo = function(x) exp(x)),
  #classif.xgboost.subsample = p_dbl(lower = 0.1, upper = 1),
  #classif.xgboost.max_depth = p_int(lower = 1L, upper = 15L),
  #classif.xgboost.min_child_weight = p_dbl(lower = 1, upper = log(150), tags = "log",
  #                                         trafo = function(x) exp(x)),
  #classif.xgboost.colsample_bytree = p_dbl(lower = 0.01, upper = 1),
  #classif.xgboost.colsample_bylevel = p_dbl(lower = 0.01, upper = 1),
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
  search_space
)

res = map_dtr(1:4, function(r) {
instance$archive$clear()
tuner = tnr("iaml_ea", mu = 100)  # see ?TunerIAMLEA
tuner$param_set$values$select_id = "select.selector"  # param id
tuner$param_set$values$interaction_id = "classif.xgboost.interaction_constraints"  # param id
tuner$param_set$values$monotone_id = "classif.xgboost.monotone_constraints"  # param id
tuner$optimize(instance)
ea = instance$archive$best()  # Pareto optimal solutions
ea_data = copy(instance$archive$data)

instance$archive$clear()
tuner = tnr("iaml")  # see ?TunerIAML
tuner$param_set$values$select_id = "select.selector"  # param id
tuner$param_set$values$interaction_id = "classif.xgboost.interaction_constraints"  # param id
tuner$param_set$values$monotone_id = "classif.xgboost.monotone_constraints"  # param id
tuner$optimize(instance)
rs = instance$archive$best()  # Pareto optimal solutions
rs_data = copy(instance$archive$data)

worst = apply(rbind(ea_data[, instance$archive$cols_y, with = FALSE], rs_data[, instance$archive$cols_y, with = FALSE]), MARGIN = 2L, FUN = function(x) max(x))
best =  apply(rbind(ea_data[, instance$archive$cols_y, with = FALSE], rs_data[, instance$archive$cols_y, with = FALSE]), MARGIN = 2L, FUN = min)
ea_y = ea[, instance$archive$cols_y, with = FALSE]
ea_y[, classif.ce := (classif.ce - best[["classif.ce"]]) / (worst[["classif.ce"]] - best[["classif.ce"]])]
ea_y[, iaml_selected_features := (iaml_selected_features - best[["iaml_selected_features"]]) / (worst[["iaml_selected_features"]] - best[["iaml_selected_features"]])]
ea_y[, iaml_selected_interactions := (iaml_selected_interactions - best[["iaml_selected_interactions"]]) / (worst[["iaml_selected_interactions"]] - best[["iaml_selected_interactions"]])]
ea_y[, iaml_selected_non_monotone := (iaml_selected_non_monotone - best[["iaml_selected_non_monotone"]]) / (worst[["iaml_selected_non_monotone"]] - best[["iaml_selected_non_monotone"]])]

rs_y = rs_data[, instance$archive$cols_y, with = FALSE]
rs_y[, classif.ce := (classif.ce - best[["classif.ce"]]) / (worst[["classif.ce"]] - best[["classif.ce"]])]
rs_y[, iaml_selected_features := (iaml_selected_features - best[["iaml_selected_features"]]) / (worst[["iaml_selected_features"]] - best[["iaml_selected_features"]])]
rs_y[, iaml_selected_interactions := (iaml_selected_interactions - best[["iaml_selected_interactions"]]) / (worst[["iaml_selected_interactions"]] - best[["iaml_selected_interactions"]])]
rs_y[, iaml_selected_non_monotone := (iaml_selected_non_monotone - best[["iaml_selected_non_monotone"]]) / (worst[["iaml_selected_non_monotone"]] - best[["iaml_selected_non_monotone"]])]

nadir = apply(rbind(ea_y, rs_y), MARGIN = 2L, FUN = function(x) max(x) + 1L)
ea_y = ea_y[!is_dominated(t(ea_y)), ]
rs_y = rs_y[!is_dominated(t(rs_y)), ]

ea_hv = emoa::dominated_hypervolume(t(ea_y), ref = nadir)
rs_hv = emoa::dominated_hypervolume(t(rs_y), ref = nadir)
data.table(ea_hv = ea_hv, rs_hv = rs_hv, ea_data = list(ea_data), rs_data = list(rs_data), repl = r)
})

if (FALSE) {
  library(ggplot2)
  dat = rbind(ea_y[, type := "ea"], rs_y[, type := "rs"])
  g = ggplot(aes(x = iaml_selected_features, y = classif.ce, colour = type), data = dat) +
    geom_point() +
    geom_step(direction = "vh")
}


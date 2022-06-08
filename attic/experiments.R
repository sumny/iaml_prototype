#library(iaml)
library(mlr3)
library(mlr3tuning)
library(mlr3learners)
library(mlr3pipelines)
library(ggplot2)
library(pammtools)
devtools::load_all()

set.seed(2906)
library(mlr3oml)
#task = tsk("oml", data_id = 1489)  # 1485
task = tsk("spam")
learner = as_learner(po("select") %>>% lrn("classif.xgboost"))  # GraphLearner via mlr3pipelines
learner_new = as_learner(po("colapply") %>>% po("select") %>>% lrn("classif.xgboost"))
learner_new$param_set$values$colapply.applicator = function(x) - x
learner_new$param_set$values$colapply.affect_columns = selector_none()
resampling = rsmp("cv", folds = 3L)
resampling$instantiate(task)
measures = list(msr("classif.ce"),
                msr("iaml_selected_features",
                    select_id = "select.selector"),  # param id
                msr("iaml_selected_interactions",
                    interaction_id = "classif.xgboost.interaction_constraints"),  # param id
                msr("iaml_selected_non_monotone",
                    monotone_id = "classif.xgboost.monotone_constraints"))  # param id
terminator = trm("evals", n_evals = 500)

search_space = ps(
  classif.xgboost.nrounds = p_dbl(lower = 1, upper = log(100), tags = c("int", "log"),
                                  trafo = function(x) as.integer(round(exp(x)))),
  classif.xgboost.eta = p_dbl(lower = log(1e-4), upper = 0, tags = "log",
                              trafo = function(x) exp(x)),
  classif.xgboost.gamma = p_dbl(lower = log(1e-4), upper = log(7), tags = "log",
                                trafo = function(x) exp(x)),
  classif.xgboost.lambda = p_dbl(lower = log(1e-4), upper = log(1000), tags = "log",
                                 trafo = function(x) exp(x)),
  classif.xgboost.alpha = p_dbl(lower = log(1e-4), upper = log(1000), tags = "log",
                                trafo = function(x) exp(x)),
  classif.xgboost.subsample = p_dbl(lower = 0.1, upper = 1),
  classif.xgboost.max_depth = p_int(lower = 1L, upper = 15L),
  classif.xgboost.min_child_weight = p_dbl(lower = 1, upper = log(150), tags = "log",
                                           trafo = function(x) exp(x)),
  classif.xgboost.colsample_bytree = p_dbl(lower = 0.01, upper = 1),
  classif.xgboost.colsample_bylevel = p_dbl(lower = 0.01, upper = 1),
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

instance_new = TuningInstanceMultiCrit$new(
  task,
  learner_new,
  resampling,
  measures,
  terminator,
  search_space,
  store_models = TRUE
)

mu = 30
res = map_dtr(1:5, function(r) {
  instance_new$archive$clear()
  tuner = tnr("iaml_ea_new", mu = mu)
  tuner$param_set$values$select_id = "select.selector"  # param id
  tuner$param_set$values$interaction_id = "classif.xgboost.interaction_constraints"  # param id
  tuner$param_set$values$monotone_id = "classif.xgboost.monotone_constraints"  # param id
  tuner$optimize(instance_new)
  eawm_data = copy(instance_new$archive$data)

  instance$archive$clear()
  tuner = tnr("iaml_ea", mu = mu, warmstart = FALSE)  # see ?TunerIAMLEA
  tuner$param_set$values$select_id = "select.selector"  # param id
  tuner$param_set$values$interaction_id = "classif.xgboost.interaction_constraints"  # param id
  tuner$param_set$values$monotone_id = "classif.xgboost.monotone_constraints"  # param id
  tuner$optimize(instance)
  ea_data = copy(instance$archive$data)
 
  instance$archive$clear()
  tuner = tnr("iaml_ea", mu = mu, warmstart = TRUE)  # see ?TunerIAMLEA
  tuner$param_set$values$select_id = "select.selector"  # param id
  tuner$param_set$values$interaction_id = "classif.xgboost.interaction_constraints"  # param id
  tuner$param_set$values$monotone_id = "classif.xgboost.monotone_constraints"  # param id
  tuner$optimize(instance)
  eaw_data = copy(instance$archive$data)

  instance$archive$clear()
  for (i in seq_len(mu)) {
    instance$eval_batch(eaw_data[i, ])
  }
  tuner = tnr("iaml")  # see ?TunerIAML
  tuner$param_set$values$select_id = "select.selector"  # param id
  tuner$param_set$values$interaction_id = "classif.xgboost.interaction_constraints"  # param id
  tuner$param_set$values$monotone_id = "classif.xgboost.monotone_constraints"  # param id
  tuner$optimize(instance)
  rs_data = copy(instance$archive$data)

  nadir = apply(rbind(eawm_data[, instance$archive$cols_y, with = FALSE], ea_data[, instance$archive$cols_y, with = FALSE], eaw_data[, instance$archive$cols_y, with = FALSE], rs_data[, instance$archive$cols_y, with = FALSE]), MARGIN = 2L, FUN = function(x) max(x) + 1L)
  
  tmp = map_dtr(1:500, function(bnr) {
    eawm_y = eawm_data[batch_nr <= bnr, instance$archive$cols_y, with = FALSE]
    eawm_y = eawm_y[!bbotk::is_dominated(t(eawm_y)), ]
    ea_y = ea_data[batch_nr <= bnr, instance$archive$cols_y, with = FALSE]
    ea_y = ea_y[!bbotk::is_dominated(t(ea_y)), ]
    eaw_y = eaw_data[batch_nr <= bnr, instance$archive$cols_y, with = FALSE]
    eaw_y = eaw_y[!bbotk::is_dominated(t(eaw_y)), ]
    rs_y = rs_data[batch_nr <= bnr, instance$archive$cols_y, with = FALSE]
    rs_y = rs_y[!bbotk::is_dominated(t(rs_y)), ]

    eawm_hv = emoa::dominated_hypervolume(t(eawm_y), ref = nadir)
    ea_hv = emoa::dominated_hypervolume(t(ea_y), ref = nadir)
    eaw_hv = emoa::dominated_hypervolume(t(eaw_y), ref = nadir)
    rs_hv = emoa::dominated_hypervolume(t(rs_y), ref = nadir)
    hvs = rbind(data.table(hv = eawm_hv, method = "eawm"), data.table(hv = ea_hv, method = "ea"), data.table(hv = rs_hv, method = "rs"), data.table(hv = eaw_hv, method = "eaw"))
    hvs[, batch_nr := bnr]
    hvs[, repl := r]
    hvs
  })
})

agg = res[, .(m_hv = mean(hv), s_hv = sd(hv) / sqrt(hv)), by = .(method, batch_nr)]

g = ggplot(aes(x = batch_nr, y = m_hv, colour = method, fill = method), data = agg[batch_nr > 30]) +
  geom_step() + labs(y = "Dom HV", x = "Iteration") +
  geom_stepribbon(aes(ymin = m_hv - s_hv, ymax = m_hv + s_hv), colour = NA, alpha = 0.3)
ggsave(file = "spam_eaw.png", plot = g)

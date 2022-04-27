
``` r
# iml example with mbo (ParEGO)
library(iaml)
library(mlr3)
library(mlr3tuning)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3mbo)  # 
library(iml)
task = tsk("spam")

learner = lrn("classif.xgboost")
learner$predict_type = "prob"
resampling = rsmp("cv", folds = 3L)
measures = list(msr("classif.ce"),
                msr("iml_number_of_features"),
                msr("iml_interaction_strength"),
                msr("iml_main_effect_complexity"))
terminator = trm("evals", n_evals = 100L)

search_space = ps(
  nrounds = p_dbl(lower = 1, upper = log(2000), tags = c("int", "log"), trafo = function(x) as.integer(round(exp(x)))),
  eta = p_dbl(lower = log(1e-4), upper = 0, tags = "log", trafo = function(x) exp(x)),
  gamma = p_dbl(lower = log(1e-4), upper = log(7), tags = "log", trafo = function(x) exp(x)),
  lambda = p_dbl(lower = log(1e-4), upper = log(1000), tags = "log", trafo = function(x) exp(x)),
  alpha = p_dbl(lower = log(1e-4), upper = log(1000), tags = "log", trafo = function(x) exp(x)),
  subsample = p_dbl(lower = 0.1, upper = 1),
  max_depth = p_int(lower = 1L, upper = 15L),
  min_child_weight = p_dbl(lower = 1, upper = log(150), tags = "log", trafo = function(x) exp(x)),
  colsample_bytree = p_dbl(lower = 0.01, upper = 1),
  colsample_bylevel = p_dbl(lower = 0.01, upper = 1)
)

instance = TuningInstanceMultiCrit$new(
  task,
  learner,
  resampling,
  measures,
  terminator,
  search_space
)

tuner = tnr("mbo")
tuner$optimize(instance)
```

``` r
# iaml example
task = tsk("spam")
learner = as_learner(po("select") %>>% lrn("classif.xgboost"))
resampling = rsmp("cv", folds = 3L)
measures = list(msr("classif.ce"),
                msr("iaml_selected_features", select_id = "select.selector"),
                msr("iaml_selected_interactions", interaction_id = "classif.xgboost.interaction_constraints"),
                msr("iaml_selected_non_monotone", monotone_id = "classif.xgboost.monotone_constraints"))
terminator = trm("evals", n_evals = 100L)

search_space = ps(
  classif.xgboost.nrounds = p_dbl(lower = 1, upper = log(2000), tags = c("int", "log"), trafo = function(x) as.integer(round(exp(x)))),
  classif.xgboost.eta = p_dbl(lower = log(1e-4), upper = 0, tags = "log", trafo = function(x) exp(x)),
  classif.xgboost.gamma = p_dbl(lower = log(1e-4), upper = log(7), tags = "log", trafo = function(x) exp(x)),
  classif.xgboost.lambda = p_dbl(lower = log(1e-4), upper = log(1000), tags = "log", trafo = function(x) exp(x)),
  classif.xgboost.alpha = p_dbl(lower = log(1e-4), upper = log(1000), tags = "log", trafo = function(x) exp(x)),
  classif.xgboost.subsample = p_dbl(lower = 0.1, upper = 1),
  classif.xgboost.max_depth = p_int(lower = 1L, upper = 15L),
  classif.xgboost.min_child_weight = p_dbl(lower = 1, upper = log(150), tags = "log", trafo = function(x) exp(x)),
  classif.xgboost.colsample_bytree = p_dbl(lower = 0.01, upper = 1),
  classif.xgboost.colsample_bylevel = p_dbl(lower = 0.01, upper = 1),
  select.selector = p_uty(),
  classif.xgboost.interaction_constraints = p_uty(),
  classif.xgboost.monotone_constraints = p_uty()
)

instance = TuningInstanceMultiCrit$new(
  task,
  learner,
  resampling,
  measures,
  terminator,
  search_space
)

tuner = tnr("iaml")
tuner$param_set$values$select_id = "select.selector"
tuner$param_set$values$interaction_id = "classif.xgboost.interaction_constraints"
tuner$param_set$values$monotone_id = "classif.xgboost.monotone_constraints"

tuner$optimize(instance)
```

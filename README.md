
# Interpretable (auto) ML within the mlr3 ecosystem via multi-objective optimization

## Installation

Installation can be performed via the `devtools` package:

``` r
devtools::install_github("sumny/iaml_prototype")
```

## General Workflow

Below, we illustrate two approaches for multi-objective optimization of
machine learning models with respect to performance and
interpretability.

Generally, we always use a `Learner`, a `Task`, `Measure`s and a
`Resampling` strategy and construct a `TuningInstanceMultiCrit` by
providing the `spearch_space` of the `Learner` as well as a termination
criteria. For more details on tuning with `mlr3`, see
[here](https://mlr3book.mlr-org.com/optimization.html).

## CE, NF, IAS, MEC optimized via ParEGO

We tune hyperparameters of an xgboost learner and optimize the
classification error, number of features, interaction strength and main
effect complexity.

``` r
library(iaml)
library(mlr3)
library(mlr3tuning)
library(mlr3learners)
library(mlr3mbo)
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
  nrounds = p_dbl(lower = 1, upper = log(2000), tags = c("int", "log"),
                  trafo = function(x) as.integer(round(exp(x)))),
  eta = p_dbl(lower = log(1e-4), upper = 0, tags = "log",
              trafo = function(x) exp(x)),
  gamma = p_dbl(lower = log(1e-4), upper = log(7), tags = "log",
                trafo = function(x) exp(x)),
  lambda = p_dbl(lower = log(1e-4), upper = log(1000), tags = "log",
                 trafo = function(x) exp(x)),
  alpha = p_dbl(lower = log(1e-4), upper = log(1000), tags = "log",
                trafo = function(x) exp(x)),
  subsample = p_dbl(lower = 0.1, upper = 1),
  max_depth = p_int(lower = 1L, upper = 15L),
  min_child_weight = p_dbl(lower = 1, upper = log(150), tags = "log",
                           trafo = function(x) exp(x)),
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

tuner = tnr("mbo")  # by default ParEGO, see https://github.com/mlr-org/mlr3mbo for details
tuner$optimize(instance)
instance$archive$best()  # Pareto optimal solutions
```

## CE, number of features, number of interactions and number of non-monotone features

We tune hyperparameters of an xgboost learner and optimize the
classification error, the number of features, number of interactions and
number of non-monotone features. Feature selection is incorporated via a
suitable `PipeOp`. Note that measures require ids of parameters of the
learner that allow for the feature selection and specification of
interaction and monotonicity constraints.

``` r
#library(iaml)
library(mlr3)
library(mlr3tuning)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3oml)
devtools::load_all()

set.seed(2906)
task = tsk("oml", data_id = 1489)
learner = as_learner(po("colapply") %>>% po("select") %>>% lrn("classif.xgboost"))
learner$param_set$values$classif.xgboost.booster = "gbtree"
learner$param_set$values$classif.xgboost.tree_method = "exact"
learner$param_set$values$colapply.applicator = function(x) - x
resampling = rsmp("holdout", ratio = 2/3)
resampling$instantiate(task)
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
terminator = trm("evals", n_evals = 100L)

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

tuner = tnr("iaml_ea_new", mu = 10)  # see ?TunerIAMLEA
tuner$param_set$values$select_id = "select.selector"  # param id
tuner$param_set$values$interaction_id = "classif.xgboost.interaction_constraints"  # param id
tuner$param_set$values$monotone_id = "classif.xgboost.monotone_constraints"  # param id
tuner$optimize(instance)
pareto = instance$archive$best()  # Pareto optimal solutions
```

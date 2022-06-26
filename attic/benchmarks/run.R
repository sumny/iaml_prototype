library(data.table)
setDTthreads(1L)
library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3oml)
library(mlr3misc)
library(mlr3tuning)
library(paradox)
library(iaml)

RhpcBLASctl::blas_set_num_threads(1L)
RhpcBLASctl::omp_set_num_threads(1L)

# FIXME: use resampling splits etc., i.e., tasks instead of datasets

eval_ = function(job, data, instance, ...) {
  library(mlr3)
  library(mlr3learners)
  library(mlr3pipelines)
  library(mlr3misc)
  library(mlr3tuning)
  library(paradox)
  library(iaml)

  data.table::setDTthreads(1L)
  RhpcBLASctl::blas_set_num_threads(1L)
  RhpcBLASctl::omp_set_num_threads(1L)

  #logger = lgr::get_logger("mlr3")
  #logger$set_threshold("warn")
  #logger = lgr::get_logger("bbotk")
  #logger$set_threshold("warn")
  #future::plan("sequential")

  task = instance$task
  resampling = instance$resampling
  actually_used = instance$actually_used
  optimizer_id = job$algo.pars$optimizer
  if (optimizer_id == "eawm") {
    learner = as_learner(po("colapply") %>>% po("select") %>>% lrn("classif.xgboost"))
    learner$param_set$values$classif.xgboost.booster = "gbtree"
    learner$param_set$values$classif.xgboost.tree_method = "exact"
    learner$param_set$values$colapply.applicator = function(x) - x
  } else {
    learner = as_learner(po("select") %>>% lrn("classif.xgboost"))
    learner$param_set$values$classif.xgboost.booster = "gbtree"
    learner$param_set$values$classif.xgboost.tree_method = "exact"
  }

  measures = list(msr("classif.ce"),
                msr("iaml_selected_features",
                    select_id = "select.selector",
                    normalize = FALSE, actually_used = actually_used),  # param id
                msr("iaml_selected_interactions",
                    interaction_id = "classif.xgboost.interaction_constraints",
                    normalize = FALSE, actually_used = actually_used),  # param id
                msr("iaml_selected_non_monotone",
                    monotone_id = "classif.xgboost.monotone_constraints",
                    normalize = FALSE, actually_used = actually_used))  # param id

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
    search_space
  )

  mu = 30L
  if (optimizer_id == "eawm") {
    tuner = tnr("iaml_ea_new", mu = mu)
  } else if (optimizer_id == "eaw") {
    tuner = tnr("iaml_ea", mu = mu, warmstart = TRUE)
  } else if (optimizer_id == "ea") {
    tuner = tnr("iaml_ea", mu = mu, warmstart = FALSE)
  } else if (optimizer_id == "rsw") {
    tuner = tnr("iaml_ea", mu = mu, warmstart = TRUE)
    instance$terminator$param_set$values$n_evals = mu
  } else if (optimizer_id == "rs") {
    tuner = tnr("iaml")
  }

  tuner$optimize(instance)
  if (optimizer_id == "rsw") {
    tuner = tnr("iaml")
    instance$terminator$param_set$values$n_evals = 500L
    tuner$optimize(instance)
  }
  instance$archive$data
}

library(batchtools)
reg = makeExperimentRegistry(file.dir = "/gscratch/lschnei8/registry_iaml_prototype")
#reg = makeExperimentRegistry(file.dir = NA)
saveRegistry(reg)

# Australian, car, airlines, blood-transfusion-service-center, phoneme, Amazon_employee_access, shuttle, madelon, madeline
# FIXME: currently only twoclass works
#ids = c(40981, 40975, 1169, 1464, 1489, 4135, 40685, 1485, 41144)
ids = c(40981, 1464, 1489, 4135, 41144)
tasks = map(ids, function(id) {
  task = tsk("oml", data_id = id)
  po("encodeimpact")$train(list(task))[[1L]]
})
resamplings = map(tasks, function(task) {
 set.seed(1)
 rsmp("holdout", ratio = 0.8)$instantiate(task)
})
instances = data.table(id = ids, task = tasks, resampling = resamplings, actually_used = FALSE)
instances = rbind(instances, instances)
instances[seq_along(ids), actually_used := TRUE]
instances[, id_plan := 1:.N]

# add problems
prob_designs = imap(split(instances, instances$id_plan), function(instance, name) {
  addProblem(as.character(instance$id_plan), fun = function(...) list(...), seed = 123)
  set_names(list(instance), as.character(instance$id_plan))
})
nn = sapply(prob_designs, names)
prob_designs = unlist(prob_designs, recursive = FALSE, use.names = FALSE)
names(prob_designs) = nn

# add eval_ algorithm (never use `eval` as a function name or have a function named `eval` in .GlobalEnv)
addAlgorithm("eval_", fun = eval_)

for (optimizer_id in c("eawm", "eaw", "ea", "rsw", "rs")) {
  ids = addExperiments(
      prob.designs = prob_designs,
      algo.designs = list(eval_ = data.table(optimizer = optimizer_id)),
      repls = 5L
  )
  addJobTags(ids, optimizer_id)
}

# standard resources used to submit jobs to cluster
resources.serial.default = list(
  walltime = 3600L * 14L, memory = 1024L * 16L, max.concurrent.jobs = 9999L
)

jobs = findJobs()
submitJobs(jobs, resources = resources.serial.default)

#######################################################################################################################################################################################################

tab = getJobTable()
tab = tab[job.id %in% findDone()$job.id]
results = reduceResultsDataTable(tab$job.id, fun = function(x, job) {
  data = x[, c("classif.ce", "iaml_selected_features", "iaml_selected_interactions", "iaml_selected_non_monotone", "batch_nr"), with = FALSE]
  data[, actually_used := job$prob.pars$actually_used]
  data[, task_id := job$prob.pars$id]
  data[, method := job$algo.pars$optimizer]
  data[, repl := job$repl]
  data
})
results = rbindlist(results$result, fill = TRUE)
saveRDS(results, "/gscratch/lschnei8/iaml_prototype.rds")


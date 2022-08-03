library(data.table)
setDTthreads(1L)
library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3oml)
library(mlr3misc)
library(mlr3tuning)
library(mlr3mbo)
library(paradox)
library(bbotk)
library(iaml)

root = here::here()
source_files = file.path(root, "attic", "benchmarks", c("helpers.R", "search_spaces.R"))

RhpcBLASctl::blas_set_num_threads(1L)
RhpcBLASctl::omp_set_num_threads(1L)

eval_ = function(job, data, instance, ...) {
  library(data.table)
  library(mlr3)
  library(mlr3learners)
  library(mlr3pipelines)
  library(mlr3misc)
  library(mlr3tuning)
  library(mlr3mbo)
  library(paradox)
  library(bbotk)
  library(iaml)
  library(data.table)

  RhpcBLASctl::blas_set_num_threads(1L)
  RhpcBLASctl::omp_set_num_threads(1L)

  logger = lgr::get_logger("mlr3")
  logger$set_threshold("warn")
  logger = lgr::get_logger("bbotk")
  logger$set_threshold("warn")
  future::plan("sequential")

  task_train = instance$task_train
  task_test = instance$task_test
  resampling_inner = instance$resampling_inner
  method = job$algo.pars$method

  results = if (method == "gagga") {
    nested_resampling_gagga(task_train, task_test = task_test, resampling_inner = resampling_inner, n_evals = 230L)
  } else if (method == "xgboost") {
    nested_resampling_xgboost(task_train, task_test = task_test, resampling_inner = resampling_inner, n_evals = 230L)
  } else if (method == "ebm") {
    reticulate::use_condaenv("EBmlr3", required = TRUE)
    library(EBmlr3)
    nested_resampling_ebm(task_train, task_test = task_test, resampling_inner = resampling_inner, n_evals = 230L)
  } else if (method == "glmnet") {
    nested_resampling_glmnet(task_train, task_test = task_test, resampling_inner = resampling_inner, n_evals = 230L)
  } else if (method == "rf") {
    random_forest(task_train, task_test = task_test)
  }
  results
}

library(batchtools)
reg = makeExperimentRegistry(file.dir = "/gscratch/lschnei8/registry_iaml_prototype_ours_so_parallel", source = source_files)
#reg = makeExperimentRegistry(file.dir = NA)
saveRegistry(reg)

ids = c(37, 43, 3903, 3913, 3918, 10093, 9946, 146819, 359955, 189922, 359962, 190392, 167120, 190137, 190410, 168350, 359975, 359972, 146820)
tasks = map(ids, function(id) {
  task = tsk("oml", task_id = id)
  task
})
#checks = map_lgl(tasks, function(task) {
#  all(c("factor", "ordered", "logical", "POSIXct", "character") %nin% unique(task$feature_types)) && sum(task$missings()) == 0L && sum(apply(task$data(cols = task$feature_names), 2, function(x) length(unique(x)) <= 2)) == 0L
#})
resamplings_outer = map(seq_along(ids), function(i) {
  id = ids[[i]]
  set.seed(id)
  rsmp("holdout", ratio = 2/3)$instantiate(tasks[[i]])
})
tasks_train = map(seq_along(ids), function(i) {
  train_set = resamplings_outer[[i]]$train_set(1L)
  task_train = tasks[[i]]$clone(deep = TRUE)$filter(rows = train_set)
  task_train
})
tasks_test = map(seq_along(ids), function(i) {
  test_set = resamplings_outer[[i]]$test_set(1L)
  task_test = tasks[[i]]$clone(deep = TRUE)$filter(rows = test_set)
  task_test
})
resamplings_inner =  map(seq_along(ids), function(i) {
  id = ids[[i]]
  set.seed(id)
  resampling_inner = rsmp("cv", folds = 10L)$instantiate(tasks_train[[i]])
  resampling_inner
})

instances = data.table(id = ids, task = tasks, resampling_outer = resamplings_outer, task_train = tasks_train, task_test = tasks_test, resampling_inner = resamplings_inner)
instances[, id_plan := 1:.N]

# add problems
prob_designs = imap(split(instances, instances$id_plan), function(instance, name) {
  addProblem(as.character(instance$id_plan), fun = function(...) list(...), seed = instance$id)
  set_names(list(instance), as.character(instance$id_plan))
})
nn = sapply(prob_designs, names)
prob_designs = unlist(prob_designs, recursive = FALSE, use.names = FALSE)
names(prob_designs) = nn

# add eval_ algorithm (never use `eval` as a function name or have a function named `eval` in .GlobalEnv)
addAlgorithm("eval_", fun = eval_)

for (method in c("gagga", "xgboost", "ebm", "glmnet", "rf")) {
  ids = addExperiments(
      prob.designs = prob_designs,
      algo.designs = list(eval_ = data.table(method = method)),
      repls = 10L
  )
  addJobTags(ids, method)
}

# standard resources used to submit jobs to cluster
# FIXME: use actual time needed
resources.serial.default = list(
  max.concurrent.jobs = 9999L, ncpus = 1L
)

jobs = getJobTable()
jobs[, memory := 1024L * 32L]
jobs[, walltime := 3600L * 24L * 3L]
jobs[tags == "gagga" | tags == "xgboost" | tags == "ebm", memory := 1024L * 64L]
jobs[tags == "gagga" | tags == "xgboost" | tags == "ebm", walltime := 3600L * 24L * 6L]
jobs[problem == 10 | problem == 12 | problem == 13 | problem == 15, memory := 1024L * 128L]
jobs = jobs[, c("job.id", "memory", "walltime")]
submitJobs(jobs, resources = resources.serial.default)

expired = jobs[job.id %in% findExpired()$job.id]
expired[, memory := 131072L]
expired[tab[job.id %in% expired$job.id]$tags == "ebm", memory := 262144L]
submitJobs(expired, resources = resources.serial.default)

#######################################################################################################################################################################################################

tab = getJobTable()
tab = tab[job.id %in% findDone()$job.id]
results = reduceResultsDataTable(tab$job.id, fun = function(x, job) {
  data = x
  data[, tuning_data := NULL]
  data[, task_id := job$prob.pars$id]
  data[, method := job$algo.pars$method]
  data[, repl := job$repl]
  data
})
results = rbindlist(results$result, fill = TRUE)
saveRDS(results, "/gscratch/lschnei8/iaml_prototype_ours_so.rds")


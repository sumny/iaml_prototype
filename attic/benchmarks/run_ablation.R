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

  task = instance$task
  repl = job$repl
  set.seed(repl)  # same outer and inner resampling for all methods given a repl on a task
  resampling_outer = rsmp("holdout", ratio = 2/3)$instantiate(task)
  train_set = resampling_outer$train_set(1L)
  test_set = resampling_outer$test_set(1L)
  task_train = task$clone(deep = TRUE)$filter(rows = train_set)
  task_test = task$clone(deep = TRUE)$filter(rows = test_set)
  resampling_inner = rsmp("cv", folds = 10L)$instantiate(task_train)
 
  method = job$algo.pars$method
  # IAML: (crossover FALSE, mutation FALSE, both FALSE, both TRUE) x use_detectors (TRUE / FALSE); both TRUE + use_detectors TRUE not needed
  #       random (TRUE, FALSE) x use_detectors (TRUE, FALSE); random FALSE + use_detectors TRUE not needed
  # FIXME:
  crossover = isTRUE(job$algo.pars$crossover)
  mutation = isTRUE(job$algo.pars$mutation)
  random = isTRUE(job$algo.pars$random)
  detectors = isTRUE(job$algo.pars$detectors)

  set.seed(job$seed) 

  results = if (method == "gagga_ablation") {
    nested_resampling_gagga_ablation(task_train, task_test = task_test, resampling_inner = resampling_inner, crossover = crossover, mutation = mutation, random = random, detectors = detectors, n_evals = 230L)
  } else if (method == "xgboost_mo") {
    nested_resampling_xgboost_mo(task_train, task_test = task_test, resampling_inner = resampling_inner, n_evals = 230L)
  }
  results
}

library(batchtools)
reg = makeExperimentRegistry(file.dir = "/gscratch/lschnei8/registry_iaml_prototype_ablation", source = source_files)
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

instances = data.table(id = ids, task = tasks)
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
ablation1 = as.data.table(expand.grid(method = "gagga_ablation", crossover = c(TRUE, FALSE), mutation = c(TRUE, FALSE), detectors = c(TRUE, FALSE)))[-1L, ]  # TRUE, TRUE, TRUE is default
ablation2 = as.data.table(expand.grid(method = "gagga_ablation", random = c(TRUE, FALSE), detectors = c(TRUE, FALSE)))[-2L, ]  # FALSE, TRUE is default
ablation3 = data.table(method = "xgboost_mo")
# NAs get default
ablation = rbind(ablation1, ablation2, ablation3, fill = TRUE)

for (i in seq_len(nrow(ablation))) {
  ids = addExperiments(
      prob.designs = prob_designs,
      algo.designs = list(eval_ = ablation[i, ]),
      repls = 10L
  )
  addJobTags(ids, as.character(ablation[i, ]$method))
}

# standard resources used to submit jobs to cluster
resources.serial.default = list(
  max.concurrent.jobs = 9999L, ncpus = 1L
)

jobs = getJobTable()
time = readRDS("time_ours_so.rds")
jobs[, method := "gagga"]  # all comparable runtime
jobs[, memory := 1024L * 64L]
jobs[problem == 10 | problem == 12 | problem == 13 | problem == 15, memory := 1024L * 128L]
jobs = merge(jobs, time, by = c("problem", "method"))
jobs = jobs[, c("job.id", "memory", "walltime")]
submitJobs(jobs, resources = resources.serial.default)

expired = jobs[job.id %in% findExpired()$job.id]
expired[, memory := 1024L * 16L]
expired[, walltime := walltime + 3600L * 24L]
expired[walltime >= 3600L * 24L * 7L, walltime := 3600L * 24L * 7L]
submitJobs(expired, resources = resources.serial.default)

#######################################################################################################################################################################################################

tab = getJobTable()
tab = tab[job.id %in% findDone()$job.id & tags == "xgboost_mo"]
results = reduceResultsDataTable(tab$job.id, fun = function(x, job) {
  data = x
  data[, tuning_data := NULL]
  data[, task_id := job$prob.pars$id]
  data[, method := job$algo.pars$method]
  data[, repl := job$repl]
  data
})
results = rbindlist(results$result, fill = TRUE)
saveRDS(results, "/gscratch/lschnei8/iaml_prototype_ablation_xgboost_mo.rds")

tab = getJobTable()
tab = tab[job.id %in% findDone()$job.id & tags == "gagga_ablation"]
results = reduceResultsDataTable(tab$job.id, fun = function(x, job) {
  data = x
  data[, tuning_data := NULL]
  data[, task_id := job$prob.pars$id]
  data[, method := job$algo.pars$method]
  data[, crossover := job$algo.pars$crossover]
  data[, mutation := job$algo.pars$mutation]
  data[, detectors := job$algo.pars$detectors]
  data[, random := job$algo.pars$random]
  data[, repl := job$repl]
  data
})
results = rbindlist(results$result, fill = TRUE)
saveRDS(results, "/gscratch/lschnei8/iaml_prototype_ablation_gagga.rds")


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

# FIXME: use resampling splits etc., i.e. tasks instead of datasets

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
  optimizer_id = job$algo.pars$optimizer
  learner = if (optimizer_id == "eawm") {
    learner = as_learner(po("colapply") %>>% po("select") %>>% lrn("classif.xgboost"))
    learner$param_set$values$classif.xgboost.booster = "gbtree"
    learner$param_set$values$classif.xgboost.tree_method = "exact"
    learner$param_set$values$colapply.applicator = function(x) - x
  } else {
    learner = as_learner(po("select") %>>% lrn("classif.xgboost"))  # GraphLearner via mlr3pipelines
    learner$param_set$values$classif.xgboost.booster = "gbtree"
    learner$param_set$values$classif.xgboost.tree_method = "exact"
  }

  measures = list(msr("classif.ce"),
                  msr("iaml_selected_features",
                      select_id = "select.selector"),  # param id
                  msr("iaml_selected_interactions",
                      interaction_id = "classif.xgboost.interaction_constraints"),  # param id
                  msr("iaml_selected_non_monotone",
                      monotone_id = "classif.xgboost.monotone_constraints"))  # param id
  terminator = trm("evals", n_evals = 500L)

  search_space = ps(
    classif.xgboost.nrounds = p_dbl(lower = 1, upper = log(1000), tags = c("int", "log"),
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
ids = c(40981, 1169, 1464, 1489, 4135, 1485, 41144)
tasks = map(ids, function(id) {
  task = tsk("oml", data_id = id)
  po("encodeimpact")$train(list(task))[[1L]]
})
resamplings = map(tasks, function(task) {
 rsmp("cv", folds = 3L)$instantiate(task)
})
instances = data.table(id = ids, task = tasks, resampling = resamplings)
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
  walltime = 3600L * 48L, memory = 1024L * 16L, max.concurrent.jobs = 9999L
)

jobs = findJobs()
submitJobs(jobs, resources = resources.serial.default)

#######################################################################################################################################################################################################

tab = getJobTable()
tab = tab[job.id %in% findDone()$job.id]
results = reduceResultsDataTable(tab$job.id, fun = function(x, job) {
  data = x[, c("classif.ce", "iaml_selected_features", "iaml_selected_interactions", "iaml_selected_non_monotone", "batch_nr"), with = FALSE]
  data[, task_id := job$prob.pars$id]
  data[, method := job$algo.pars$optimizer]
  data[, repl := job$repl]
  data
})
results = rbindlist(results$result, fill = TRUE)
saveRDS(results, "/gscratch/lschnei8/iaml_prototype.rds")

# FIXME: in analysis check which were one hot encoded and which were not
#nadir = apply(rbind(eawm_data[, instance$archive$cols_y, with = FALSE], ea_data[, instance$archive$cols_y, with = FALSE], eaw_data[, instance$archive$cols_y, with = FALSE], rs_data[, instance$archive$cols_y, with = FALSE]), MARGIN = 2L, FUN = function(x) max(x) + 1L)


#
#tmp = map_dtr(1:500, function(bnr) {
#  eawm_y = eawm_data[batch_nr <= bnr, instance$archive$cols_y, with = FALSE]
#  eawm_y = eawm_y[!bbotk::is_dominated(t(eawm_y)), ]
#  ea_y = ea_data[batch_nr <= bnr, instance$archive$cols_y, with = FALSE]
#  ea_y = ea_y[!bbotk::is_dominated(t(ea_y)), ]
#  eaw_y = eaw_data[batch_nr <= bnr, instance$archive$cols_y, with = FALSE]
#  eaw_y = eaw_y[!bbotk::is_dominated(t(eaw_y)), ]
#  rs_y = rs_data[batch_nr <= bnr, instance$archive$cols_y, with = FALSE]
#  rs_y = rs_y[!bbotk::is_dominated(t(rs_y)), ]
#
#  eawm_hv = emoa::dominated_hypervolume(t(eawm_y), ref = nadir)
#  ea_hv = emoa::dominated_hypervolume(t(ea_y), ref = nadir)
#  eaw_hv = emoa::dominated_hypervolume(t(eaw_y), ref = nadir)
#  rs_hv = emoa::dominated_hypervolume(t(rs_y), ref = nadir)
#  hvs = rbind(data.table(hv = eawm_hv, method = "eawm"), data.table(hv = ea_hv, method = "ea"), data.table(hv = rs_hv, method = "rs"), data.table(hv = eaw_hv, method = "eaw"))
#  hvs[, batch_nr := bnr]
#  hvs[, repl := r]
#  hvs
#})
#
#
#agg = res[, .(m_hv = mean(hv), s_hv = sd(hv) / sqrt(hv)), by = .(method, batch_nr)]
#
#g = ggplot(aes(x = batch_nr, y = m_hv, colour = method, fill = method), data = agg[batch_nr > 30]) +
#  geom_step() + labs(y = "Dom HV", x = "Iteration") +
#  geom_stepribbon(aes(ymin = m_hv - s_hv, ymax = m_hv + s_hv), colour = NA, alpha = 0.3)
#ggsave(file = "spam_eaw.png", plot = g)

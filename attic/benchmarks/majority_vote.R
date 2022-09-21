library(data.table)
setDTthreads(1L)
library(mlr3)
library(mlr3oml)
library(mlr3misc)

# run locally and not on cluster

set.seed(2409)

learner = lrn("classif.featureless")

ids = c(37, 43, 3903, 3913, 3918, 10093, 9946, 146819, 359955, 189922, 359962, 190392, 167120, 190137, 190410, 168350, 359975, 359972, 146820)
tasks = map(ids, function(id) {
  task = tsk("oml", task_id = id)
  task
})

results = map_dtr(seq_along(tasks), function(i) {
  map_dtr(seq_len(10L), function(repl_) {
    task = tasks[[i]]
    learner = lrn("classif.featureless")
    set.seed(repl_)  # same outer and inner resampling for all methods given a repl on a task
    resampling_outer = rsmp("holdout", ratio = 2/3)$instantiate(task)
    train_set = resampling_outer$train_set(1L)
    test_set = resampling_outer$test_set(1L)
    task_train = task$clone(deep = TRUE)$filter(rows = train_set)
    task_test = task$clone(deep = TRUE)$filter(rows = test_set)
    resampling_inner = rsmp("cv", folds = 10L)$instantiate(task_train)

    val = resample(task_train, learner = learner, resampling = resampling_inner)$aggregate(msr("classif.ce"))
    learner$train(task_train)
    test = learner$predict(task_test)$score(msr("classif.ce"))
    data.table(task_id = ids[i], method = "majority", repl = repl_, classif.ce = val, ce_test = test, iaml_selected_features_proxy = 0, iaml_selected_interactions_proxy = 0, iaml_selected_non_monotone_proxy = 0)
  })
})

saveRDS(results, "iaml_prototype_majority_vote.rds")

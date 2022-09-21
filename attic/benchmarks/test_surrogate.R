library(mlr3)
library(mlr3learners)
library(mlr3misc)
library(data.table)
library(iaml)

all = readRDS("iaml_prototype_ours_so.rds")
task_ids = unique(all$task_id)
results = map_dtr(task_ids, function(task_id_) {
  use_all_x = FALSE
  tryCatch({
  dat = rbindlist(all[method == "gagga" & task_id == task_id_][["tuning_pareto"]])
  
  x_ids = c("classif.xgboost.nrounds", "classif.xgboost.eta", "classif.xgboost.gamma", "classif.xgboost.lambda", "classif.xgboost.alpha", "classif.xgboost.subsample", "classif.xgboost.max_depth", "classif.xgboost.min_child_weight", "classif.xgboost.colsample_bytree", "classif.xgboost.colsample_bylevel")
  
  x = map_dtc(x_ids, function(x_id) {
    unlist(dat[[x_id]])
  })
  colnames(x) = x_ids
  
  y = dat[, c("classif.ce", "iaml_selected_features_proxy", "iaml_selected_interactions_proxy", "iaml_selected_non_monotone_proxy")]
  
  features = map_dtr(dat$iaml, function(x) {
    features = x$groups$features
    value = x$groups$belonging - 1L  # start from 0
    monotonicity = x$monotone_features$monotonicity
    monotonicity[monotonicity == 1L] = -1L  # constrained get -1L
    monotonicity[is.na(monotonicity) | monotonicity == 0L] = 1L  # unconstrained gets 1L
    value = value * monotonicity
    names(value) = features
    as.data.table(t(value))
  })
  
  dat = cbind(x, features, y)
  
  task = TaskRegr$new("test", backend = dat, target = "classif.ce")
  if (use_all_x) task$col_roles$feature = c(x_ids, colnames(features)) else task$col_roles$feature = x_ids
  
  learner = lrn("regr.ranger", num.trees = 2000L, keep.inbag = TRUE)
  
  task$col_roles$target = "classif.ce"
  kt_ce = resample(task, learner, resampling = rsmp("cv"))$aggregate(msr("regr.ktau"))
  
  task$col_roles$target = "iaml_selected_features_proxy"
  kt_f = resample(task, learner, resampling = rsmp("cv"))$aggregate(msr("regr.ktau"))
  
  task$col_roles$target = "iaml_selected_interactions_proxy"
  kt_i = resample(task, learner, resampling = rsmp("cv"))$aggregate(msr("regr.ktau"))
  
  task$col_roles$target = "iaml_selected_non_monotone_proxy"
  kt_m = resample(task, learner, resampling = rsmp("cv"))$aggregate(msr("regr.ktau"))
 
  tmp = rbind(data.table(kt = kt_ce, measure = "ce"), data.table(kt = kt_f, measure = "features"), data.table(kt = kt_i, measure = "interactions"), data.table(kt = kt_m, measure = "monotonicity"))
  tmp[, task_id := task_id_]
  tmp
  }, error = function(ec) data.table())
})

library(ggplot2)

results_all_x = results
results_x = results

g_all = ggplot(aes(x = as.factor(task_id), y = kt, colour = measure), data = results_all_x) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(x = "Task ID", y = "Kendall's Tau", colour = "Measure")
ggsave("test_surrogate_all.png", plot = g_all, width = 16, height = 8)

g_x = ggplot(aes(x = as.factor(task_id), y = kt, colour = measure), data = results_x) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(x = "Task ID", y = "Kendall's Tau", colour = "Measure")
ggsave("test_surrogate_x.png", plot = g_x, width = 16, height = 8)


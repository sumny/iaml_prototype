library(batchtools)
library(data.table)
library(mlr3misc)
library(emoa)
library(bbotk)

reg = loadRegistry("/home/lps/Clusters/home")

gagga = readRDS("iaml_prototype_ours_so.rds")[method == "gagga"]
xgboost_mo = readRDS("iaml_prototype_ablation_xgboost_mo.rds")
dat = rbind(gagga, xgboost_mo, fill = TRUE)
majority = readRDS("iaml_prototype_majority_vote.rds")

ys = c("classif.ce", "iaml_selected_features_proxy", "iaml_selected_interactions_proxy", "iaml_selected_non_monotone_proxy")
best_paretos = map_dtr(unique(dat$task_id), function(task_id_) {
  map_dtr(unique(dat$repl), function(repl_) {
    tmp = dat[task_id == task_id_ & repl == repl_]
    if (nrow(tmp) != 2L) {
      return(data.table())
    }
    best_pareto = unique(rbind(tmp[method == "gagga", "tuning_pareto"][[1L]][[1L]][, ..ys], tmp[method == "xgboost_mo", "tuning_pareto"][[1L]][[1L]][, ..ys], majority[task_id == task_id_ & repl == repl_, ..ys]))
    best_pareto = best_pareto[!is_dominated(t(best_pareto)), ]
    data.table(best_pareto = list(best_pareto), repl = repl_, task_id = task_id_)
  })
})
best_paretos[, id := paste0(task_id, "_", repl)]

# ours_so
tab = getJobTable()
tab = tab[job.id %in% findDone()$job.id & tags == "gagga"]
results = reduceResultsDataTable(tab$job.id, fun = function(x, job) {
  if (paste0(job$prob.pars$id, "_", job$repl) %nin% best_paretos$id) {
    return(data.table())
  }
  ref = t(t(c(1, 1, 1, 1)))
  ys = c("classif.ce", "iaml_selected_features_proxy", "iaml_selected_interactions_proxy", "iaml_selected_non_monotone_proxy")
  best_pareto = best_paretos[task_id == job$prob.pars$id & repl == job$repl]$best_pareto[[1L]]
  tuning_data = x$tuning_data[[1L]]
  tuning_data[, iteration := seq_len(.N)]
  tuning_data = tuning_data[, c("iteration", ys), with = FALSE]
  tuning_data = rbind(cbind(data.table(iteration = 0L), majority[task_id == job$prob.pars$id & repl == job$repl, ..ys]), tuning_data)
  map_dtr(1:230, function(iteration_) {
    dhv_val_ = dominated_hypervolume(t(tuning_data[1:(iteration_ + 1), - "iteration"]), ref = ref)
    hv_val_ = hypervolume_indicator(t(tuning_data[1:(iteration_ + 1), - "iteration"]), o = t(best_pareto), ref = ref)
    data.table(dhv_val = dhv_val_, hv_val = hv_val_, iteration = iteration_, task_id = job$prob.pars$id, method = job$algo.pars$method, repl = job$repl)
  })
})
results = rbindlist(results$result, fill = TRUE)
saveRDS(results, "gagga_dhv_majority_vote.rds")

# xgboost_mo iteration
tab = getJobTable()
tab = tab[job.id %in% findDone()$job.id & tags == "xgboost_mo"]
results = reduceResultsDataTable(tab$job.id, fun = function(x, job) {
  if (paste0(job$prob.pars$id, "_", job$repl) %nin% best_paretos$id) {
    return(data.table())
  }
  ref = t(t(c(1, 1, 1, 1)))
  ys = c("classif.ce", "iaml_selected_features_proxy", "iaml_selected_interactions_proxy", "iaml_selected_non_monotone_proxy")
  best_pareto = best_paretos[task_id == job$prob.pars$id & repl == job$repl]$best_pareto[[1L]]
  tuning_data = x$tuning_data[[1L]]
  tuning_data[, iteration := seq_len(.N)]
  tuning_data = tuning_data[, c("iteration", ys), with = FALSE]
  tuning_data = rbind(cbind(data.table(iteration = 0L), majority[task_id == job$prob.pars$id & repl == job$repl, ..ys]), tuning_data)
  map_dtr(1:230, function(iteration_) {
    dhv_val_ = dominated_hypervolume(t(tuning_data[1:(iteration_ + 1), - "iteration"]), ref = ref)
    hv_val_ = hypervolume_indicator(t(tuning_data[1:(iteration_ + 1), - "iteration"]), o = t(best_pareto), ref = ref)
    data.table(dhv_val = dhv_val_, hv_val = hv_val_, iteration = iteration_, task_id = job$prob.pars$id, method = job$algo.pars$method, repl = job$repl)
  })
})
results = rbindlist(results$result, fill = TRUE)
saveRDS(results, "xgboost_mo_dhv_majority_vote.rds")


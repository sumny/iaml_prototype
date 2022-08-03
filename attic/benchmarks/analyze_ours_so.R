library(data.table)
library(mlr3misc)
library(bbotk)
library(ggplot2)
library(emoa)

dat = readRDS("iaml_prototype_ours_so.rds")

ref = t(t(c(1, 1, 1, 1)))
hvs = map_dtr(unique(dat$task_id), function(task_id_) {
  map_dtr(unique(dat$repl), function(repl_) {
    tmp = dat[task_id == task_id_ & repl == repl_]
      if (nrow(tmp) != 5L) {
        return(data.table())
      }
      gagga = tmp[method == "gagga"]$pareto[[1L]][, c("ce_test", "iaml_selected_features_proxy", "iaml_selected_interactions_proxy", "iaml_selected_non_monotone_proxy")]
      gagga_hv = dominated_hypervolume(t(gagga), ref = ref)
      rest = tmp[method != "gagga", c("ce_test", "iaml_selected_features_proxy", "iaml_selected_interactions_proxy", "iaml_selected_non_monotone_proxy")]
      rest_hv = dominated_hypervolume(t(rest), ref = ref)
      data.table(gagga_hv = gagga_hv, rest_hv = rest_hv, repl = repl_, task_id = task_id_)
  })
})

mean_hvs = hvs[, .(mean_gagga_hv = mean(gagga_hv), se_gagga_hv = sd(gagga_hv) / sqrt(.N), mean_rest_hv = mean(rest_hv), se_rest_hv = sd(rest_hv) / sqrt(.N)), by = .(task_id)]

is_dominated = map_dtr(unique(dat$task_id), function(task_id_) {
  map_dtr(unique(dat$repl), function(repl_) {
    tmp = dat[task_id == task_id_ & repl == repl_]
    if (nrow(tmp) != 5L) {
      return(data.table())
    }
    gagga = tmp[method == "gagga"]$pareto[[1L]][, c("ce_test", "iaml_selected_features_proxy", "iaml_selected_interactions_proxy", "iaml_selected_non_monotone_proxy")]
    n_gagga = nrow(gagga)
    map_dtr(c("xgboost", "ebm", "glmnet", "rf"), function(method_) {
      other = tmp[method == method_, c("ce_test", "iaml_selected_features_proxy", "iaml_selected_interactions_proxy", "iaml_selected_non_monotone_proxy")]
      data.table(is_dominated = is_dominated(t(rbind(gagga, other)))[n_gagga + 1L], method = method_, repl = repl_, task_id = task_id_)
    })
  })
})

mean_is_dominated = is_dominated[, .(mean_is_dominated = mean(is_dominated), se_is_dominated = sd(is_dominated) / sqrt(.N)), by = .(method)]


get_nearest = function(gagga, y) {
  stopifnot(nrow(y) == 1L)
  gagga[which.min(apply(gagga, MARGIN = 1L, function(x) sqrt(rowSums((x - y)^2)))), ]
}

performance_change = map_dtr(unique(dat$task_id), function(task_id_) {
  map_dtr(unique(dat$repl), function(repl_) {
    tmp = dat[task_id == task_id_ & repl == repl_]
    if (nrow(tmp) != 5L) {
      return(data.table())
    }
    gagga = tmp[method == "gagga"]$pareto[[1L]][, c("ce_test", "iaml_selected_features_proxy", "iaml_selected_interactions_proxy", "iaml_selected_non_monotone_proxy")]
    map_dtr(c("xgboost", "ebm", "glmnet", "rf"), function(method_) {
      other = tmp[method == method_, c("ce_test", "iaml_selected_features_proxy", "iaml_selected_interactions_proxy", "iaml_selected_non_monotone_proxy")]
      cbind(other - get_nearest(gagga, y = other), method = method_, repl = repl_, task_id = task_id_)
    })
  })
})

mean_performance_change = performance_change[, .(mean_ce_test = mean(ce_test), se_ce_test = sd(ce_test) / sqrt(.N),
                                                 mean_iaml_selected_features_proxy = mean(iaml_selected_features_proxy), se_iaml_selected_features_proxy = sd(iaml_selected_features_proxy) / sqrt(.N),
                                                 mean_iaml_selected_interactions_proxy = mean(iaml_selected_interactions_proxy), se_iaml_selected_interactions_proxy = sd(iaml_selected_interactions_proxy) / sqrt(.N),
                                                 mean_iaml_selected_non_monotone_proxy = mean(iaml_selected_non_monotone_proxy), se_iaml_selected_non_monotone_proxy = sd(iaml_selected_non_monotone_proxy) / sqrt(.N)), by = .(method, task_id)]

ggplot(aes(x = method, y = mean_ce_test), data = mean_performance_change) +
  geom_point() + 
  geom_errorbar(aes(ymin = mean_ce_test - se_ce_test, ymax = mean_ce_test + se_ce_test), width = 0.2) +
  facet_wrap(~ task_id)

ggplot(aes(x = method, y = mean_iaml_selected_features_proxy), data = mean_performance_change) +
  geom_point() + 
  geom_errorbar(aes(ymin = mean_iaml_selected_features_proxy - se_iaml_selected_features_proxy, ymax = mean_iaml_selected_features_proxy + se_iaml_selected_features_proxy), width = 0.2) +
  facet_wrap(~ task_id)

ggplot(aes(x = method, y = mean_iaml_selected_interactions_proxy), data = mean_performance_change) +
  geom_point() + 
  geom_errorbar(aes(ymin = mean_iaml_selected_interactions_proxy - se_iaml_selected_interactions_proxy, ymax = mean_iaml_selected_interactions_proxy + se_iaml_selected_interactions_proxy), width = 0.2) +
  facet_wrap(~ task_id)

ggplot(aes(x = method, y = mean_iaml_selected_non_monotone_proxy), data = mean_performance_change) +
  geom_point() + 
  geom_errorbar(aes(ymin = mean_iaml_selected_non_monotone_proxy - se_iaml_selected_non_monotone_proxy, ymax = mean_iaml_selected_non_monotone_proxy + se_iaml_selected_non_monotone_proxy), width = 0.2) +
  ylim(c(-1, 1)) +
  facet_wrap(~ task_id)

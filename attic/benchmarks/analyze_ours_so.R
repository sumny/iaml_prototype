library(data.table)
library(mlr3misc)
library(bbotk)
library(ggplot2)
library(emoa)

dat = readRDS("iaml_prototype_ours_so.rds")

ref = t(t(c(1, 1, 1, 1)))
#ref = t(t(c(1, 1, 1)))
hvs = map_dtr(unique(dat$task_id), function(task_id_) {
  map_dtr(unique(dat$repl), function(repl_) {
    tmp = dat[task_id == task_id_ & repl == repl_]
      #if (nrow(tmp) != 5L) {
      if (nrow(tmp) < 2L | "gagga" %nin% tmp$method) {
        return(data.table())
      }
      gagga = tmp[method == "gagga"]$pareto[[1L]][, c("ce_test", "iaml_selected_features_proxy", "iaml_selected_interactions_proxy", "iaml_selected_non_monotone_proxy")]
      #gagga = tmp[method == "gagga"]$pareto[[1L]][, c("ce_test", "iaml_selected_features_proxy", "iaml_selected_interactions_proxy")]
      gagga_hv = dominated_hypervolume(t(gagga), ref = ref)
      rest = tmp[method != "gagga", c("ce_test", "iaml_selected_features_proxy", "iaml_selected_interactions_proxy", "iaml_selected_non_monotone_proxy")]
      #rest = tmp[method != "gagga", c("ce_test", "iaml_selected_features_proxy", "iaml_selected_interactions_proxy")]
      rest_hv = dominated_hypervolume(t(rest), ref = ref)
      data.table(gagga_hv = gagga_hv, rest_hv = rest_hv, repl = repl_, task_id = task_id_)
  })
})

mean_hvs = hvs[, .(mean_gagga_hv = mean(gagga_hv), se_gagga_hv = sd(gagga_hv) / sqrt(.N), mean_rest_hv = mean(rest_hv), se_rest_hv = sd(rest_hv) / sqrt(.N)), by = .(task_id)]

mean_hvs = rbind(data.table(task_id = mean_hvs$task_id, mean_hv = mean_hvs$mean_gagga_hv, se_hv = mean_hvs$se_gagga_hv, method = "GAGGA"),
                 data.table(task_id = mean_hvs$task_id, mean_hv = mean_hvs$mean_rest_hv, se_hv = mean_hvs$se_rest_hv, method = "Baselines"))
mean_hvs[, task_id := as.factor(task_id)]

ggplot(aes(x = task_id, y = mean_hv, colour = method), data = mean_hvs) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_hv - se_hv, ymax = mean_hv + se_hv), width = 0.2) +
  labs(y = "Mean Dominated HV", x = "Task ID", colour = "Method") +
  theme_minimal()

is_dominated = map_dtr(unique(dat$task_id), function(task_id_) {
  map_dtr(unique(dat$repl), function(repl_) {
    tmp = dat[task_id == task_id_ & repl == repl_]
    #if (nrow(tmp) != 5L) {
    if (nrow(tmp) < 2L | "gagga" %nin% tmp$method) {
      return(data.table())
    }
    gagga = tmp[method == "gagga"]$pareto[[1L]][, c("ce_test", "iaml_selected_features_proxy", "iaml_selected_interactions_proxy", "iaml_selected_non_monotone_proxy")]
    #gagga = tmp[method == "gagga"]$pareto[[1L]][, c("ce_test", "iaml_selected_features_proxy", "iaml_selected_interactions_proxy")]
    n_gagga = nrow(gagga)
    map_dtr(c("xgboost", "ebm", "glmnet", "rf"), function(method_) {
      other = tmp[method == method_, c("ce_test", "iaml_selected_features_proxy", "iaml_selected_interactions_proxy", "iaml_selected_non_monotone_proxy")]
      #other = tmp[method == method_, c("ce_test", "iaml_selected_features_proxy", "iaml_selected_interactions_proxy")]
      if (nrow(other) == 0L) {
        return(data.table())
      }
      data.table(is_dominated = is_dominated(t(rbind(gagga, other)))[n_gagga + 1L], method = method_, repl = repl_, task_id = task_id_)
    })
  })
})

mean_is_dominated = is_dominated[, .(mean_is_dominated = mean(is_dominated), se_is_dominated = sd(is_dominated) / sqrt(.N)), by = .(method)]

get_nearest = function(gagga, y, objective = "ce_test") {
  stopifnot(nrow(y) == 1L)
  gagga[which.min(sqrt((gagga[, ..objective][[1L]] - y[[objective]][[1L]]) ^ 2)), ]
}

performance_change = map_dtr(unique(dat$task_id), function(task_id_) {
  map_dtr(unique(dat$repl), function(repl_) {
    tmp = dat[task_id == task_id_ & repl == repl_]
    #if (nrow(tmp) != 5L) {
    if (nrow(tmp) < 2L | "gagga" %nin% tmp$method) {
      return(data.table())
    }
    gagga = tmp[method == "gagga"]$pareto[[1L]][, c("ce_test", "iaml_selected_features_proxy", "iaml_selected_interactions_proxy", "iaml_selected_non_monotone_proxy")]
    map_dtr(c("xgboost", "ebm", "glmnet", "rf"), function(method_) {
      other = tmp[method == method_, c("ce_test", "iaml_selected_features_proxy", "iaml_selected_interactions_proxy", "iaml_selected_non_monotone_proxy")]
      if (nrow(other) == 0L) {
        return(data.table())
      }
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
  facet_wrap(~ task_id)

mean_mean_performance_change = mean_performance_change[, .(mean_ce_test = mean(mean_ce_test), se_ce_test = sd(mean_ce_test) / sqrt(.N),
                                                           mean_iaml_selected_features_proxy = mean(mean_iaml_selected_features_proxy), se_iaml_selected_features_proxy = sd(mean_iaml_selected_features_proxy) / sqrt(.N),
                                                           mean_iaml_selected_interactions_proxy = mean(mean_iaml_selected_interactions_proxy), se_iaml_selected_interactions_proxy = sd(mean_iaml_selected_interactions_proxy) / sqrt(.N),
                                                           mean_iaml_selected_non_monotone_proxy = mean(mean_iaml_selected_non_monotone_proxy), se_iaml_selected_non_monotone_proxy = sd(mean_iaml_selected_non_monotone_proxy) / sqrt(.N)), by = .(method)]

best = map_dtr(unique(dat$task_id), function(task_id_) {
  map_dtr(unique(dat$repl), function(repl_) {
    tmp = dat[task_id == task_id_ & repl == repl_]
      #if (nrow(tmp) != 5L) {
      if (nrow(tmp) < 2L | "gagga" %nin% tmp$method) {
        return(data.table())
      }
      gagga = tmp[method == "gagga"]$pareto[[1L]][, c("ce_test", "iaml_selected_features_proxy", "iaml_selected_interactions_proxy", "iaml_selected_non_monotone_proxy")]
      gagga = gagga[which.min(ce_test), ]
      gagga[, method := "gagga"]
      rest = tmp[method != "gagga", c("ce_test", "iaml_selected_features_proxy", "iaml_selected_interactions_proxy", "iaml_selected_non_monotone_proxy", "method")]
      result = rbind(gagga, rest)
      result[, repl := repl_]
      result[, task_id := task_id_]
      result
  })
})

mean_best = best[, .(mean_ce_test = mean(ce_test), se_ce_test = sd(ce_test) / sqrt(.N),
                     mean_iaml_selected_features_proxy = mean(iaml_selected_features_proxy), se_iaml_selected_features_proxy = sd(iaml_selected_features_proxy) / sqrt(.N),
                     mean_iaml_selected_interactions_proxy = mean(iaml_selected_interactions_proxy), se_iaml_selected_interactions_proxy = sd(iaml_selected_interactions_proxy) / sqrt(.N),
                     mean_iaml_selected_non_monotone_proxy = mean(iaml_selected_non_monotone_proxy), se_iaml_selected_non_monotone_proxy = sd(iaml_selected_non_monotone_proxy) / sqrt(.N)), by = .(method, task_id)]
mean_best[, mf := as.numeric(as.factor(method)), by = .(task_id)]
mean_best[, text_space := mf + 0.5]
mean_best[method == "xgboost", text_space := mf - 0.5]
mean_best[, method := as.factor(method)]

ggplot(aes(x = method, y = mean_ce_test, colour = method), data = mean_best) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_ce_test - se_ce_test, ymax = mean_ce_test + se_ce_test), width = 0.2) +
  geom_text(aes(x = text_space, y = mean_ce_test, label = paste0(format(round(mean_iaml_selected_features_proxy, 2), nsmall = 2), "/", format(round(mean_iaml_selected_interactions_proxy, 2), nsmall = 2), "/", format(round(mean_iaml_selected_non_monotone_proxy, 2), nsmall = 2))), size = 3) +
  facet_wrap(~ task_id, scales = "free") +
  labs(y = "Mean Classification Error", x = "", colour = "Method") +
  theme_minimal()

mean_mean_best = mean_best[, .(mean_ce_test = mean(mean_ce_test), se_ce_test = sd(mean_ce_test) / sqrt(.N),
                               mean_iaml_selected_features_proxy = mean(mean_iaml_selected_features_proxy), se_iaml_selected_features_proxy = sd(mean_iaml_selected_features_proxy) / sqrt(.N),
                               mean_iaml_selected_interactions_proxy = mean(mean_iaml_selected_interactions_proxy), se_iaml_selected_interactions_proxy = sd(mean_iaml_selected_interactions_proxy) / sqrt(.N),
                               mean_iaml_selected_non_monotone_proxy = mean(mean_iaml_selected_non_monotone_proxy), se_iaml_selected_non_monotone_proxy = sd(mean_iaml_selected_non_monotone_proxy) / sqrt(.N)), by = .(method)]


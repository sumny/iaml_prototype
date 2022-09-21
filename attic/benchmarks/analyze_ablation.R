library(data.table)
library(mlr3misc)
library(bbotk)
library(ggplot2)
library(emoa)
library(pammtools)
library(scmamp)

gagga_dhv = readRDS("gagga_dhv_majority_vote.rds")
xgboost_mo_dhv = readRDS("xgboost_mo_dhv_majority_vote.rds")
anytime_dhvs = rbind(gagga_dhv, xgboost_mo_dhv)

mean_anytime_hvs = anytime_dhvs[, .(mean_anytime_hv = mean(hv_val), se_anytime_hv = sd(hv_val) / sqrt(.N)), by = .(iteration, method, task_id)]

g = ggplot(aes(x = iteration, y = mean_anytime_hv, colour = method, fill = method), data = mean_anytime_hvs) +
  geom_step() +
  geom_stepribbon(aes(ymin = mean_anytime_hv - se_anytime_hv, ymax = mean_anytime_hv + se_anytime_hv), colour = NA, alpha = 0.25) +
  labs(y = "Mean HVI", x = "Iteration", colour = "Method", fill = "Method") +
  facet_wrap(~ task_id, scales = "free", nrow = 5, ncol = 4) +
  theme_minimal()

ggsave("plots/gagga_xgboost_ablation_all.png", plot = g, width = 12, height = 8)

mean_mean_anytime_hvs = mean_anytime_hvs[, .(mean_anytime_hv = mean(mean_anytime_hv), se_anytime_hv = sd(mean_anytime_hv) / sqrt(.N)), by = .(iteration, method)]

g = ggplot(aes(x = iteration, y = mean_anytime_hv, colour = method, fill = method), data = mean_mean_anytime_hvs) +
  geom_step() +
  geom_stepribbon(aes(ymin = mean_anytime_hv - se_anytime_hv, ymax = mean_anytime_hv + se_anytime_hv), colour = NA, alpha = 0.25) +
  labs(y = "Mean HVI", x = "Iteration", colour = "Method", fill = "Method") +
  theme_minimal()

ggsave("plots/gagga_xgboost_ablation_average.png", plot = g, width = 6, height = 4)

wst_val = - as.matrix(dcast(mean_anytime_hvs[iteration == 130, ], task_id ~ method, value.var = "mean_anytime_hv")[, -1L])
wilcoxonSignedTest(wst_val[, 1L], wst_val[, 2L])

diff_val = wst_val[, 1L] - wst_val[, 2L]
any(diff_val == 0)
gagga_wins = sum(diff_val > 0)
binom.test(gagga_wins, n = length(diff_val))

###

gagga = readRDS("iaml_prototype_ours_so.rds")[method == "gagga"]
xgboost_mo = readRDS("iaml_prototype_ablation_xgboost_mo.rds")
dat = rbind(gagga, xgboost_mo, fill = TRUE)
majority = readRDS("iaml_prototype_majority_vote.rds")

anytime_hvs = map_dtr(unique(dat$task_id), function(task_id_) {
  #if (nrow(dat[task_id == task_id_]) != 20L) {
  #  return(data.table())
  #}
  map_dtr(unique(dat$repl), function(repl_) {
    tmp = dat[task_id == task_id_ & repl == repl_]
    if (nrow(tmp) != 2L) {
      return(data.table())
    }
    hvs_gagga = tmp[method == "gagga", "dhv_val_anytime"][[1L]][[1L]]
    hvs_gagga[, method := "gagga"]
    hvs_xgboost = tmp[method == "xgboost_mo", "dhv_val_anytime"][[1L]][[1L]]
    hvs_xgboost[, method := "xgboost"]
    hvs = rbind(hvs_gagga, hvs_xgboost)
    hvs[, repl := repl_]
    hvs[, task_id := task_id_]
    hvs
  })
})

mean_anytime_hvs = anytime_hvs[, .(mean_anytime_hv = mean(dhv_val), se_anytime_hv = sd(dhv_val) / sqrt(.N)), by = .(iteration, method, task_id)]

g = ggplot(aes(x = iteration, y = mean_anytime_hv, colour = method, fill = method), data = mean_anytime_hvs) +
  geom_step() +
  geom_stepribbon(aes(ymin = mean_anytime_hv - se_anytime_hv, ymax = mean_anytime_hv + se_anytime_hv), colour = NA, alpha = 0.25) +
  labs(y = "Mean Dominated HV", x = "Iteration", colour = "Method", fill = "Method") +
  facet_wrap(~ task_id, scales = "free", nrow = 5, ncol = 4) +
  theme_minimal()

ggsave("plots/gagga_xgboost_ablation_all.png", plot = g, width = 12, height = 8)

mean_mean_anytime_hvs = mean_anytime_hvs[, .(mean_anytime_hv = mean(mean_anytime_hv), se_anytime_hv = sd(mean_anytime_hv) / sqrt(.N)), by = .(iteration, method)]

g = ggplot(aes(x = iteration, y = mean_anytime_hv, colour = method, fill = method), data = mean_mean_anytime_hvs) +
  geom_step() +
  geom_stepribbon(aes(ymin = mean_anytime_hv - se_anytime_hv, ymax = mean_anytime_hv + se_anytime_hv), colour = NA, alpha = 0.25) +
  labs(y = "Mean Dominated HV", x = "Iteration", colour = "Method", fill = "Method") +
  theme_minimal()

ggsave("plots/gagga_xgboost_ablation_average.png", plot = g, width = 6, height = 4)

mean_dhvs_val = dat[, .(mean_dhv_val = mean(dhv_val), se_dhv_val = sd(dhv_val) / sqrt(.N)), by = .(method, task_id)]

mean_mean_dhvs_val = mean_dhvs_val[, .(mean_dhv_val = mean(mean_dhv_val), se_dhv_val = sd(mean_dhv_val) / sqrt(.N)), by = .(method)]

mean_dhvs_test = dat[, .(mean_dhv_test = mean(dhv_test), se_dhv_test = sd(dhv_test) / sqrt(.N)), by = .(method, task_id)]

mean_mean_dhvs_test = mean_dhvs_test[, .(mean_dhv_test = mean(mean_dhv_test), se_dhv_test = sd(mean_dhv_test) / sqrt(.N)), by = .(method)]

### validation dhv including majority vote baseline as a point

ref = t(t(c(1, 1, 1, 1)))
ys = c("classif.ce", "iaml_selected_features_proxy", "iaml_selected_interactions_proxy", "iaml_selected_non_monotone_proxy")

dhvs_val = map_dtr(unique(dat$task_id), function(task_id_) {
  #if (nrow(dat[task_id == task_id_]) != 20L) {
  #  return(data.table())
  #}
  map_dtr(unique(dat$repl), function(repl_) {
    tmp = dat[task_id == task_id_ & repl == repl_]
    if (nrow(tmp) != 2L) {
      return(data.table())
    }
    gagga = tmp[method == "gagga", "tuning_pareto"][[1L]][[1L]][, ..ys]
    gagga = rbind(gagga, majority[task_id == task_id_ & repl == repl_, ..ys])
    hv_gagga = if(nrow(gagga) > 1) dominated_hypervolume(t(gagga), ref = ref) else 0
    xgboost = tmp[method == "xgboost_mo", "tuning_pareto"][[1L]][[1L]][, ..ys]
    xgboost = rbind(xgboost, majority[task_id == task_id_ & repl == repl_, ..ys])
    hv_xgboost = if (nrow(xgboost) > 0) dominated_hypervolume(t(xgboost), ref = ref) else 0
    res = data.table(dhv_val = c(hv_gagga, hv_xgboost), method = c("gagga", "xgboost"))
    res[, repl := repl_]
    res[, task_id := task_id_]
  })
})

mean_dhvs_val = dhv_val[, .(mean_dhv_val = mean(dhv_val), se_dhv_val = sd(dhv_val) / sqrt(.N)), by = .(method, task_id)]

ggplot(aes(x = as.factor(task_id), y = mean_dhv_val, colour = method), data = mean_dhvs_val) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_dhv_val - se_dhv_val, ymax = mean_dhv_val + se_dhv_val))

mean_mean_dhvs_val = mean_dhvs_val[, .(mean_dhv_val = mean(mean_dhv_val), se_dhv_val = sd(mean_dhv_val) / sqrt(.N)), by = .(method)]

wst_val = as.matrix(dcast(mean_dhvs_val, task_id ~ method, value.var = "mean_dhv_val")[, -1L])
wilcoxonSignedTest(wst_val[, 1L], wst_val[, 2L])

diff_val = wst_val[, 1L] - wst_val[, 2L]
any(diff_val == 0)
gagga_wins = sum(diff_val > 0)
binom.test(gagga_wins, n = length(diff_val))

### test dhv including majority vote baseline as a point

ys = c("ce_test", "iaml_selected_features_proxy", "iaml_selected_interactions_proxy", "iaml_selected_non_monotone_proxy")

dhvs_test = map_dtr(unique(dat$task_id), function(task_id_) {
  #if (nrow(dat[task_id == task_id_]) != 20L) {
  #  return(data.table())
  #}
  map_dtr(unique(dat$repl), function(repl_) {
    tmp = dat[task_id == task_id_ & repl == repl_]
    if (nrow(tmp) != 2L) {
      return(data.table())
    }
    gagga = tmp[method == "gagga", "pareto"][[1L]][[1L]][, ..ys]
    gagga = rbind(gagga, majority[task_id == task_id_ & repl == repl_, ..ys])
    hv_gagga = if(nrow(gagga) > 1) dominated_hypervolume(t(gagga), ref = ref) else 0
    xgboost = tmp[method == "xgboost_mo", "pareto"][[1L]][[1L]][, ..ys]
    xgboost = rbind(xgboost, majority[task_id == task_id_ & repl == repl_, ..ys])
    hv_xgboost = if (nrow(xgboost) > 0) dominated_hypervolume(t(xgboost), ref = ref) else 0
    res = data.table(dhv_test = c(hv_gagga, hv_xgboost), method = c("gagga", "xgboost"))
    res[, repl := repl_]
    res[, task_id := task_id_]
  })
})

mean_dhvs_test = dhvs_test[, .(mean_dhv_test = mean(dhv_test), se_dhv_test = sd(dhv_test) / sqrt(.N)), by = .(method, task_id)]

ggplot(aes(x = as.factor(task_id), y = mean_dhv_test, colour = method), data = mean_dhvs_test) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_dhv_test - se_dhv_test, ymax = mean_dhv_test + se_dhv_test))

mean_mean_dhvs_test = mean_dhvs_test[, .(mean_dhv_test = mean(mean_dhv_test), se_dhv_test = sd(mean_dhv_test) / sqrt(.N)), by = .(method)]

wst_test = as.matrix(dcast(mean_dhvs_test, task_id ~ method, value.var = "mean_dhv_test")[, -1L])
wilcoxonSignedTest(wst_test[, 1L], wst_test[, 2L])

diff_test = wst_test[, 1L] - wst_test[, 2L]
any(diff_test == 0)
gagga_wins = sum(diff_test > 0)
binom.test(gagga_wins, n = length(diff_test))

### mutation vs. crossover (detectors TRUE)

gagga_ablation = readRDS("iaml_prototype_ablation_gagga.rds")
dat = rbind(gagga, gagga_ablation, fill = TRUE)

# detectors FALSE
anytime_hvs = map_dtr(unique(dat$task_id), function(task_id_) {
  #if (nrow(dat[task_id == task_id_]) != 20L) {
  #  return(data.table())
  #}
  map_dtr(unique(dat$repl), function(repl_) {
    tmp = dat[task_id == task_id_ & repl == repl_]
    if ("gagga" %nin% tmp$method) {
      return(data.table())
    }
    hvs_gagga = tmp[method == "gagga", "dhv_val_anytime"][[1L]][[1L]]
    hvs_gagga[, method := "gagga"]
    hvs_ablation = tmp[is.na(random) & detectors == FALSE & method != "gagga"]
    if (nrow(hvs_ablation) != 4L) {
      return(data.table())
    }
    hvs_ablation[, modification := paste0(crossover, "_", mutation)]
    hvs_0 = hvs_ablation[modification == "TRUE_TRUE", "dhv_val_anytime"][[1L]][[1L]]
    hvs_0[, method := "TRUE_TRUE"]
    hvs_1 = hvs_ablation[modification == "FALSE_TRUE", "dhv_val_anytime"][[1L]][[1L]]
    hvs_1[, method := "FALSE_TRUE"]
    hvs_2 = hvs_ablation[modification == "TRUE_FALSE", "dhv_val_anytime"][[1L]][[1L]]
    hvs_2[, method := "TRUE_FALSE"]
    hvs_3 = hvs_ablation[modification == "FALSE_FALSE", "dhv_val_anytime"][[1L]][[1L]]  # no GGA, only GA
    hvs_3[, method := "FALSE_FALSE"]
    hvs = rbind(hvs_gagga, hvs_0, hvs_1, hvs_2, hvs_3)
    hvs[, repl := repl_]
    hvs[, task_id := task_id_]
    hvs
  })
})

# detectors TRUE
anytime_hvs = map_dtr(unique(dat$task_id), function(task_id_) {
  #if (nrow(dat[task_id == task_id_]) != 20L) {
  #  return(data.table())
  #}
  map_dtr(unique(dat$repl), function(repl_) {
    tmp = dat[task_id == task_id_ & repl == repl_]
    if ("gagga" %nin% tmp$method) {
      return(data.table())
    }
    hvs_gagga = tmp[method == "gagga", "dhv_val_anytime"][[1L]][[1L]]
    hvs_gagga[, method := "gagga"]
    hvs_ablation = tmp[is.na(random) & detectors == TRUE & method != "gagga"]
    if (nrow(hvs_ablation) != 3L) {
      return(data.table())
    }
    hvs_ablation[, modification := paste0(crossover, "_", mutation)]
    hvs_1 = hvs_ablation[modification == "FALSE_TRUE", "dhv_val_anytime"][[1L]][[1L]]
    hvs_1[, method := "FALSE_TRUE"]
    hvs_2 = hvs_ablation[modification == "TRUE_FALSE", "dhv_val_anytime"][[1L]][[1L]]
    hvs_2[, method := "TRUE_FALSE"]
    hvs_3 = hvs_ablation[modification == "FALSE_FALSE", "dhv_val_anytime"][[1L]][[1L]]  # no GGA, only GA
    hvs_3[, method := "FALSE_FALSE"]
    hvs = rbind(hvs_gagga, hvs_1, hvs_2, hvs_3)
    hvs[, repl := repl_]
    hvs[, task_id := task_id_]
    hvs
  })
})

# random TRUE
anytime_hvs = map_dtr(unique(dat$task_id), function(task_id_) {
  #if (nrow(dat[task_id == task_id_]) != 20L) {
  #  return(data.table())
  #}
  map_dtr(unique(dat$repl), function(repl_) {
    tmp = dat[task_id == task_id_ & repl == repl_]
    if ("gagga" %nin% tmp$method) {
      return(data.table())
    }
    hvs_gagga = tmp[method == "gagga", "dhv_val_anytime"][[1L]][[1L]]
    hvs_gagga[, method := "gagga"]
    hvs_ablation = tmp[!is.na(random) & method != "gagga"]
    if (nrow(hvs_ablation) != 3L) {
      return(data.table())
    }
    hvs_ablation[, modification := paste0(detectors, "_", random)]
    hvs_1 = hvs_ablation[modification == "TRUE_TRUE", "dhv_val_anytime"][[1L]][[1L]]
    hvs_1[, method := "TRUE_TRUE"]
    hvs_2 = hvs_ablation[modification == "FALSE_TRUE", "dhv_val_anytime"][[1L]][[1L]]
    hvs_2[, method := "FALSE_TRUE"]
    hvs_3 = hvs_ablation[modification == "FALSE_FALSE", "dhv_val_anytime"][[1L]][[1L]]  # no GGA, only GA
    hvs_3[, method := "FALSE_FALSE"]
    hvs = rbind(hvs_gagga, hvs_1, hvs_2, hvs_3)
    hvs[, repl := repl_]
    hvs[, task_id := task_id_]
    hvs
  })
})

mean_anytime_hvs = anytime_hvs[, .(mean_anytime_hv = mean(dhv_val), se_anytime_hv = sd(dhv_val) / sqrt(.N)), by = .(iteration, method, task_id)]

g = ggplot(aes(x = iteration, y = mean_anytime_hv, colour = method, fill = method), data = mean_anytime_hvs[iteration > 30]) +
  geom_step() +
  #geom_stepribbon(aes(ymin = mean_anytime_hv - se_anytime_hv, ymax = mean_anytime_hv + se_anytime_hv), colour = NA, alpha = 0.25) +
  labs(y = "Mean Dominated HV", x = "Iteration", colour = "Method", fill = "Method") +
  facet_wrap(~ task_id, scales = "free", nrow = 5, ncol = 4) +
  theme_minimal()

mean_mean_anytime_hvs = mean_anytime_hvs[, .(mean_anytime_hv = mean(mean_anytime_hv), se_anytime_hv = sd(mean_anytime_hv) / sqrt(.N)), by = .(iteration, method)]

g = ggplot(aes(x = iteration, y = mean_anytime_hv, colour = method, fill = method), data = mean_mean_anytime_hvs) +
  geom_step() +
  #geom_stepribbon(aes(ymin = mean_anytime_hv - se_anytime_hv, ymax = mean_anytime_hv + se_anytime_hv), colour = NA, alpha = 0.25) +
  labs(y = "Mean Dominated HV", x = "Iteration", colour = "Method", fill = "Method") +
  theme_minimal()


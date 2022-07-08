library(data.table)
library(mlr3misc)
library(ggplot2)
library(pammtools)

dat = readRDS("iaml_prototype.rds")
dat = dat[actually_used == TRUE, ]
ys = c("classif.ce", "iaml_selected_features", "iaml_selected_interactions", "iaml_selected_non_monotone")
n_features = data.table(task_id = c(40981, 1169, 1464, 1489, 4135, 1485, 41144), n = c(22, 11, 4, 5, 18, 500, 259))
res = map_dtr(unique(dat$task_id), function(task_id_) {
  map_dtr(unique(dat$repl), function(repl_) {
    tmp = dat[task_id == task_id_ & repl == repl_]
    tmp[, classif.ce := (classif.ce - min(classif.ce)) / (max(classif.ce) - min(classif.ce))]
    tmp[, iaml_selected_features := (iaml_selected_features - min(iaml_selected_features)) / (max(iaml_selected_features) - min(iaml_selected_features))]
    tmp[, iaml_selected_interactions := (iaml_selected_interactions - min(iaml_selected_interactions)) / (max(iaml_selected_interactions) - min(iaml_selected_interactions))]
    tmp[, iaml_selected_non_monotone := (iaml_selected_non_monotone - min(iaml_selected_non_monotone)) / (max(iaml_selected_non_monotone) - min(iaml_selected_non_monotone))]
    if (nrow(tmp) == 2500L) {
      nadir = apply(tmp[, ..ys], 2, function(x) max(x) + 1)
      best = tmp[, ..ys]
      best = best[!bbotk::is_dominated(t(best)), ]
      best_hv = emoa::dominated_hypervolume(t(best), ref = nadir)
      map_dtr(unique(tmp$batch_nr), function(batch_nr_) {
        eawm_y = tmp[method == "eawm" & batch_nr <= batch_nr_, ..ys]
        eawm_y = eawm_y[!bbotk::is_dominated(t(eawm_y)), ]
        eaw_y = tmp[method == "eaw" & batch_nr <= batch_nr_, ..ys]
        eaw_y = eaw_y[!bbotk::is_dominated(t(eaw_y)), ]
        ea_y = tmp[method == "ea" & batch_nr <= batch_nr_, ..ys]
        ea_y = ea_y[!bbotk::is_dominated(t(ea_y)), ]
        rsw_y = tmp[method == "rsw" & batch_nr <= batch_nr_, ..ys]
        rsw_y = rsw_y[!bbotk::is_dominated(t(rsw_y)), ]
        rs_y = tmp[method == "rs" & batch_nr <= batch_nr_, ..ys]
        rs_y = rs_y[!bbotk::is_dominated(t(rs_y)), ]

        eawm_hv = best_hv - emoa::dominated_hypervolume(t(eawm_y), ref = nadir)
        eaw_hv = best_hv - emoa::dominated_hypervolume(t(eaw_y), ref = nadir)
        ea_hv = best_hv - emoa::dominated_hypervolume(t(ea_y), ref = nadir)
        rsw_hv = best_hv - emoa::dominated_hypervolume(t(rsw_y), ref = nadir)
        rs_hv = best_hv - emoa::dominated_hypervolume(t(rs_y), ref = nadir)
        hvs = rbind(data.table(hv = eawm_hv, method = "eawm"), data.table(hv = eaw_hv, method = "eaw"), data.table(hv = ea_hv, method = "ea"), data.table(hv = rsw_hv, method = "rsw"), data.table(hv = rs_hv, method = "rs"))
        hvs[, batch_nr := batch_nr_]
        hvs[, repl := repl_]
        hvs[, task_id := task_id_]
        hvs
      })
    }
  })
})

agg = res[, .(m_hv = mean(hv), s_hv = sd(hv) / sqrt(.N)), by = .(task_id, batch_nr, method)]
agg = merge(agg, n_features, by = "task_id")
agg[, task_id := paste0(task_id, ": ", n)]

g = ggplot(aes(x = batch_nr, y = m_hv, colour = method, fill = method), data = agg) +
  scale_y_log10() +
  geom_step() + labs(y = "Diff Dom HV", x = "Iteration") +
  geom_stepribbon(aes(ymin = m_hv - s_hv, ymax = m_hv + s_hv), colour = NA, alpha = 0.3) +
  facet_wrap(~ task_id, scales = "free") +
  geom_vline(xintercept = 30) +
  theme_minimal()
ggsave(file = "bench_actually_true.png", plot = g)

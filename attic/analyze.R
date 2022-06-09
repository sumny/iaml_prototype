library(data.table)
library(mlr3misc)
library(ggplot2)
library(pammtools)

dat = readRDS("iaml_prototype.rds")
ys = c("classif.ce", "iaml_selected_features", "iaml_selected_interactions", "iaml_selected_non_monotone")
n_features = list("40981" = 43, "1169" = 615, "1464" = 5, "1489" = 6, "4135" = 4508, "1485" = 501, "41144" = 260)
res = map_dtr(unique(dat$task_id), function(task_id_) {
  n = n_features[[as.character(task_id_)]]
  map_dtr(unique(dat$repl), function(repl_) {
    tmp = dat[task_id == task_id_ & repl == repl_]
    tmp[, iaml_selected_features := iaml_selected_features / n]
    tmp[, iaml_selected_interactions := iaml_selected_interactions / n ^ 2]
    tmp[, iaml_selected_non_monotone := iaml_selected_non_monotone / n]
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

g = ggplot(aes(x = batch_nr, y = m_hv, colour = method, fill = method), data = agg[batch_nr >= 30L]) +
  geom_step() + labs(y = "Diff Dom HV", x = "Iteration") +
  #geom_stepribbon(aes(ymin = m_hv - s_hv, ymax = m_hv + s_hv), colour = NA, alpha = 0.3) +
  facet_wrap(~ task_id, scales = "free")
ggsave(file = "test_bench.png", plot = g)

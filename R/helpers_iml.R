# qlogis with some tolerance on 0 and 1
qlogisc = function(p) {
  p[p == 1] = 1 - .Machine$double.neg.eps
  p[p == 0] = .Machine$double.eps
  stats::qlogis(p)
}

# work on logits in the case of binary classification
create_predict_fun_custom = function(model, task, predict.fun = NULL, type = NULL) {
  if (!requireNamespace("mlr3")) {
    "Please install the mlr3 package."
  }
  if (task == "classification") {
    function(newdata) {
      if (model$predict_type == "response") {
        pred = stats::predict(model, newdata = newdata)
        factor_to_dataframe(pred)
      } else {
        tmp = data.table(stats::predict(model, newdata = newdata, predict_type = "prob"), check.names = FALSE)
        # NOTE: work on logits
        cols = colnames(tmp)
        tmp[, (cols) := lapply(.SD, function(x) qlogisc(x)), .SDcols = cols]
        as.data.frame(tmp)
      }
    }
  } else if (task == "regression") {
    function(newdata) {
      data.frame(stats::predict(model, newdata = newdata))
    }
  } else {
    stop(sprintf("Task type '%s' not supported", task))
  }
}

factor_to_dataframe = function(fac) {
  check_vector(fac)
  res = data.frame(stats::model.matrix(~ fac - 1, data.frame(fac = fac), sep = ":"))
  colnames(res) = substring(colnames(res), 4)
  res
}


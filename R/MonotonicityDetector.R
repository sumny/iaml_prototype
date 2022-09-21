# Monotonicity Detection via AIC comparison of monotone restricted splines - scam
# we assume integer or numeric features due to later using xgboost
# logicals must be converted to integers
MonotonicityDetector = R6Class("MonotonicityDetectorDetector",
  public = list(
    initialize = function(task) {
      assert_task(task, feature_types = c("integer", "numeric"))
      feature_names = task$feature_names
      feature_types = task$feature_types
      y = task$data(cols = task$target_names)[[1L]]  # regardless of regr or classif
      if (task$task_type == "classif") {
        assert_true(length(unique(y)) == 2L)  # FIXME: tweak this for multiclass, multinom() does not work
      }
      n_features = length(feature_names)
      self$task = task
      self$classification = task$task_type == "classif"
      self$data = task$data()
      self$n_features = n_features
      self$feature_names = feature_names
      self$feature_types = feature_types
      self$y_name = task$target_names
      self$aic_table = data.table(feature_name = feature_names, aic_increasing = numeric(n_features), aic_decreasing = numeric(n_features))
      self$unconstrained_weight_table = data.table(feature_name = feature_names, unconstrained_weight = numeric(n_features))
    },

    task = NULL,
    classification = NULL,
    data = NULL,
    n_features = NULL,
    feature_names = NULL,
    feature_types = NULL,
    y_name = NULL,
    aic_table = NULL,
    unconstrained_weight_table = NULL,

    compute_aics = function() {
      pb = progress_bar$new(format = "Fitting scams [:bar] :percent eta: :eta", total = length(self$feature_names))
      for (x_name in self$feature_names) {
        pb$tick()
        private$.compute_aic(x_name)
      }
    },

    get_sign = function(feature_name) {
      x_name = assert_choice(feature_name, choices = self$feature_names)
      aics = self$aic_table[feature_name == x_name, c("aic_increasing", "aic_decreasing")]
      if (which.min(aics) == 1L) {
        1L
      } else if (which.min(aics == 2L)) {
        -1L
      }
    },

    compute_unconstrained_weights = function() {
      for (x_name in self$feature_names) {
        private$.compute_unconstrained_weight(x_name)
      }
    }
  ),

  #active = list(
  #),
  private = list(
    .compute_aic = function(x_name, fraction = 2/3, seed = NULL) {
      control = scam.control(maxit = 10L, devtol.fit = 1e-4, steptol.fit = 1e-4)  # quite permissive fitting parameters; we do not need to be that precise
      fam = if (self$classification) binomial() else gaussian()
      form = as.formula(paste0(self$y_name, " ~ ", "s(", x_name, ")"))
      # FIXME: changed 16.08.2022 after benchmarks
      data = self$data[sample(seq_len(.N), size = ceiling(2/3 * .N), replace = TRUE), ]
      s = tryCatch(withTimeout(scam(formula = form, family = fam, data = self$data, control = control), elapsed = 60L), error = function(ec) NULL)
      form_inc = as.formula(paste0(self$y_name, " ~ ", "s(", x_name, ", bs = 'mpi')"))
      s_inc = tryCatch(withThimeout(scam(formula = form_inc, family = fam, data = self$data, control = control), elapsed = 60L), error = function(ec) NULL)
      form_decr = as.formula(paste0(self$y_name, " ~ ", "s(", x_name, ", bs = 'mpd')"))
      s_decr = tryCatch(withTimeout(scam(formula = form_decr, family = fam, data = self$data, control = control), elapsed = 60L), error = function(ec) NULL)

      aic_ = tryCatch(AIC(s), error = function(ec) Inf)
      aic_increasing_ = tryCatch(AIC(s_inc), error = function(ec) Inf)
      aic_decreasing_ = tryCatch(AIC(s_decr), error = function(ec) Inf)

      self$aic_table[feature_name == x_name, aic := aic_]
      self$aic_table[feature_name == x_name, aic_increasing := aic_increasing_]
      self$aic_table[feature_name == x_name, aic_decreasing := aic_decreasing_]
    },

    .compute_unconstrained_weight = function(x_name) {
      aics = self$aic_table[feature_name == x_name, c("aic", "aic_increasing", "aic_decreasing")]
      sign = self$get_sign(x_name)
      aic = aics[["aic"]]
      aic_constrained = if (sign == 1L) aics[["aic_increasing"]] else if (sign == -1L) aics[["aic_decreasing"]]
      # if aic_constrained == aic, sampling weight for unconstraint is set to at least 0.2 (custom lower bound)
      delta = max(0, aic_constrained - aic)
      p = (exp(- delta / 2))  # probability of constrained model minimizing aic Burnham & Anderson (2004)
      p_adjusted = p * 0.8
      tmp = 1 - p_adjusted  # sampling weight for unconstrained model
      if (!is.finite(tmp) | is.nan(tmp)) tmp = 1  # if something goes wrong or models were not fit, opt for unconstrained model
      self$unconstrained_weight_table[feature_name == x_name, unconstrained_weight := tmp]
    }
  )
)

########## test
if (FALSE) {
  task = tsk("spam")
  detector = MonotonicityDetector$new(task)
  detector$compute_aics()
  detector$aic_table
  detector$get_sign("address")
  detector$compute_unconstrained_weights()
  detector$unconstrained_weight_table
}

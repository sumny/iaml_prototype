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
    },

    task = NULL,
    classification = NULL,
    data = NULL,
    n_features = NULL,
    feature_names = NULL,
    feature_types = NULL,
    y_name = NULL,
    aic_table = NULL,

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

    get_unconstrained_weight = function(feature_name) {
      x_name = assert_choice(feature_name, choices = self$feature_names)
      aics = self$aic_table[feature_name == x_name, c("aic", "aic_increasing", "aic_decreasing")]
      sign = self$get_sign(x_name)
      aic = aics[["aic"]]
      aic_constrained = if (sign == 1L) aics[["aic_increasing"]] else if (sign == -1L) aics[["aic_decreasing"]]
      1 - min(1, max(0, (aic / aic_constrained))) # relative change in aic when moving from constrained to unconstrained; the larger the better; this is used for the sampling of monotonicity attributes
    }
  ),

  #active = list(
  #),
  private = list(
    .compute_aic = function(x_name) {
      control = scam.control(maxit = 100L, devtol.fit = 1e-5, steptol.fit = 1e-5)
      fam = if (self$classification) binomial() else gaussian()
      form = as.formula(paste0(self$y_name, " ~ ", "s(", x_name, ")"))
      s = scam(formula = form, family = fam, data = self$data, control = control)
      form_inc = as.formula(paste0(self$y_name, " ~ ", "s(", x_name, ", bs = 'mpi')"))
      s_inc = scam(formula = form_inc, family = fam, data = self$data, control = control)
      form_decr = as.formula(paste0(self$y_name, " ~ ", "s(", x_name, ", bs = 'mpd')"))
      s_decr = scam(formula = form_decr, family = fam, data = self$data, control = control)
      self$aic_table[feature_name == x_name, aic := AIC(s)]
      self$aic_table[feature_name == x_name, aic_increasing := AIC(s_inc)]
      self$aic_table[feature_name == x_name, aic_decreasing := AIC(s_decr)]
    }
  )
)

########## test
if (FALSE) {
  task = tsk("spam")
  detector = MonotonicityDetector$new(task)
  detector$compute_aics()
  detector$get_sign("address")
}

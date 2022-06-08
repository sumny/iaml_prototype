# Monotonicity Detection via AIC comarison of monotone restricted splines - scam

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
      for (x_name in self$feature_names) {
        private$.compute_aic(x_name)
      }
    },

    get_sign = function(feature_name) {
      x_name = assert_choice(feature_name, choices = self$feature_names)
      aics = self$aic_table[feature_name ==  x_name, c("aic_increasing", "aic_decreasing")]
      if (which.min(aics) == 1L) {
        1L
      } else if (which.min(aics == 2L)) {
        -1L
      }
    }
  ),

  #active = list(
  #),
  private = list(
    .compute_aic = function(x_name) {
      fam = if (self$classification) binomial() else gaussian()
      form_inc = as.formula(paste0(self$y_name, " ~ ", "s(", x_name, ", bs = 'mpi')"))
      s_inc = scam(formula = form_inc, family = fam, data = self$data)
      form_decr = as.formula(paste0(self$y_name, " ~ ", "s(", x_name, ", bs = 'mpd')"))
      s_decr = scam(formula = form_decr, family = fam, data = self$data)
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
  detector$get_sign("adress")
}

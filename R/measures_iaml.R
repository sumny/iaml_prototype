#' @title IAML Selected Features Proxy
#'
#' @name mlr_measures_iaml_selected_features_proxy
#'
#' @description
#' Proxy measure that simply returns 0.
#' Can be used as a proxy measure which is updated within an optimizer.
#' Only to be used internally.
#'
#' @templateVar id iaml_selected_features_proxy
#' @template measure
#'
#' @export
MeasureIAMLSelectedFeaturesProxy = R6Class("MeasureIAMLSelectedFeaturesProxy",
  inherit = mlr3::Measure,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {

      super$initialize(
        id = "iaml_selected_features_proxy",
        param_set = ps(),
        task_type = NA_character_,
        properties = NA_character_,
        predict_type = "response",
        range = c(0, Inf),
        minimize = TRUE,
        label = "Proxy",
        man = "iaml::mlr_measures_iaml_selected_features_proxy"
      )
    }
  ),

  private = list(
    .score = function(...) {
      0
    }
  )
)

#' @title IAML Selected Interactions Proxy
#'
#' @name mlr_measures_iaml_selected_interactions_proxy
#'
#' @description
#' Proxy measure that simply returns 0.
#' Can be used as a proxy measure which is updated within an optimizer.
#' Only to be used internally.
#'
#' @templateVar id iaml_selected_interactions_proxy
#' @template measure
#'
#' @export
MeasureIAMLSelectedInteractionsProxy = R6Class("MeasureIAMLSelectedInteractionsProxy",
  inherit = mlr3::Measure,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {

      super$initialize(
        id = "iaml_selected_interactions_proxy",
        param_set = ps(),
        task_type = NA_character_,
        properties = NA_character_,
        predict_type = "response",
        range = c(0, Inf),
        minimize = TRUE,
        label = "Proxy",
        man = "iaml::mlr_measures_iaml_selected_interactions_proxy"
      )
    }
  ),

  private = list(
    .score = function(...) {
      0
    }
  )
)

#' @title IAML Selected Non Monotone Proxy
#'
#' @name mlr_measures_iaml_selected_non_monotone_proxy
#'
#' @description
#' Proxy measure that simply returns 0.
#' Can be used as a proxy measure which is updated within an optimizer.
#' Only to be used internally.
#'
#' @templateVar id iaml_selected_non_monotone_proxy
#' @template measure
#'
#' @export
MeasureIAMLSelectedNonMonotoneProxy = R6Class("MeasureIAMLSelectedNonMonotoneProxy",
  inherit = mlr3::Measure,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {

      super$initialize(
        id = "iaml_selected_non_monotone_proxy",
        param_set = ps(),
        task_type = NA_character_,
        properties = NA_character_,
        predict_type = "response",
        range = c(0, Inf),
        minimize = TRUE,
        label = "Proxy",
        man = "iaml::mlr_measures_iaml_selected_non_monotone_proxy"
      )
    }
  ),

  private = list(
    .score = function(...) {
      0
    }
  )
)

#' @title IAML Selected Features Measure
#'
#' @name mlr_measures_iaml_selected_features
#'
#' @description
#' Measures the number of selected features by extracting it from [mlr3pipelines::GraphLearner]s that contain a
#' [mlr3pipelines::PipeOpSelect].
#' If parameter `normalize` is set to `TRUE`, the relative number of features instead of the absolute
#' number of features is returned.
#' Note that the models must be stored to be able to extract this information.
#' This measure is typically only used in combination with [TunerIAML] because it requires parameters of the learner to
#' have additional attributes set and will not work in other workflows.
#'
#' This measure requires the [mlr3::Task] and the [mlr3::Learner] for scoring.
#'
#' @templateVar id iaml_selected_features
#' @template measure
#'
#' @section Parameters:
#' \describe{
#' \item{`normalize`}{`logical(1)`\cr
#' Should the relative number of features instad of the absolute number be returned?
#' }
#' \item{`select_id`}{`character(1)`\cr
#' ID of param in Learner that selects features.}
#' }
#'
#' @export
MeasureIAMLSelectedFeatures = R6Class("MeasureIAMLSelectedFeatures",
  inherit = mlr3::Measure,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        normalize = p_lgl(default = FALSE, tags = "required"),
        actually_used = p_lgl(default = FALSE, tags = "required"),
        select_id = p_uty(tags = "required"),
        learner_id = p_uty(tags = "required")
      )
      param_set$values = list(normalize = FALSE, actually_used = FALSE, select_id = "select.selector", learner_id = "classif.xgboost")

      super$initialize(
        id = "iaml_selected_features",
        param_set = param_set,
        task_type = NA_character_,
        properties = c("requires_task", "requires_learner", "requires_model"),
        predict_type = "response",
        range = c(0, Inf),
        minimize = TRUE,
        label = "Absolute or Relative Frequency of Selected Features",
        man = "iaml::mlr_measures_iaml_selected_features"
      )
    }
  ),

  private = list(
    .score = function(prediction, task, learner, ...) {
      n_selected = attr(learner$param_set$values[[self$param_set$values$select_id]], "n_selected")
      n_selected_total = attr(learner$param_set$values[[self$param_set$values$select_id]], "n_selected_total")

      if (self$param_set$values$actually_used) {
        learner_ = learner$clone(deep = TRUE)
        learner_$train(task)
        xgb_model_name = self$param_set$values$learner_id
        if (xgb_model_name == "NULL") {  # hacky due to default xgb
          features = learner_$model$feature_names
          n_selected_total = length(task$feature_names)
          tmp = tryCatch(xgb_model_dt_tree(features, learner_$model), error = function(ec) {
            NULL
          })
        } else {
          features = learner_$model[[xgb_model_name]]$model$feature_names
          tmp = tryCatch(xgb_model_dt_tree(features, learner_$model[[xgb_model_name]]$model), error = function(ec) {
            NULL
          })
        }
        used = if (is.null(tmp)) {
          features
        } else {
          sort(unique(tmp$Feature[tmp$Feature != "Leaf"]))
        }
        n_a_selected = length(used)
        attr(n_a_selected, "n_selected") = n_selected
        attr(n_a_selected, "used") = used
        n_selected = n_a_selected
      }

      if (self$param_set$values$normalize) {
        n_selected = n_selected / n_selected_total
      }

      n_selected
    }
  )
)

#' @title IAML Selected Interactions Measure
#'
#' @name mlr_measures_iaml_selected_interactions
#'
#' @description
#' Measures the number of selected interactions by extracting it from an appropriate [mlr3::Learner]
#' If parameter `normalize` is set to `TRUE`, the relative number of interactions instead of the absolute
#' number of interactions is returned.
#' Note that the models must be stored to be able to extract this information.
#' This measure is typically only used in combination with [TunerIAML] because it requires parameters of the learner to
#' have additional attributes set and will not work in other workflows.
#'
#' This measure requires the [mlr3::Task] and the [mlr3::Learner] for scoring.
#'
#' @templateVar id iaml_selected_interactions
#' @template measure
#'
#' @section Parameters:
#' \describe{
#' \item{`normalize`}{`logical(1)`\cr
#' Should the relative number of interactions instad of the absolute number be returned?
#' }
#' \item{`interaction_id`}{`character(1)`\cr
#' ID of param in Learner that sets interaction constraints.}
#' }
#'
#' @export
MeasureIAMLSelectedInteractions = R6Class("MeasureIAMLSelectedInteractions",
  inherit = mlr3::Measure,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        normalize = p_lgl(default = FALSE, tags = "required"),
        actually_used = p_lgl(default = FALSE, tags = "required"),
        interaction_id = p_uty(tags = "required"),
        learner_id = p_uty(tags = "required")

      )
      param_set$values = list(normalize = FALSE, actually_used = FALSE, interaction_id = "classif.xgboost.interaction_constraints", learner_id = "classif.xgboost")

      super$initialize(
        id = "iaml_selected_interactions",
        param_set = param_set,
        task_type = NA_character_,
        properties = c("requires_task", "requires_learner", "requires_model"),
        predict_type = "response",
        range = c(0, Inf),
        minimize = TRUE,
        label = "Absolute or Relative Frequency of Selected Interactions",
        man = "iaml::mlr_measures_iaml_selected_interactions"
      )
   }
  ),

  private = list(
    .score = function(prediction, task, learner, ...) {
      n_interactions = attr(learner$param_set$values[[self$param_set$values$interaction_id]], "n_interactions")
      n_interactions_total = attr(learner$param_set$values[[self$param_set$values$interaction_id]], "n_interactions_total")

      if (self$param_set$values$actually_used) {
        learner_ = learner$clone(deep = TRUE)
        learner_$train(task)
        xgb_model_name = self$param_set$values$learner_id
        if (xgb_model_name == "NULL") {  # hacky due to default xgb
          features = learner_$model$feature_names
          n_selected_total = length(task$feature_names)
          n_interactions_total = (n_selected_total * (n_selected_total - 1L)) / 2L
          pairs = tryCatch(interactions(learner_$model, option = "pairs"), error = function(ec) {
            NULL
          })
        } else {
          features = learner_$model[[xgb_model_name]]$model$feature_names
          pairs = tryCatch(interactions(learner_$model[[xgb_model_name]]$model, option = "pairs"), error = function(ec) {
            NULL
          })
        }
        if (is.null(pairs)) {
          n_a_interactions = n_interactions_total
        } else {
          tmp = get_actual_interactions(features, pairs)
          n_a_interactions = tmp$n_interactions
        }
        attr(n_a_interactions, "n_interactions") = n_interactions
        attr(n_a_interactions, "I") = tmp$I
        n_interactions = n_a_interactions
      }

      if (self$param_set$values$normalize) {
        n_interactions = n_interactions / n_interactions_total
        if (n_interactions_total == 0) {
          n_interactions = 0
        }
      }

      n_interactions
    }
  )
)

#' @title IAML Selected Non Monotone Features Measure
#'
#' @name mlr_measures_iaml_selected_non_monotone
#'
#' @description
#' Measures the number of non monotone features by extracting it from an appropriate [mlr3::Learner]
#' If parameter `normalize` is set to `TRUE`, the relative number of non monotone features instead of the absolute
#' number of non monotone is returned.
#' Note that the models must be stored to be able to extract this information.
#' This measure is typically only used in combination with [TunerIAML] because it requires parameters of the learner to
#' have additional attributes set and will not work in other workflows.
#'
#' This measure requires the [mlr3::Task] and the [mlr3::Learner] for scoring.
#'
#' @templateVar id iaml_selected_non_monotone
#' @template measure
#'
#' @section Parameters:
#' \describe{
#' \item{`normalize`}{`logical(1)`\cr
#' Should the relative number of non monotone features instad of the absolute number be returned?
#' }
#' \item{`monotone_id`}{`character(1)`\cr
#' ID of param in Learner that sets monotonicity constraints.}
#' }
#'
#' @export
MeasureIAMLSelectedNonMonotone = R6Class("MeasureIAMLSelectedNonMonotone",
  inherit = mlr3::Measure,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        normalize = p_lgl(default = FALSE, tags = "required"),
        actually_used = p_lgl(default = FALSE, tags = "required"),
        monotone_id = p_uty(tags = "required"),
        learner_id = p_uty(tags = "required")
      )
      param_set$values = list(normalize = FALSE, actually_used = FALSE, monotone_id = "classif.xgboost.monotone_constraints", learner_id = "classif.xgboost")

      super$initialize(
        id = "iaml_selected_non_monotone",
        param_set = param_set,
        task_type = NA_character_,
        properties = c("requires_task", "requires_learner", "requires_model"),
        predict_type = "response",
        range = c(0, Inf),
        minimize = TRUE,
        label = "Absolute or Relative Frequency of Selected Non Monotone Features",
        man = "iaml::mlr_measures_iaml_selected_non_monotone"
      )
    }
  ),

  private = list(
    .score = function(prediction, task, learner, ...) {
      n_non_monotone = attr(learner$param_set$values[[self$param_set$values$monotone_id]], "n_non_monotone")
      n_non_monotone_total = attr(learner$param_set$values[[self$param_set$values$monotone_id]], "n_non_monotone_total")

      if (self$param_set$values$actually_used) {
        learner_ = learner$clone(deep = TRUE)
        learner_$train(task)
        xgb_model_name = self$param_set$values$learner_id
        if (xgb_model_name == "NULL") {  # hacky due to default xgb
          features = learner_$model$feature_names
          n_non_monotone_total = length(task$feature_names)
          tmp = tryCatch(xgb_model_dt_tree(features, learner_$model), error = function(ec) {
            NULL
          })
          used = if (is.null(tmp)) {
            features
          } else {
            sort(unique(tmp$Feature[tmp$Feature != "Leaf"]))
          }
          a_non_monotone = rep(0, length(used))
          names(a_non_monotone) = used
          n_a_non_monotone = sum(a_non_monotone == 0)
          attr(n_a_non_monotone, "n_non_monotone") = n_non_monotone
          attr(n_a_non_monotone, "a_non_monotone") = a_non_monotone
          n_non_monotone = n_a_non_monotone
        } else {
          features = learner_$model[[xgb_model_name]]$model$feature_names
          tmp = tryCatch(xgb_model_dt_tree(features, learner_$model[[xgb_model_name]]$model), error = function(ec) {
            NULL
          })
          used = if (is.null(tmp)) {
            features
          } else {
            sort(unique(tmp$Feature[tmp$Feature != "Leaf"]))
          }
          a_non_monotone = learner_$param_set$values[[self$param_set$values$monotone_id]]
          a_non_monotone = a_non_monotone[names(a_non_monotone) %in% used]
          n_a_non_monotone = sum(a_non_monotone == 0)
          attr(n_a_non_monotone, "n_non_monotone") = n_non_monotone
          attr(n_a_non_monotone, "a_non_monotone") = a_non_monotone
          n_non_monotone = n_a_non_monotone
        }
      }

      if (self$param_set$values$normalize) {
        n_non_monotone = n_non_monotone / n_non_monotone_total
      }

      n_non_monotone
    }
  )
)


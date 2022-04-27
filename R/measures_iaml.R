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
        select_id = p_uty(tags = "required")
      )
      param_set$values = list(normalize = FALSE, select_id = "select.selector")

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
        interaction_id = p_uty(tags = "required")
      )
      param_set$values = list(normalize = FALSE, interaction_id = "classif.xgboost.interaction_constraints")

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
      if (self$param_set$values$normalize) {
        n_interactions = n_interactions / n_interactions_total
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
        monotone_id = p_uty(tags = "required")
      )
      param_set$values = list(normalize = FALSE, monotone_id = "classif.xgboost.monotone_constraints")

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
      if (self$param_set$values$normalize) {
        n_non_monotone = n_non_monotone / n_non_monotone_total
      }

      n_non_monotone
    }
  )
)


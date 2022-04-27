#' @title IML Number of Features Used
#'
#' @name mlr_measures_iml_number_of_features
#'
#' @description
#' Measures the number of selected features used by a model according to Molnar et al. (2020).
#' If parameter `normalize` is set to `TRUE`, the relative number of features instead of the absolute
#' number of features is returned.
#' Note that the models must be stored to be able to extract this information.
#'
#' This measure requires the [mlr3::Task] and the [mlr3::Learner] for scoring.
#'
#' @templateVar id iml_number_of_features
#' @template measure
#'
#' @section Parameters:
#' \describe{
#' \item{`normalize`}{`logical(1)`\cr
#' Should the relative number of features instad of the absolute number be returned?
#' }
#' }
#'
#' @references
#' `r format_bib("molnar_2020")`
#' @export
MeasureIMLNF = R6Class("MeasureIMLNF",
  inherit = mlr3::Measure,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        normalize = p_lgl(default = FALSE, tags = "required")
      )
      param_set$values = list(normalize = FALSE)

      super$initialize(
        id = "iml_number_of_features",
        param_set = param_set,
        task_type = NA_character_,
        properties = c("requires_task", "requires_learner", "requires_model"),
        predict_type = "prob",
        range = c(0, Inf),
        minimize = TRUE,
        label = "Absolute or Relative Frequency of Selected Features",
        man = "iaml::mlr_measures_iml_number_of_features"
      )
    }
  ),

  private = list(
    .score = function(prediction, task, learner, ...) {
      imeasure_hash_table = get("imeasure_hash_table", envir = asNamespace("iaml"))
      # NOTE: here, we train the model on all data to get the interpretability measures
      if (learner$hash %nin% imeasure_hash_table$hash) {
        learner_ = learner$clone(deep = TRUE)
        learner_$train(task)
        if (task$task_type == "classif") {
          stopifnot(length(task$class_names) == 2L)
          pred = Predictor$new(learner_, data = as.data.frame(task$data()), y = task$target_names, class = task$positive)
          pred$prediction.function = create_predict_fun_custom(learner_, task = "classification")
        } else if (task$task_type == "regr") {
          pred = Predictor$new(learner_, data = as.data.frame(task$data()), y = task$target_names)
          pred$prediction.function = create_predict_fun_custom(learner_, task = "regression")
        }
        imeasure = FunComplexity$new(pred, max_seg_cat = 9L, max_seg_num = 5L, epsilon = 0.05, grid.size = 200L)  # FIXME: hps
        imeasure_hash_table = rbind(data.table(hash = learner$hash, imeasure = list(imeasure)), imeasure_hash_table)[1:min(nrow(imeasure_hash_table), 100L), ]
        unlockBinding("imeasure_hash_table", asNamespace("iaml"))
        assign("imeasure_hash_table", value = imeasure_hash_table, envir = asNamespace("iaml"))
        lockBinding("imeasure_hash_table", asNamespace("iaml"))
      } else {
        imeasure = imeasure_hash_table[hash == learner$hash, ]$imeasure[[1L]]
      }
      n = imeasure$n_features
      if (self$param_set$values$normalize) {
        n = n / length(task$feature_names)
      }
      n
    }
  )
)

#' @title IML Interaction Strength
#'
#' @name mlr_measures_iml_interaction_strength
#'
#' @description
#' Measures the interaction strength of features of a model according to Molnar et al. (2020).
#' Note that the models must be stored to be able to extract this information.
#'
#' This measure requires the [mlr3::Task] and the [mlr3::Learner] for scoring.
#'
#' @templateVar id iml_interaction_strength
#' @template measure
#'
#' @references
#' `r format_bib("molnar_2020")`
#' @export
MeasureIMLIAS = R6Class("MeasureIMLIAS",
  inherit = mlr3::Measure,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
      )

      super$initialize(
        id = "iml_interaction_strength",
        param_set = param_set,
        task_type = NA_character_,
        properties = c("requires_task", "requires_learner", "requires_model"),
        predict_type = "prob",
        range = c(0, Inf),
        minimize = TRUE,
        label = "Interaction Strength of Features",
        man = "iaml::mlr_measures_iml_interaction_strength"
      )
    }
  ),

  private = list(
    .score = function(prediction, task, learner, ...) {
      imeasure_hash_table = get("imeasure_hash_table", envir = asNamespace("iaml"))
      # NOTE: here, we train the model on all data to get the interpretability measures
      if (learner$hash %nin% imeasure_hash_table$hash) {
        learner_ = learner$clone(deep = TRUE)
        learner_$train(task)
        if (task$task_type == "classif") {
          stopifnot(length(task$class_names) == 2L)
          pred = Predictor$new(learner_, data = as.data.frame(task$data()), y = task$target_names, class = task$positive)
          pred$prediction.function = create_predict_fun_custom(learner_, task = "classification")
        } else if (task$task_type == "regr") {
          pred = Predictor$new(learner_, data = as.data.frame(task$data()), y = task$target_names)
          pred$prediction.function = create_predict_fun_custom(learner_, task = "regression")
        }
        imeasure = FunComplexity$new(pred, max_seg_cat = 9L, max_seg_num = 5L, epsilon = 0.05, grid.size = 200L)  # FIXME: hps
        imeasure_hash_table = rbind(data.table(hash = learner$hash, imeasure = list(imeasure)), imeasure_hash_table)[1:min(nrow(imeasure_hash_table), 100L), ]
        unlockBinding("imeasure_hash_table", asNamespace("iaml"))
        assign("imeasure_hash_table", value = imeasure_hash_table, envir = asNamespace("iaml"))
        lockBinding("imeasure_hash_table", asNamespace("iaml"))
      } else {
        imeasure = imeasure_hash_table[hash == learner$hash, ]$imeasure[[1L]]
      }
      1 - imeasure$r2
    }
  )
)

#' @title IML Main Effect Complexity
#'
#' @name mlr_measures_iml_main_effect_complexity
#'
#' @description
#' Measures the main effect complexity of features of a model according to Molnar et al. (2020).
#' Note that the models must be stored to be able to extract this information.
#'
#' This measure requires the [mlr3::Task] and the [mlr3::Learner] for scoring.
#'
#' @templateVar id iml_main_effect_complexity
#' @template measure
#'
#' @references
#' `r format_bib("molnar_2020")`
#' @export
MeasureIMLMEC = R6Class("MeasureIMLMEC",
  inherit = mlr3::Measure,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        normalize = p_lgl(default = FALSE, tags = "required")
      )
      param_set$values = list(normalize = FALSE)

      super$initialize(
        id = "iml_main_effect_complexity",
        param_set = param_set,
        task_type = NA_character_,
        properties = c("requires_task", "requires_learner", "requires_model"),
        predict_type = "prob",
        range = c(0, Inf),
        minimize = TRUE,
        label = "Main Effect Complexity of Features",
        man = "iaml::mlr_measures_iml_main_effect_complexity"
      )
    }
  ),

  private = list(
    .score = function(prediction, task, learner, ...) {
      imeasure_hash_table = get("imeasure_hash_table", envir = asNamespace("iaml"))
      # NOTE: here, we train the model on all data to get the interpretability measures
      if (learner$hash %nin% imeasure_hash_table$hash) {
        learner_ = learner$clone(deep = TRUE)
        learner_$train(task)
        if (task$task_type == "classif") {
          stopifnot(length(task$class_names) == 2L)
          pred = Predictor$new(learner_, data = as.data.frame(task$data()), y = task$target_names, class = task$positive)
          pred$prediction.function = create_predict_fun_custom(learner_, task = "classification")
        } else if (task$task_type == "regr") {
          pred = Predictor$new(learner_, data = as.data.frame(task$data()), y = task$target_names)
          pred$prediction.function = create_predict_fun_custom(learner_, task = "regression")
        }
        imeasure = FunComplexity$new(pred, max_seg_cat = 9L, max_seg_num = 5L, epsilon = 0.05, grid.size = 200L)  # FIXME: hps
        imeasure_hash_table = rbind(data.table(hash = learner$hash, imeasure = list(imeasure)), imeasure_hash_table)[1:min(nrow(imeasure_hash_table), 100L), ]
        unlockBinding("imeasure_hash_table", asNamespace("iaml"))
        assign("imeasure_hash_table", value = imeasure_hash_table, envir = asNamespace("iaml"))
        lockBinding("imeasure_hash_table", asNamespace("iaml"))
      }  else {
        imeasure = imeasure_hash_table[hash == learner$hash, ]$imeasure[[1L]]
      }
      imeasure$c_wmean
    }
  )
)


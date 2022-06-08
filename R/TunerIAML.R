#' @title Multi-objective Hyperparameter Optimization, Feature Selection and Interaction and Monotonicity Constraints
#'
#' @name mlr_tuner_iaml
#'
#' @description
#' Performs joint multi-objective optimization of hyperparameters, feature selection and interaction and monotonicity
#' constraints of a suitable [mlr3::Learner].
#'
#' This requires an appropriate [mlr3::Learner], that allows for selecting features, and setting interaction and
#' monotonicity constraints, e.g., xgboost.
#'
#' @templateVar id iaml
#' @template section_dictionary_tuners
#'
#' @section Parameters:
#' \describe{
#' \item{`select_id`}{`character(1)`\cr
#' ID of param in Learner that selects features.}
#' \item{`interaction_id`}{`character(1)`\cr
#' ID of param in Learner that sets interaction constraints.}
#' \item{`monotone_id`}{`character(1)`\cr
#' ID of param in Learner that sets monotonicity constraints.}
#' \item{`batch_size`}{`integer(1)`\cr
#' Maximum number of points to try in a batch.}
#' }
#'
#' @template section_progress_bars
#' @template section_logging
#'
#' @family Tuner
#'
#' @export
TunerIAML = R6Class("TunerIAML",
  inherit = mlr3tuning::Tuner,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        select_id = p_uty(tags = "required"),
        interaction_id = p_uty(tags = "required"),
        monotone_id = p_uty(tags = "required"),
        batch_size = p_int(default = 1L, lower = 1L, upper = 1L, tags = "required")  # FIXME: currently only batch_size of 1 see below
      )
      param_set$values = list(select_id = "select.selector", interaction_id = "classif.xgboost.interaction_constraints", monotone_id = "classif.xgboost.monotone_constraints", batch_size = 1L)
      super$initialize(
        param_set = param_set,
        param_classes = c("ParamDbl", "ParamFct", "ParamInt", "ParamLgl", "ParamUty"),
        properties = "multi-crit",
        #packages = "iaml",
        label = "FIXME:",
        man = "iaml::mlr_tuners_iaml"
      )
    }
  ),

  private = list(
    .optimize = function(inst) {
      # FIXME: bug in mlr3? learner param set is not pertained correctly, therefore values are always the same, only affects batch size > 1
      batch_size = self$param_set$values$batch_size
      select_id = self$param_set$values$select_id
      interaction_id = self$param_set$values$interaction_id
      monotone_id = self$param_set$values$monotone_id
      param_ids = setdiff(inst$search_space$ids(), c(select_id, interaction_id, monotone_id))
      param_space = ParamSet$new(inst$search_space$params[param_ids])
      param_space$trafo = inst$search_space$trafo
      param_space$deps = inst$search_space$deps

      task = inst$objective$task

      repeat {  # iterate until we have an exception from eval_batch
        population = generate_design_random(param_space, n = batch_size)$data  # param_space
        sIm = map_dtr(seq_len(batch_size), function(i) {  # sIm space
          iaml = IAMLPoint$new(task)
          data.table(iaml = list(iaml),
                     s = list(iaml$create_selector()),
                     I = list(iaml$create_interaction_constraints()),
                     m = list(iaml$create_monotonicity_constraints()))
        })
        colnames(sIm) = c("iaml", select_id, interaction_id, monotone_id)

        population = cbind(population, sIm)

        inst$eval_batch(population)
      }

      inst
    }
  )
)


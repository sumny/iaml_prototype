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
        batch_size = p_int(default = 1L, tags = "required")
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
      batch_size = self$param_set$values$batch_size
      select_id = self$param_set$values$select_id
      interaction_id = self$param_set$values$interaction_id
      monotone_id = self$param_set$values$monotone_id
      param_ids = setdiff(inst$search_space$ids(), c(select_id, interaction_id, monotone_id))
      param_space = ParamSet$new(inst$search_space$params[param_ids])
      param_space$trafo = inst$search_space$trafo
      param_space$deps = inst$search_space$deps

      sampler = SamplerUnif$new(param_space)

      task = inst$objective$task
      features = task$feature_names
      n_features = length(task$feature_names)

      repeat {  # iterate until we have an exception from eval_batch
        design = sampler$sample(batch_size)

        sIm = map_dtr(seq_len(batch_size), function(i) {
          n_selected = sample(seq_len(n_features), size = 1L)
          selected_features = sample(features, size = n_selected, replace = FALSE)
          interactions = sample_interactions_random(selected_features)
          eqcs = get_eqcs(interactions)
          s = selector_name(selected_features)
          attr(s, "n_selected") = n_selected
          attr(s, "n_selected_total") = n_features
          I = list(I = get_matrix(eqcs), classes = map(eqcs, function(x) match(x, selected_features)))
          m = sample_m(I)
          interaction_constraints = I$classes
          n_interactions = sum(I$I)
          n_interactions_total = nrow(I$I) ^ 2L
          I = map(interaction_constraints, function(x) x - 1L)
          attr(I, "n_interactions") = n_interactions
          attr(I, "n_interactions_total") = n_interactions_total
          n_non_monotone = sum(m != 0)
          n_non_monotone_total = length(m)
          attr(m, "n_non_monotone") = n_non_monotone
          attr(m, "n_non_monotone_total") = n_non_monotone_total
          data.table(s = list(s), I = list(I), m = list(m))
        })
        colnames(sIm) = c(select_id, interaction_id, monotone_id)
        inst$eval_batch(cbind(design$data, sIm))
      }

      inst
    }
  )
)


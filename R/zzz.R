#' @importFrom R6 R6Class
#' @import checkmate
#' @import data.table
#' @import paradox
#' @import mlr3misc
#' @import bbotk
#' @import lgr
#' @import mlr3
#' @import mlr3learners
#' @import mlr3pipelines
#' @import mlr3tuning
#@importFrom stats setNames runif dnorm pnorm

.onLoad = function(libname, pkgname) { # nolint
  # nocov start
  backports::import(pkgname)

  # add iaml to tuner dictionary
  x = utils::getFromNamespace("mlr_tuners", ns = "mlr3tuning")
  x$add("iaml", TunerIAML)

  # add iaml to measures dictionary
  x = utils::getFromNamespace("mlr_measures", ns = "mlr3")
  x$add("iaml_selected_features", function() MeasureIAMLSelectedFeatures$new())
  x$add("iaml_selected_interactions", function() MeasureIAMLSelectedInteractions$new())
  x$add("iaml_selected_non_monotone", function() MeasureIAMLSelectedNonMonotone$new())
  # add iaml to measures dictionary
  x$add("iml_number_of_features", function() MeasureIMLNF$new())
  x$add("iml_interaction_strength", function() MeasureIMLIAS$new())
  x$add("iml_main_effect_complexity", function() MeasureIMLMEC$new())

  # setup logger
  assign("lg", lgr::get_logger("bbotk"), envir = parent.env(environment()))

  if (Sys.getenv("IN_PKGDOWN") == "true") {
    lg$set_threshold("warn")
  }

  imeasure_hash_table = data.table(hash = character(), imeasure = list())
  assign("imeasure_hash_table", value = imeasure_hash_table, envir = asNamespace("iaml"))
} # nocov end

# static code checks should not complain about commonly used data.table columns
utils::globalVariables(c(".", "..feature", "xv", "alev", "interval", "ale", "n", "lvl"))

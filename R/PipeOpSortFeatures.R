#' @export
PipeOpSortFeatures = R6Class("PipeOpSortFeatures",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "sortfeatures", param_vals = list()) {
      super$initialize(id, param_set = ps(), param_vals = NULL, can_subset_cols = FALSE)
    }
  ),
  private = list(
    .transform = function(task) {
      task$col_roles$feature = sort(task$col_roles$feature)
      task
    }
  )
)

# cbind conjugated features to the task (prefixed with ".neg_")
PipeOpAddConjugates = R6Class("PipeOpAddConjugates",
  inherit = mlr3pipelines::PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "add_conjugates") {
      super$initialize(id, tags = "data transform")
    }
  ),
  private = list(

    .get_state = function(task) {
      assert_task(task, feature_types = c("integer", "numeric"))
      feature_names = colnames(task$data(cols = task$feature_names))
      new_feature_names = paste0(".neg_", feature_names)  # this is our contract to later work with conjugated features
      assert_true(length(intersect(new_feature_names, feature_names)) == 0L)
      list(NULL)
    },

    .transform = function(task) {
      taskdata = task$data(cols = task$feature_names)
      taskdata_conjugate = - taskdata
      feature_names = colnames(taskdata)
      new_feature_names = paste0(".neg_", feature_names)
      colnames(taskdata_conjugate) = new_feature_names
      task$cbind(taskdata_conjugate)
      task
    }
  )
)


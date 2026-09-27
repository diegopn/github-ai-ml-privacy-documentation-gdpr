MockStage <- R6::R6Class(
  "MockStage",
  public = list(
    initialize = function(label, recorder, failure = NULL) {
      private$label <- label
      private$recorder <- recorder
      private$failure <- failure
      invisible(self)
    },
    run = function(...) {
      if (!is.null(private$failure)) stop(private$failure)
      private$recorder$record(private$label)
      invisible(TRUE)
    }
  ),
  private = list(label = NULL, recorder = NULL, failure = NULL),
  lock_class = TRUE,
  cloneable = FALSE
)

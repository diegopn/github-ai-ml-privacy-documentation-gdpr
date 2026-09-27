MockRunRecorder <- R6::R6Class(
  "MockRunRecorder",
  public = list(
    initialize = function() {
      private$recorded_items <- character()
      invisible(self)
    },
    record = function(value) {
      private$recorded_items <- c(private$recorded_items, value)
      invisible(value)
    },
    items = function() private$recorded_items
  ),
  private = list(recorded_items = NULL),
  lock_class = TRUE,
  cloneable = FALSE
)

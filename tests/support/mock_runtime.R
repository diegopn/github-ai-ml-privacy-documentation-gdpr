MockRuntime <- R6::R6Class(
  "MockRuntime",
  public = list(
    initialize = function(start = 1000) {
      private$time <- as.numeric(start)
      invisible(self)
    },
    now = function() private$time,
    sleep = function(seconds) {
      private$time <- private$time + as.numeric(seconds)
      invisible(NULL)
    }
  ),
  private = list(time = NULL),
  lock_class = TRUE,
  cloneable = FALSE
)

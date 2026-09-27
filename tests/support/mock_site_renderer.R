MockSiteRenderer <- R6::R6Class(
  "MockSiteRenderer",
  public = list(
    initialize = function(recorder) {
      private$recorder <- recorder
      invisible(self)
    },
    render = function() {
      private$recorder$record("site")
      invisible(TRUE)
    }
  ),
  private = list(recorder = NULL),
  lock_class = TRUE,
  cloneable = FALSE
)

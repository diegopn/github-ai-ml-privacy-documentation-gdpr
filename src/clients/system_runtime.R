SystemRuntime <- R6::R6Class(
  "SystemRuntime",
  public = list(
    now = function() as.numeric(Sys.time()),
    sleep = function(seconds) Sys.sleep(seconds)
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

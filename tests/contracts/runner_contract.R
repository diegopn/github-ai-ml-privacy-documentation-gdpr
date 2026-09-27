RunnerContract <- R6::R6Class(
  "RunnerContract",
  public = list(
    run = function(context) {
      project <- context$new_test_project()
      recorder <- MockRunRecorder$new()
      runner <- context$classes()$ExperimentRunner$new(
        project$config,
        project$store,
        context$services()$lock_manager,
        MockStage$new("selection", recorder),
        MockStage$new("collection", recorder),
        MockStage$new("analysis", recorder),
        context$cli(), context$values(), MockSiteRenderer$new(recorder)
      )
      runner$run("run")
      status <- jsonlite::fromJSON(file.path(project$root, "outputs", "metadata", "run_status.json"), simplifyVector = FALSE)
      context$check("runner executa etapas na ordem e grava status final",
        all(identical(recorder$items(), c("selection", "collection", "analysis", "site")),
          identical(status$state, "completed"), length(status$stages) == 4L))
      rate_limit <- structure(list(message = "rate limit", call = NULL, rate_limited = TRUE),
        class = c("github_rate_limit_error", "github_api_error", "error", "condition"))
      paused_recorder <- MockRunRecorder$new()
      paused_runner <- context$classes()$ExperimentRunner$new(
        project$config,
        project$store,
        context$services()$lock_manager,
        MockStage$new("selection", paused_recorder, rate_limit),
        MockStage$new("collection", paused_recorder),
        MockStage$new("analysis", paused_recorder),
        context$cli(), context$values(), MockSiteRenderer$new(paused_recorder)
      )
      tryCatch(paused_runner$run("run"), error = identity)
      paused <- jsonlite::fromJSON(file.path(project$root, "outputs", "metadata", "run_status.json"), simplifyVector = FALSE)
      context$check("runner marca rate limit estruturado como execução pausada",
        identical(paused$state, "paused"))
      invisible(TRUE)
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

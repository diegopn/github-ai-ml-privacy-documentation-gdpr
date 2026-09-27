ExperimentRunner <- R6::R6Class(
  "ExperimentRunner",
  public = list(
    initialize = function(config, store, lock_manager, selector, collector, analyzer, cli, values, site_renderer) {
      if (!inherits(config, "ProjectConfig")) stop("ExperimentRunner exige ProjectConfig.", call. = FALSE)
      if (!inherits(store, "ArtifactStore")) stop("ExperimentRunner exige ArtifactStore.", call. = FALSE)
      if (!inherits(lock_manager, "ArtifactLock")) stop("ExperimentRunner exige ArtifactLock.", call. = FALSE)
      if (!inherits(cli, "ExperimentCli")) stop("ExperimentRunner exige ExperimentCli.", call. = FALSE)
      if (!inherits(values, "ValueTools")) stop("ExperimentRunner exige ValueTools.", call. = FALSE)
      private$assert_dependency(selector, "run", "selector")
      private$assert_dependency(collector, "run", "collector")
      private$assert_dependency(analyzer, "run", "analyzer")
      private$assert_dependency(site_renderer, "render", "site_renderer")
      private$config <- config
      private$store <- store
      private$lock_manager <- lock_manager
      private$selector <- selector
      private$collector <- collector
      private$analyzer <- analyzer
      private$cli <- cli
      private$values <- values
      private$site_renderer <- site_renderer
      invisible(self)
    },
    run = function(mode = "run") {
      private$mode <- mode
      private$stages <- list()
      private$started_at <- Sys.time()
      stages <- private$cli$pipeline_stages(mode)
      lock_path <- file.path(private$config$path("output_root", "outputs"), "metadata", ".pipeline.lock")
      private$lock_manager$acquire(lock_path, timeout_seconds = 0,
        stale_seconds = as.numeric(private$config$get("api", "lock_stale_seconds", 300)),
        metadata = list(mode = mode, project_root = private$config$root()))
      on.exit(private$lock_manager$release(lock_path), add = TRUE)
      private$write_status("running")
      tryCatch({
        for (stage in stages) private$run_stage(stage, stages)
        private$write_status("completed")
        cat("Execução concluída.\n")
        invisible(TRUE)
      }, error = function(error) {
        state <- if (inherits(error, "github_rate_limit_error")) "paused" else "failed"
        private$write_status(state, conditionMessage(error))
        cat(sprintf("Execução %s.\n", if (state == "paused") "pausada" else "interrompida"))
        stop(error)
      })
    }
  ),
  private = list(
    config = NULL, store = NULL, lock_manager = NULL, selector = NULL, collector = NULL, analyzer = NULL,
    cli = NULL, values = NULL, site_renderer = NULL, stages = list(), mode = NULL, started_at = NULL,
    assert_dependency = function(value, method, label) {
      if (!(inherits(value, "R6") && is.function(value[[method]]))) stop(sprintf("%s precisa implementar $%s().", label, method), call. = FALSE)
    },
    action = function(stage) {
      switch(stage,
        selection = private$selector$run(),
        collection = private$collector$run(),
        collection_check = {
          sample <- private$store$assert_published_sample()
          private$store$read_complete_checkpoint(sample, private$store$raw_path(), private$store$hash_file(private$store$sample_path()))
        },
        analysis = private$analyzer$run(),
        site = private$site_renderer$render(),
        stop(sprintf("Etapa desconhecida: %s", stage), call. = FALSE))
    },
    run_stage = function(stage, stages) {
      label <- sprintf("Etapa %d/%d — %s", length(private$stages) + 1L, length(stages), private$cli$stage_label(stage))
      started <- Sys.time()
      if (stage != "site") cat(sprintf("\n=== %s ===\n", label))
      private$write_status("running", current_stage = label)
      tryCatch(private$action(stage), error = function(error) {
        finished <- Sys.time()
        private$stages[[length(private$stages) + 1L]] <- private$stage_record(label, "failed", started, finished, error)
        stop(error)
      })
      finished <- Sys.time()
      private$stages[[length(private$stages) + 1L]] <- private$stage_record(label, "completed", started, finished)
      private$write_status("running")
      invisible(TRUE)
    },
    stage_record = function(label, state, started, finished, error = NULL) {
      result <- list(stage = label, status = state,
        started_at = private$values$format_utc(started), finished_at = private$values$format_utc(finished),
        elapsed_seconds = as.numeric(difftime(finished, started, units = "secs")))
      if (!is.null(error)) result$error <- conditionMessage(error)
      result
    },
    write_status = function(state, error_message = NULL, current_stage = NULL) {
      status <- list(state = state, mode = private$mode,
        started_at = private$values$format_utc(private$started_at),
        updated_at = private$values$format_utc(Sys.time()), project_root = private$config$root(),
        pid = Sys.getpid(), current_stage = current_stage, stages = private$stages)
      if (!is.null(error_message)) status$error <- as.character(error_message)
      private$store$write_json(status, file.path(private$config$path("output_root", "outputs"), "metadata", "run_status.json"))
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

ExperimentRunner <- R6::R6Class(
  "ExperimentRunner",
  public = list(
    config = NULL,
    store = NULL,
    selector = NULL,
    collector = NULL,
    analyzer = NULL,
    stages = list(),
    mode = NULL,
    started_at = NULL,
    initialize = function(config, store, selector, collector, analyzer) {
      self$config <- config
      self$store <- store
      self$selector <- selector
      self$collector <- collector
      self$analyzer <- analyzer
    },
    run = function(mode = "run") {
      self$mode <- mode
      self$stages <- list()
      self$started_at <- Sys.time()
      lock_path <- file.path(self$config$path("output_root", "outputs"), "metadata", ".pipeline.lock")
      acquire_file_lock(lock_path, timeout_seconds = 0,
                        stale_seconds = as.numeric(self$config$get("api", "lock_stale_seconds", 300)),
                        metadata = list(mode = mode, project_root = self$config$root))
      on.exit(release_file_lock(lock_path), add = TRUE)
      private$write_status("running")
      tryCatch({
        for (stage in pipeline_stages(mode)) private$run_stage(stage)
        private$write_status("completed")
        cat("Execução concluída.\n")
        invisible(TRUE)
      }, error = function(error) {
        state <- if (api_error_is_rate_limited(error)) "paused" else "failed"
        private$write_status(state, conditionMessage(error))
        cat(sprintf("Execução %s.\n", if (state == "paused") "pausada" else "interrompida"))
        stop(conditionMessage(error), call. = FALSE)
      })
    }
  ),
  private = list(
    action = function(stage) {
      switch(stage,
        selection = self$selector$run(),
        collection = self$collector$run(),
        collection_check = {
          sample <- self$store$assert_published_sample()
          self$store$assert_checkpoint_complete(sample, self$store$raw_path(), self$store$hash_file(self$store$sample_path()))
        },
        analysis = self$analyzer$run(),
        site = private$render_site(),
        stop(sprintf("Etapa desconhecida: %s", stage), call. = FALSE)
      )
    },
    run_stage = function(stage) {
      label <- sprintf("Etapa %d/%d — %s", length(self$stages) + 1L,
                       length(pipeline_stages(self$mode)), STAGE_LABELS[[stage]])
      started <- Sys.time()
      if (stage != "site") cat(sprintf("\n=== %s ===\n", label))
      private$write_status("running", current_stage = label)
      tryCatch(private$action(stage), error = function(error) {
        finished <- Sys.time()
        self$stages[[length(self$stages) + 1L]] <- private$stage_record(label, "failed", started, finished, error)
        stop(error)
      })
      finished <- Sys.time()
      self$stages[[length(self$stages) + 1L]] <- private$stage_record(label, "completed", started, finished)
      private$write_status("running")
      invisible(TRUE)
    },
    stage_record = function(label, state, started, finished, error = NULL) {
      result <- list(stage = label, status = state,
                     started_at = format_utc(started), finished_at = format_utc(finished),
                     elapsed_seconds = as.numeric(difftime(finished, started, units = "secs")))
      if (!is.null(error)) result$error <- conditionMessage(error)
      result
    },
    write_status = function(state, error_message = NULL, current_stage = NULL) {
      status <- list(state = state, mode = self$mode,
                     started_at = format_utc(self$started_at), updated_at = format_utc(Sys.time()),
                     project_root = self$config$root, pid = Sys.getpid(),
                     current_stage = current_stage, stages = self$stages)
      if (!is.null(error_message)) status$error <- as.character(error_message)
      self$store$write_json(status, file.path(self$config$path("output_root", "outputs"), "metadata", "run_status.json"))
    },
    render_site = function() {
      if (!nzchar(Sys.which("quarto"))) stop("Quarto não encontrado no PATH; ele é necessário para gerar o site.", call. = FALSE)
      exit_status <- suppressWarnings(system2("quarto", "render", stdout = FALSE, stderr = FALSE))
      if (!identical(as.integer(exit_status), 0L)) stop("Não foi possível gerar o site com o Quarto.", call. = FALSE)
      cat("Site gerado.\n")
      invisible(TRUE)
    }
  )
)

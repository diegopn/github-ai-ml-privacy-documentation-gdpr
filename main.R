#!/usr/bin/env Rscript

main_file_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
main_file <- if (length(main_file_argument)) sub("^--file=", "", main_file_argument[[1L]]) else file.path(getwd(), "main.R")
if (!file.exists(main_file)) main_file <- file.path(getwd(), main_file)
project_root <- normalizePath(file.path(dirname(main_file), "."), mustWork = TRUE)
setwd(project_root)

source(file.path(project_root, "functions", "common.R"))
source(file.path(project_root, "functions", "cli.R"))
source(file.path(project_root, "functions", "select_sample.R"))
source(file.path(project_root, "functions", "collect.R"))
source(file.path(project_root, "functions", "analyze.R"))

main_options <- parse_main_options(commandArgs(trailingOnly = TRUE))
if (isTRUE(main_options$help)) {
  cat(usage_text(), "\n")
  quit(save = "no", status = 0L, runLast = FALSE)
}
status_path <- file.path(project_root, "outputs", "metadata", "run_status.json")
run_lock_path <- project_lock_path()
run_started <- Sys.time()
run_started_at <- format(run_started, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
stage_results <- list()

selection_status_for_run <- function() {
  selection_state <- read_selection_state()
  if (!is.list(selection_state)) return(NULL)
  list(
    status = scalar_text(selection_state$status, ""),
    operation = scalar_text(selection_state$operation, ""),
    run_id = scalar_text(selection_state$run_id, ""),
    sample_path = scalar_text(selection_state$sample_path, ""),
    sample_sha256 = scalar_text(selection_state$sample_sha256, ""),
    configuration_fingerprint = scalar_text(selection_state$configuration_fingerprint, ""),
    evaluated_candidates = scalar_int(selection_state$evaluated_candidates, 0L),
    total_candidates = scalar_int(selection_state$total_candidates, 0L),
    rate_limit_retry_at = scalar_text(selection_state$rate_limit_retry_at, ""),
    error = scalar_text(selection_state$error, "")
  )
}

write_run_status <- function(state, error_message = NULL, current_stage = NULL) {
  dir.create(dirname(status_path), recursive = TRUE, showWarnings = FALSE)
  status <- list(
    state = state,
    mode = main_options$mode,
    started_at = run_started_at,
    updated_at = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
    project_root = project_root,
    pid = Sys.getpid(),
    current_stage = current_stage,
    selection = selection_status_for_run(),
    stages = stage_results
  )
  if (!is.null(error_message)) status$error <- as.character(error_message)
  atomic_write_json(status, status_path, pretty = TRUE)
}

pipeline_error_state <- function() {
  selection_state <- read_selection_state()
  if (is.list(selection_state) && identical(scalar_text(selection_state$status, ""), "paused")) "paused" else "failed"
}

stage_result <- function(label, status, started, finished, error = NULL) {
  result <- list(
    stage = label,
    status = status,
    started_at = format(started, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
    finished_at = format(finished, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
    elapsed_seconds = as.numeric(difftime(finished, started, units = "secs"))
  )
  if (!is.null(error)) result$error <- conditionMessage(error)
  result
}

run_stage <- function(label, action) {
  started <- Sys.time()
  cat(sprintf("\n=== %s ===\n  Início: %s\n",
              label, format(started, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")))
  write_run_status("running", current_stage = label)
  result <- tryCatch(
    force(action),
    error = function(error) {
      finished <- Sys.time()
      failure_state <- pipeline_error_state()
      stage_results[[length(stage_results) + 1L]] <<- stage_result(label, failure_state, started, finished, error)
      write_run_status(failure_state, conditionMessage(error))
      state_label <- if (identical(failure_state, "paused")) "pausada" else "interrompida"
      elapsed_minutes <- as.numeric(difftime(finished, started, units = "mins"))
      cat(sprintf("  Etapa %s após %.2f min.\n\n", state_label, elapsed_minutes))
      stop(error)
    }
  )
  finished <- Sys.time()
  stage_results[[length(stage_results) + 1L]] <<- stage_result(label, "completed", started, finished)
  write_run_status("running")
  cat(sprintf("  Etapa concluída em %.2f min.\n\n",
              as.numeric(difftime(finished, started, units = "mins"))))
  invisible(result)
}

render_site <- function() {
  if (!nzchar(Sys.which("quarto"))) stop("Quarto não encontrado no PATH; ele é necessário para gerar o site.")
  exit_status <- system2("quarto", "render")
  if (!identical(as.integer(exit_status), 0L)) stop(sprintf("Quarto terminou com código %d.", exit_status))
  invisible(TRUE)
}

assert_collection_ready <- function() {
  assert_current_selection_valid(sample_path())
  sample <- read_sample(sample_path())
  checkpoint <- raw_checkpoint_path()
  if (!file.exists(checkpoint)) {
    stop("A coleta ainda não foi executada. Use Rscript main.R --select.", call. = FALSE)
  }
  assert_checkpoint_complete(sample, checkpoint, sha256_file(sample_path()))
  cat(sprintf("  Coleta validada: %d repositórios prontos para análise.\n", nrow(sample)))
  invisible(TRUE)
}

collect_current_sample <- function() {
  assert_current_selection_valid(sample_path())
  run_collection(list(
    input = sample_path(),
    output = dirname(raw_checkpoint_path()),
    fresh = TRUE
  ))
}

pipeline_actions <- list(
  selection = function() run_selection(list(output = sample_path())),
  collection = collect_current_sample,
  collection_check = assert_collection_ready,
  analysis = function() run_analysis(list(
    input = sample_path(), raw = raw_checkpoint_path(), output = output_root_path()
  )),
  site = render_site
)

execute_pipeline <- function(mode) {
  stages <- pipeline_stages(mode)
  for (index in seq_along(stages)) {
    stage <- stages[[index]]
    label <- sprintf("Etapa %d/%d — %s", index, length(stages), STAGE_LABELS[[stage]])
    run_stage(label, pipeline_actions[[stage]]())
  }
  invisible(TRUE)
}

run_app <- function() {
  acquire_file_lock(
    run_lock_path,
    timeout_seconds = 0,
    stale_seconds = as.numeric(setting("api", "lock_stale_seconds", 300)),
    metadata = list(mode = main_options$mode, project_root = project_root)
  )
  tryCatch({
    write_run_status("running")
    execute_pipeline(main_options$mode)
    write_run_status("completed")
    cat("\n=== RESUMO DA EXECUÇÃO ===\n")
    for (result in stage_results) {
      cat(sprintf("  [OK] %s — %.2f min\n", result$stage, result$elapsed_seconds / 60))
    }
    total_minutes <- as.numeric(difftime(Sys.time(), run_started, units = "mins"))
    cat(sprintf("  Resultado: concluído\n  Tempo total: %.2f min\n  Detalhes: %s\n",
                total_minutes, project_relative(status_path)))
  }, error = function(error) {
    state <- pipeline_error_state()
    write_run_status(state, conditionMessage(error))
    cat(sprintf("\nExecução %s.\n  Detalhes: %s\n",
                if (identical(state, "paused")) "pausada" else "interrompida",
                project_relative(status_path)))
    stop(conditionMessage(error), call. = FALSE)
  }, finally = {
    release_file_lock(run_lock_path)
  })
}

run_app()

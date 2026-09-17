#!/usr/bin/env Rscript
## Orquestra seleção opcional, coleta e análise em sequência.
## Sem --select, a amostra versionada é preservada e o checkpoint pode ser usado offline.

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1L]]) else file.path("scripts", "run_pipeline.R")
project_root <- if (file.exists(script_path)) normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE) else getwd()
setwd(project_root)

source(file.path(project_root, "functions", "select_sample.R"))
run_selection_function <- run_selection
source(file.path(project_root, "functions", "collect.R"))
run_collection_function <- run_collection
source(file.path(project_root, "functions", "analyze.R"))
run_analysis_function <- run_analysis

parse_pipeline_options <- function(args) {
  values <- list(input = sample_path(), workers = 1L, output = output_root_path(), select = FALSE)
  index <- 1L
  while (index <= length(args)) {
    option <- args[[index]]
    if (identical(option, "--select")) {
      values$select <- TRUE
      index <- index + 1L
    } else if (option %in% c("--input", "--workers", "--output") && index < length(args)) {
      name <- sub("^--", "", option)
      value <- args[[index + 1L]]
      values[[name]] <- if (name == "workers") max(1L, as.integer(value)) else value
      index <- index + 2L
    } else {
      stop("Uso: Rscript scripts/run_pipeline.R [--select] [--input arquivo.csv] [--workers N] [--output diretório]")
    }
  }
  values
}

options_pipeline <- parse_pipeline_options(commandArgs(trailingOnly = TRUE))
if (options_pipeline$select) {
  run_selection_function(list(input = "", output = options_pipeline$input))
}
raw_directory <- dirname(raw_checkpoint_path())
run_collection_function(list(input = options_pipeline$input, output = raw_directory, workers = options_pipeline$workers))
run_analysis_function(list(input = options_pipeline$input, raw = raw_checkpoint_path(), output = options_pipeline$output))

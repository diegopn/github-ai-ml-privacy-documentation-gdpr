## Executa coleta e análise em sequência.

source(file.path("R", "collect.R"))
run_collection_function <- run_collection
source(file.path("R", "analyze.R"))
run_analysis_function <- run_analysis

parse_pipeline_options <- function(args) {
  values <- list(input = file.path("data", "repositorios_selecionados.csv"), workers = 1L, output = "outputs")
  index <- 1L
  while (index <= length(args)) {
    option <- args[[index]]
    if (option %in% c("--input", "--workers", "--output") && index < length(args)) {
      name <- sub("^--", "", option)
      value <- args[[index + 1L]]
      values[[name]] <- if (name == "workers") max(1L, as.integer(value)) else value
      index <- index + 2L
    } else stop("Uso: Rscript R/run_pipeline.R [--input arquivo.csv] [--workers N] [--output diretório]")
  }
  values
}

if (sys.nframe() == 0L) {
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  project_dir <- if (length(script_arg)) normalizePath(file.path(dirname(sub("^--file=", "", script_arg[[1L]])), ".."), mustWork = TRUE) else getwd()
  setwd(project_dir)
  options_pipeline <- parse_pipeline_options(commandArgs(trailingOnly = TRUE))
  raw_directory <- file.path("data", "raw")
  run_collection_function(list(input = options_pipeline$input, output = raw_directory, workers = options_pipeline$workers))
  run_analysis_function(list(input = options_pipeline$input, raw = file.path(raw_directory, "repository_results.jsonl"), output = options_pipeline$output))
}

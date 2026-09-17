## Atalho para executar a análise completa a partir do checkpoint coletado.

source(file.path("R", "analyze.R"))

if (sys.nframe() == 0L) {
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  project_dir <- if (length(script_arg)) normalizePath(file.path(dirname(sub("^--file=", "", script_arg[[1L]])), ".."), mustWork = TRUE) else getwd()
  setwd(project_dir)
  run_analysis(parse_options(commandArgs(trailingOnly = TRUE)))
}

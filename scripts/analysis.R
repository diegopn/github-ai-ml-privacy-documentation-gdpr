#!/usr/bin/env Rscript
## Recalcula a análise a partir dos dados versionados.

# Descobre a raiz do projeto.
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1L]]) else file.path("scripts", "analysis.R")
project_root <- if (file.exists(script_path)) normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE) else getwd()
setwd(project_root)
# Carrega a análise e repassa os argumentos.
source(file.path(project_root, "functions", "analyze.R"))
run_analysis(parse_options(commandArgs(trailingOnly = TRUE)))

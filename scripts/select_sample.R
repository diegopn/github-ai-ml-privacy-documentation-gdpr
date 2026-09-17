#!/usr/bin/env Rscript
## Gera ou revalida a amostra usando os critérios definidos em settings.yml.

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1L]]) else file.path("scripts", "select_sample.R")
project_root <- if (file.exists(script_path)) normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE) else getwd()
setwd(project_root)
source(file.path(project_root, "functions", "select_sample.R"))
run_selection(parse_selection_options(commandArgs(trailingOnly = TRUE)))

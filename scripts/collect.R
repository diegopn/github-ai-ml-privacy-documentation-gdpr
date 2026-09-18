#!/usr/bin/env Rscript
## Coleta os snapshots e grava o checkpoint JSONL.

# Descobre a raiz do projeto.
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1L]]) else file.path("scripts", "collect.R")
project_root <- if (file.exists(script_path)) normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE) else getwd()
setwd(project_root)
# Carrega a coleta e repassa as opções.
source(file.path(project_root, "functions", "collect.R"))
run_collection(parse_options(commandArgs(trailingOnly = TRUE)))

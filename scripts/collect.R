#!/usr/bin/env Rscript
## Ponto de entrada da coleta histórica: grava cada repositório no checkpoint
## JSONL e retoma registros compatíveis quando a execução é interrompida.

# Descobre a raiz do projeto antes de carregar as funções compartilhadas.
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1L]]) else file.path("scripts", "collect.R")
project_root <- if (file.exists(script_path)) normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE) else getwd()
setwd(project_root)
# Carrega a coleta e repassa as opções de entrada, saída e trabalhadores.
source(file.path(project_root, "functions", "collect.R"))
run_collection(parse_options(commandArgs(trailingOnly = TRUE)))

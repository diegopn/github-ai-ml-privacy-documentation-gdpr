#!/usr/bin/env Rscript
## Ponto de entrada para recalcular a análise a partir do CSV e do checkpoint
## versionados, sem realizar novas chamadas à API do GitHub.

# Descobre a raiz a partir do próprio arquivo para que o script funcione
# mesmo quando é chamado fora do diretório do projeto.
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1L]]) else file.path("scripts", "analysis.R")
project_root <- if (file.exists(script_path)) normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE) else getwd()
setwd(project_root)
# Carrega as funções da análise e encaminha os argumentos para o pipeline offline.
source(file.path(project_root, "functions", "analyze.R"))
run_analysis(parse_options(commandArgs(trailingOnly = TRUE)))

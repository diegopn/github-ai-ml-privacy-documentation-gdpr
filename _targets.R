# Pipeline declarativo opcional para atualizar os artefatos sem repetir etapas.
library(targets)

source("R/select_sample.R")
source("R/collect.R")
source("R/analyze.R")

tar_option_set(packages = c("jsonlite", "knitr"))

list(
  tar_target(sample_csv, "data/repositorios_selecionados.csv", format = "file"),
  tar_target(raw_checkpoint, "data/raw/repository_results.jsonl", format = "file"),
  tar_target(
    analysis_outputs,
    {
      # A análise lê o checkpoint e devolve todos os artefatos principais.
      run_analysis(list(input = sample_csv, raw = raw_checkpoint, output = "outputs"))
      c(
        "outputs/dataset_final_privacidade_gdpr.csv",
        "outputs/evidencias_positivas.csv",
        "outputs/estatisticas.json",
        "outputs/resultados_estatisticos.md",
        "outputs/experimento_privacidade_gdpr.md",
        "outputs/manifest.json"
      )
    },
    format = "file"
  )
)

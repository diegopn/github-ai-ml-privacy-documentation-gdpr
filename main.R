#!/usr/bin/env Rscript

file_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
main_file <- if (length(file_argument)) sub("^--file=", "", file_argument[[1L]]) else file.path(getwd(), "main.R")
if (!file.exists(main_file)) main_file <- file.path(getwd(), main_file)
project_root <- normalizePath(dirname(main_file), mustWork = TRUE)
setwd(project_root)

source(file.path(project_root, "src", "bootstrap.R"), local = .GlobalEnv)
bootstrap_project(project_root)

options <- parse_main_options(commandArgs(trailingOnly = TRUE))
if (isTRUE(options$help)) {
  cat(usage_text(), "\n")
  quit(save = "no", status = 0L, runLast = FALSE)
}

config <- PROJECT_CONFIG
store <- ArtifactStore$new(config)
client <- GitHubClient$new(config)
classifier <- PrivacyDocumentClassifier$new()
builder <- DatasetBuilder$new(config, classifier)
writer <- ReportWriter$new(config, store)
selector <- SampleSelector$new(config, client, store)
collector <- RepositoryCollector$new(config, client, store, classifier)
analyzer <- ExperimentAnalyzer$new(config, store, builder, writer)
runner <- ExperimentRunner$new(config, store, selector, collector, analyzer)
runner$run(options$mode)

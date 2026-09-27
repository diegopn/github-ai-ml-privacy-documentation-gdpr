#!/usr/bin/env Rscript

local({
  file_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  main_file <- if (length(file_argument)) sub("^--file=", "", file_argument[[1L]]) else file.path(getwd(), "main.R")
  if (!file.exists(main_file)) main_file <- file.path(getwd(), main_file)
  project_root <- normalizePath(dirname(main_file), mustWork = TRUE)
  setwd(project_root)

  bootstrap_namespace <- new.env(parent = globalenv())
  sys.source(file.path(project_root, "src", "bootstrap.R"), envir = bootstrap_namespace)
  project <- bootstrap_namespace$ProjectBootstrap$new(project_root)$build()
  options <- project$cli$parse_main_options(commandArgs(trailingOnly = TRUE))
  if (isTRUE(options$help)) {
    cat(project$cli$usage_text(), "\n")
    quit(save = "no", status = 0L, runLast = FALSE)
  }
  if (identical(options$mode, "test")) {
    test_namespace <- new.env(parent = globalenv())
    sys.source(file.path(project_root, "tests", "test_contracts.R"), envir = test_namespace)
    test_namespace$ContractTestRunner$new()$run(project_root)
  } else if (identical(options$mode, "prepare-site")) {
    project$services$site_content$write()
  } else {
    project$runner$run(options$mode)
  }
})

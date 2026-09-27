ContractTestRunner <- R6::R6Class(
  "ContractTestRunner",
  public = list(
    run = function(project_root = getwd()) {
      project_root <- normalizePath(project_root, mustWork = TRUE)
      previous_directory <- getwd()
      on.exit(setwd(previous_directory), add = TRUE)
      setwd(project_root)
      namespace <- new.env(parent = globalenv())
      support <- list.files(file.path(project_root, "tests", "support"), pattern = "\\.R$", full.names = TRUE)
      base <- support[basename(support) == "mock_github_client_base.R"]
      contracts <- list.files(file.path(project_root, "tests", "contracts"), pattern = "\\.R$", full.names = TRUE)
      for (file in c(base, setdiff(support, base), contracts)) sys.source(file, envir = namespace)
      context <- namespace$ContractTestContext$new(project_root)
      on.exit(context$cleanup(), add = TRUE)
      cases <- lapply(private$contract_classes, function(class_name) namespace[[class_name]]$new())
      for (test_case in cases) test_case$run(context)
      context$summary()
      invisible(TRUE)
    }
  ),
  private = list(
    contract_classes = c(
      "ConfigurationCliContract",
      "ClassifierContract",
      "SelectorContract",
      "ApiClientContract",
      "StorageContract",
      "ArtifactLockContract",
      "CollectorContract",
      "CollectionProtocolContract",
      "DatasetBuilderContract",
      "StatisticsContract",
      "ReportWriterContract",
      "SiteContentContract",
      "RunnerContract",
      "MainEntryContract",
      "ArchitectureContract"
    )
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

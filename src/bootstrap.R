bootstrap_project <- function(root) {
  if (!requireNamespace("R6", quietly = TRUE)) stop("Instale o pacote 'R6' para executar o projeto.", call. = FALSE)
  sys.source(file.path(root, "src", "shared", "scalar_converters.R"), envir = .GlobalEnv)
  sys.source(file.path(root, "src", "config", "project_config.R"), envir = .GlobalEnv)
  initialize_project_config(root)

  module_groups <- c("storage", "domain", "data", "clients", "selection", "collection", "analysis", "reporting", "app")
  for (group in module_groups) {
    directory <- file.path(root, "src", group)
    files <- sort(list.files(directory, pattern = "\\.R$", full.names = TRUE, recursive = TRUE))
    for (file in files) sys.source(file, envir = .GlobalEnv)
  }
  invisible(PROJECT_CONFIG)
}

ProjectConfig <- R6::R6Class(
  "ProjectConfig",
  public = list(
    root = NULL,
    settings_file = NULL,
    settings = NULL,
    initialize = function(root, settings_file = file.path("config", "settings.yml")) {
      self$root <- normalizePath(root, mustWork = TRUE)
      self$settings_file <- if (grepl("^(/|[A-Za-z]:[/\\\\])", settings_file)) {
        settings_file
      } else {
        file.path(self$root, settings_file)
      }
      if (!file.exists(self$settings_file)) stop(sprintf("Configuração não encontrada: %s", self$settings_file), call. = FALSE)
      if (!requireNamespace("yaml", quietly = TRUE)) stop("Instale o pacote 'yaml' para ler config/settings.yml.", call. = FALSE)
      if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Instale o pacote 'jsonlite' para executar o projeto.", call. = FALSE)
      if (!requireNamespace("R6", quietly = TRUE)) stop("Instale o pacote 'R6' para executar o projeto.", call. = FALSE)
      self$settings <- yaml::read_yaml(self$settings_file)
      private$validate()
    },
    get = function(section, key, default = NULL) {
      values <- self$settings[[section]] %||% list()
      value <- values[[key]]
      if (is.null(value) || !length(value)) default else value
    },
    path = function(name, default) {
      value <- self$get("paths", name, default)
      if (is.list(value)) value <- value[[1L]]
      value <- path.expand(as.character(value))
      if (grepl("^(/|[A-Za-z]:[/\\\\])", value)) value else file.path(self$root, value)
    },
    relative = function(path) {
      absolute <- normalizePath(path, mustWork = FALSE)
      prefix <- paste0(self$root, .Platform$file.sep)
      if (startsWith(absolute, prefix)) substring(absolute, nchar(prefix) + 1L) else absolute
    },
    output_paths = function(root = self$path("output_root", "outputs")) {
      list(root = root, tables = file.path(root, "tables"), figures = file.path(root, "figures"),
           reports = file.path(root, "reports"), metadata = file.path(root, "metadata"))
    }
  ),
  private = list(
    validate = function() {
      required <- c("project", "paths", "analysis", "api", "selection")
      missing <- setdiff(required, names(self$settings))
      if (length(missing)) stop(sprintf("Seções ausentes em config/settings.yml: %s", paste(missing, collapse = ", ")), call. = FALSE)
      if (!length(self$settings$selection$topics)) stop("A configuração precisa conter ao menos um tópico de seleção.", call. = FALSE)
      if (is.na(as.numeric(self$get("analysis", "alpha", 0.05)))) stop("analysis.alpha precisa ser numérico.", call. = FALSE)
      invisible(TRUE)
    }
  )
)

PROJECT_CONFIG <- NULL

initialize_project_config <- function(root) {
  PROJECT_CONFIG <<- ProjectConfig$new(root)
  PRE_UNTIL <<- as.character(PROJECT_CONFIG$get("analysis", "pre_until", "2018-05-24T23:59:59Z"))
  POST_UNTIL <<- as.character(PROJECT_CONFIG$get("analysis", "post_until", "2026-06-30T23:59:59Z"))
  ALPHA <<- as.numeric(PROJECT_CONFIG$get("analysis", "alpha", 0.05))
  invisible(PROJECT_CONFIG)
}

setting <- function(section, key, default = NULL) {
  if (is.null(PROJECT_CONFIG)) stop("ProjectConfig precisa ser inicializada antes de carregar os módulos.", call. = FALSE)
  PROJECT_CONFIG$get(section, key, default)
}

project_path <- function(...) file.path(PROJECT_CONFIG$root, ...)
resolve_project_path <- function(path) {
  path <- scalar_text(path, "")
  if (!nzchar(path)) return(path)
  if (grepl("^~", path)) return(path.expand(path))
  if (grepl("^(/|[A-Za-z]:[/\\\\])", path)) path else project_path(path)
}
project_relative <- function(path) PROJECT_CONFIG$relative(path)
path_setting <- function(name, default) resolve_project_path(setting("paths", name, default))
sample_path <- function() path_setting("sample", "inputs/final/selected_repositories.csv")
sample_hash_path <- function() path_setting("sample_hash", "inputs/final/published_sample.sha256.txt")
raw_checkpoint_path <- function() path_setting("raw_checkpoint", "inputs/raw/repository_results.jsonl")
reference_spdx_path <- function() path_setting("reference_spdx", "inputs/reference/osi_approved_spdx_ids.txt")
selection_audit_path <- function() path_setting("selection_audit", "outputs/metadata/selection_search_manifest.json")
output_root_path <- function() path_setting("output_root", "outputs")
output_paths <- function(output = output_root_path()) PROJECT_CONFIG$output_paths(resolve_project_path(output))

COLLECTOR_PROTOCOL_VERSION <- "open-source-no-size-limit-expanded-topics-2026-09"
COLLECTOR_PROTOCOL_COMPATIBLE_VERSIONS <- c(COLLECTOR_PROTOCOL_VERSION, "open-source-no-size-limit-2026-09")
RULE_VERSION <- "semantic-conservative-2026-09-c1-c4"

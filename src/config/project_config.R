ProjectConfig <- R6::R6Class(
  "ProjectConfig",
  public = list(
    initialize = function(root, values, settings_file = file.path("config", "settings.yml")) {
      if (!inherits(values, "ValueTools")) stop("ProjectConfig exige ValueTools.", call. = FALSE)
      if (!is.character(root) || length(root) != 1L || !dir.exists(root)) {
        stop("A raiz do projeto precisa ser uma pasta existente.", call. = FALSE)
      }
      private$values <- values
      private$project_root <- normalizePath(root, mustWork = TRUE)
      private$config_file <- if (grepl("^(/|[A-Za-z]:[/\\\\])", settings_file)) {
        settings_file
      } else {
        file.path(private$project_root, settings_file)
      }
      if (!file.exists(private$config_file)) {
        stop(sprintf("Configuração não encontrada: %s", private$config_file), call. = FALSE)
      }
      if (!requireNamespace("yaml", quietly = TRUE)) stop("Instale o pacote 'yaml' para ler config/settings.yml.", call. = FALSE)
      if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Instale o pacote 'jsonlite' para executar o projeto.", call. = FALSE)
      private$settings <- yaml::read_yaml(private$config_file)
      private$validate()
    },
    root = function() private$project_root,
    analysis_cutoffs = function() {
      c(pre = as.character(self$get("analysis", "pre_until", "2018-05-24T23:59:59Z")),
        post = as.character(self$get("analysis", "post_until", "2026-06-30T23:59:59Z")))
    },
    selection_settings = function() {
      gdpr_date <- as.character(self$get("selection", "gdpr_date", "2018-05-25T00:00:00Z"))
      list(
        gdpr_date = gdpr_date,
        topics = as.character(unlist(self$get("selection", "topics", character()), use.names = FALSE)),
        created_start = as.character(self$get("selection", "search_start_date", "2008-01-01")),
        created_end = format(as.Date(substr(gdpr_date, 1L, 10L)) - 1L, "%Y-%m-%d"),
        min_stars = as.integer(self$get("selection", "min_stars", 500L)),
        min_issues = as.integer(self$get("selection", "min_issues", 100L)),
        min_activity_months = as.numeric(self$get("selection", "min_activity_months", 24)),
        page_size = 100L,
        max_results_per_query = as.integer(self$get("selection", "max_results_per_query", 1000L)),
        protocol_version = "topic-expanded-partitioned-2026-09",
        search_interval_seconds = as.numeric(self$get("api", "search_interval_seconds", 2.2))
      )
    },
    selection_configuration = function(settings = self$selection_settings()) {
      list(
        protocol_version = settings$protocol_version,
        topics = settings$topics,
        gdpr_date = settings$gdpr_date,
        search_start_date = settings$created_start,
        search_end_date = settings$created_end,
        search_page_size = settings$page_size,
        max_results_per_query = settings$max_results_per_query,
        min_stars = settings$min_stars,
        min_issues = settings$min_issues,
        min_activity_months = settings$min_activity_months
      )
    },
    get = function(section, key, default = NULL) {
      values <- private$settings[[section]]
      if (!is.list(values)) values <- list()
      value <- values[[key]]
      if (!length(value)) default else value
    },
    path = function(name, default = NULL) {
      value <- self$get("paths", name, default)
      if (!length(value)) stop(sprintf("Caminho não configurado: paths.%s", name), call. = FALSE)
      self$resolve(value)
    },
    resolve = function(path) {
      path <- private$values$scalar_text(path, "")
      if (!nzchar(path)) return(path)
      if (grepl("^~", path)) return(path.expand(path))
      if (grepl("^(/|[A-Za-z]:[/\\\\])", path)) path else file.path(private$project_root, path)
    },
    relative = function(path) {
      absolute <- normalizePath(path, mustWork = FALSE)
      prefix <- paste0(private$project_root, .Platform$file.sep)
      if (identical(absolute, private$project_root)) "." else if (startsWith(absolute, prefix)) {
        substring(absolute, nchar(prefix) + 1L)
      } else absolute
    },
    output_paths = function(root = self$path("output_root", "outputs")) {
      list(root = root, tables = file.path(root, "tables"), figures = file.path(root, "figures"),
           reports = file.path(root, "reports"), metadata = file.path(root, "metadata"))
    }
  ),
  private = list(
    values = NULL,
    project_root = NULL,
    config_file = NULL,
    settings = NULL,
    validate = function() {
      required <- c("project", "paths", "analysis", "api", "selection")
      missing <- setdiff(required, names(private$settings))
      if (length(missing)) stop(sprintf("Seções ausentes em config/settings.yml: %s", paste(missing, collapse = ", ")), call. = FALSE)
      for (section in required) {
        if (!is.list(private$settings[[section]])) {
          stop(sprintf("A seção %s precisa ser um mapa de configurações.", section), call. = FALSE)
        }
      }
      private$validate_paths()
      alpha <- private$numeric_value(private$settings$analysis$alpha, "analysis.alpha")
      if (alpha <= 0 || alpha >= 1) stop("analysis.alpha precisa estar entre 0 e 1.", call. = FALSE)
      topics <- private$settings$selection$topics
      if (!is.character(topics) || !length(topics) || anyNA(topics) || any(!nzchar(trimws(topics)))) {
        stop("A configuração precisa conter tópicos de seleção não vazios.", call. = FALSE)
      }
      private$validate_numeric_settings()
      private$validate_dates()
      private$validate_consistency()
      invisible(TRUE)
    },
    validate_paths = function() {
      required <- c("sample", "sample_hash", "raw_checkpoint", "reference_spdx", "selection_audit", "output_root")
      missing <- setdiff(required, names(private$settings$paths))
      if (length(missing)) stop(sprintf("Caminhos ausentes: %s", paste(missing, collapse = ", ")), call. = FALSE)
      for (key in names(private$settings$paths)) {
        value <- private$settings$paths[[key]]
        if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(trimws(value))) {
          stop(sprintf("paths.%s precisa conter um único caminho não vazio.", key), call. = FALSE)
        }
      }
    },
    numeric_value = function(value, name) {
      if ((!is.numeric(value) && !is.character(value)) || length(value) != 1L || is.na(value)) {
        stop(sprintf("%s precisa conter um único número finito.", name), call. = FALSE)
      }
      parsed <- suppressWarnings(as.numeric(value))
      if (!is.finite(parsed)) stop(sprintf("%s precisa conter um único número finito.", name), call. = FALSE)
      parsed
    },
    validate_numeric_settings = function() {
      keys <- list(
        selection = c("min_stars", "min_issues", "min_activity_months", "max_results_per_query"),
        api = c("connect_timeout_seconds", "timeout_seconds", "max_attempts", "retry_base_seconds",
          "retry_max_seconds", "retry_budget_seconds", "max_rate_wait_seconds", "search_interval_seconds",
          "core_interval_seconds", "raw_interval_seconds", "graphql_interval_seconds",
          "lock_timeout_seconds", "lock_stale_seconds")
      )
      zero_allowed <- c("retry_budget_seconds", "max_rate_wait_seconds", "search_interval_seconds",
        "core_interval_seconds", "raw_interval_seconds", "graphql_interval_seconds", "lock_timeout_seconds")
      integer_keys <- c("min_stars", "min_issues", "max_results_per_query", "max_attempts")
      for (section in names(keys)) {
        for (key in intersect(keys[[section]], names(private$settings[[section]]))) {
          name <- paste(section, key, sep = ".")
          value <- private$numeric_value(private$settings[[section]][[key]], name)
          if (value < 0 || (value == 0 && !key %in% zero_allowed)) {
            stop(sprintf("%s está fora do intervalo permitido.", name), call. = FALSE)
          }
          if (key %in% integer_keys && (value != floor(value) || value > .Machine$integer.max)) {
            stop(sprintf("%s precisa ser um inteiro representável em R.", name), call. = FALSE)
          }
        }
      }
    },
    validate_dates = function() {
      keys <- list(analysis = c("pre_until", "post_until"), selection = c("gdpr_date", "search_start_date"))
      for (section in names(keys)) {
        for (key in intersect(keys[[section]], names(private$settings[[section]]))) {
          value <- private$settings[[section]][[key]]
          if (!private$is_valid_date(value)) {
            stop(sprintf("%s.%s precisa ser uma data válida em YYYY-MM-DD ou YYYY-MM-DDTHH:MM:SSZ.",
              section, key), call. = FALSE)
          }
        }
      }
    },
    validate_consistency = function() {
      cutoffs <- self$analysis_cutoffs()
      instants <- ifelse(nchar(cutoffs) == 10L, paste0(cutoffs, "T00:00:00Z"), cutoffs)
      if (instants[["pre"]] >= instants[["post"]]) {
        stop("analysis.pre_until precisa ser anterior a analysis.post_until.", call. = FALSE)
      }
      selection <- self$selection_settings()
      if (as.Date(selection$created_start) > as.Date(selection$created_end)) {
        stop("selection.search_start_date precisa ser anterior a selection.gdpr_date.", call. = FALSE)
      }
      if (selection$max_results_per_query > 1000L) {
        stop("selection.max_results_per_query não pode exceder o limite de 1000 da Search API.", call. = FALSE)
      }
      if (as.numeric(self$get("api", "retry_max_seconds", 60)) < as.numeric(self$get("api", "retry_base_seconds", 2))) {
        stop("api.retry_max_seconds precisa ser maior ou igual a api.retry_base_seconds.", call. = FALSE)
      }
      if (as.numeric(self$get("api", "lock_stale_seconds", 300)) < 5) {
        stop("api.lock_stale_seconds precisa ser pelo menos 5.", call. = FALSE)
      }
    },
    is_valid_date = function(value) {
      if (!is.character(value) || length(value) != 1L || is.na(value)) return(FALSE)
      date_format <- if (nchar(value) == 10L) "%Y-%m-%d" else "%Y-%m-%dT%H:%M:%SZ"
      parsed <- as.POSIXct(value, format = date_format, tz = "UTC")
      identical(format(parsed, format = date_format, tz = "UTC"), value)
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

GitHubClient <- R6::R6Class(
  "GitHubClient",
  public = list(
    initialize = function(config, values, http_transport) {
      if (!inherits(config, "ProjectConfig")) stop("GitHubClient exige ProjectConfig.", call. = FALSE)
      if (!inherits(values, "ValueTools")) stop("GitHubClient exige ValueTools.", call. = FALSE)
      if (!inherits(http_transport, "GitHubHttpTransport")) {
        stop("GitHubClient exige GitHubHttpTransport.", call. = FALSE)
      }
      private$config <- config
      private$values <- values
      private$http_transport <- http_transport
      invisible(self)
    },
    assert_authenticated = function() {
      if (!nzchar(private$token())) stop("GITHUB_TOKEN não encontrado no ambiente ou em .env.", call. = FALSE)
      invisible(TRUE)
    },
    api = function(path, params = list(), return_headers = FALSE) {
      url <- private$api_url(path, params)
      resource <- if (grepl("/search/", path, fixed = TRUE)) "search" else "core"
      private$http_transport$request_json(url, resource, return_headers = return_headers, token = private$token())
    },
    graphql = function(query, variables = list()) {
      base <- sub("/$", "", as.character(private$config$get("api", "github_graphql_base", "https://api.github.com/graphql")))
      payload <- jsonlite::toJSON(list(query = query, variables = variables), auto_unbox = TRUE, null = "null", digits = 16)
      response <- private$http_transport$request_json(base, "graphql", "POST", payload, token = private$token())
      private$interpret_graphql_response(response)
    },
    raw = function(repository, sha, path) {
      base <- sub("/$", "", as.character(private$config$get("api", "raw_base", "https://raw.githubusercontent.com")))
      url <- paste0(base, "/", repository, "/", sha, "/", private$encode_path(path))
      response <- private$http_transport$request_text(url, "raw", token = "")
      list(status = response$status, text = if (response$status == 200L) response$body else "",
           note = if (response$status == 200L) "" else response$error,
           rate = private$values$or_else(response$rate, list()), rate_limited = isTRUE(response$rate_limited))
    },
    response_ok = function(response) {
      if (!is.list(response)) return(FALSE)
      status <- tryCatch(suppressWarnings(as.integer(response$status)), error = function(...) NA_integer_)
      length(status) == 1L && is.finite(status) && status >= 200L && status < 300L &&
        !isTRUE(response$rate_limited) && !nzchar(private$values$scalar_text(response$error, ""))
    },
    response_condition = function(response, fallback_message = NULL) {
      default_message <- sprintf("GitHub API HTTP %s", private$values$scalar_text(response$status, "?"))
      message <- private$values$scalar_text(response$error, private$values$or_else(fallback_message, default_message))
      if (!nzchar(message)) message <- private$values$or_else(fallback_message, default_message)
      classes <- if (isTRUE(response$rate_limited)) c("github_rate_limit_error", "github_api_error") else "github_api_error"
      errorCondition(message, class = classes, status = response$status,
        rate_limited = isTRUE(response$rate_limited), rate = private$values$or_else(response$rate, list()))
    },
    is_rate_limit_error = function(error) inherits(error, "github_rate_limit_error")
  ),
  private = list(
    config = NULL,
    values = NULL,
    http_transport = NULL,
    api_url = function(path, params) {
      if (!is.list(params) || (length(params) && (is.null(names(params)) || any(!nzchar(names(params)))))) {
        stop("Os parâmetros da API precisam ser uma lista nomeada.", call. = FALSE)
      }
      query <- if (length(params)) paste(vapply(names(params), function(name) {
        value <- params[[name]]
        if (length(value) != 1L || is.na(value)) stop(sprintf("Parâmetro da API não escalar: %s", name), call. = FALSE)
        paste0(utils::URLencode(name, reserved = TRUE, repeated = TRUE), "=", utils::URLencode(as.character(value), reserved = TRUE, repeated = TRUE))
      }, character(1L)), collapse = "&") else ""
      base <- sub("/$", "", as.character(private$config$get("api", "github_base", "https://api.github.com")))
      url <- paste0(base, path, if (nzchar(query)) paste0("?", query) else "")
      url
    },
    interpret_graphql_response = function(response) {
      if (!self$response_ok(response)) return(response)
      body <- response$body
      if (!is.list(body)) return(private$invalid_graphql_response(response))
      errors <- private$values$or_else(body$errors, list())
      if (!length(errors)) {
        if (!is.list(body$data)) return(private$invalid_graphql_response(response))
        return(response)
      }
      valid_errors <- is.list(errors) && all(vapply(errors, is.list, logical(1L)))
      if (!valid_errors) {
        response$status <- 0L
        response$error <- "Resposta GraphQL contém errors em formato inválido."
        return(response)
      }
      messages <- vapply(errors, function(error) private$values$scalar_text(error$message, "erro GraphQL"), character(1L))
      limited <- vapply(errors, private$is_graphql_rate_limit, logical(1L))
      detail <- paste(unique(messages), collapse = "; ")
      response$rate_limited <- isTRUE(response$rate_limited) || any(limited)
      response$error <- if (nzchar(detail)) detail else "Falha na consulta GraphQL."
      if (isTRUE(response$rate_limited)) {
        private$http_transport$register_rate_limit("graphql", private$values$or_else(response$rate, list()))
      }
      response
    },
    invalid_graphql_response = function(response) {
      response$status <- 0L
      response$error <- "Resposta GraphQL em formato inválido: esperado um objeto data ou errors."
      response
    },
    is_graphql_rate_limit = function(error) {
      extensions <- if (is.list(error$extensions)) error$extensions else list()
      types <- c(private$values$scalar_text(error$type, ""), private$values$scalar_text(extensions$type, ""),
        private$values$scalar_text(extensions$code, ""))
      any(toupper(types) %in% c("RATE_LIMITED", "RATE_LIMIT", "SECONDARY_RATE_LIMIT"))
    },
    token = function() {
      token <- Sys.getenv("GITHUB_TOKEN", unset = "")
      if (!nzchar(token)) token <- private$read_dotenv_token(file.path(private$config$root(), ".env"))
      token
    },
    encode_path = function(path) {
      parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
      paste(vapply(parts, utils::URLencode, character(1L), reserved = TRUE, repeated = TRUE), collapse = "/")
    },
    read_dotenv_token = function(path) {
      if (!file.exists(path)) return("")
      lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
      lines <- sub("^[[:space:]]*export[[:space:]]+", "", lines)
      match <- grep("^[[:space:]]*GITHUB_TOKEN[[:space:]]*=", lines, value = TRUE)
      if (!length(match)) return("")
      token <- trimws(sub("^[[:space:]]*GITHUB_TOKEN[[:space:]]*=", "", match[[1L]]))
      if (nchar(token) >= 2L) {
        first <- substr(token, 1L, 1L)
        last <- substr(token, nchar(token), nchar(token))
        if (first %in% c("\"", "'") && identical(first, last)) {
          token <- substr(token, 2L, nchar(token) - 1L)
        }
      }
      token
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

api_rate_state_path <- function() {
  resolve_project_path(as.character(setting("api", "rate_state_path", "outputs/metadata/github_api_rate_state.json")))
}

api_rate_state_default <- function() {
  list(
    last_request = list(search = 0, core = 0, raw = 0, graphql = 0),
    not_before = list(search = 0, core = 0, raw = 0, graphql = 0),
    updated_at = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
  )
}

read_api_rate_state <- function(path = api_rate_state_path()) {
  if (!file.exists(path)) return(api_rate_state_default())
  state <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(...) NULL)
  if (!is.list(state)) return(api_rate_state_default())
  defaults <- api_rate_state_default()
  state$last_request <- utils::modifyList(defaults$last_request, state$last_request %||% list())
  state$not_before <- utils::modifyList(defaults$not_before, state$not_before %||% list())
  state
}

api_state_number <- function(state, section, resource) {
  value <- (state[[section]] %||% list())[[resource]]
  value <- suppressWarnings(as.numeric(value %||% 0))
  if (is.na(value)) 0 else value
}

api_rate_interval <- function(resource) {
  key <- switch(
    resource,
    search = "search_interval_seconds",
    raw = "raw_interval_seconds",
    graphql = "graphql_interval_seconds",
    "core_interval_seconds"
  )
  value <- suppressWarnings(as.numeric(setting("api", key, if (resource == "search") 2.2 else 0)))
  if (is.na(value) || value < 0) 0 else value
}

api_wait_for_slot <- function(resource = "core") {
  resource <- if (resource %in% c("search", "raw", "graphql")) resource else "core"
  interval <- api_rate_interval(resource)
  path <- api_rate_state_path()
  lock_path <- paste0(path, ".lock")
  lock_timeout <- as.numeric(setting("api", "lock_timeout_seconds", 30))
  stale_seconds <- as.numeric(setting("api", "lock_stale_seconds", 300))
  wait <- with_file_lock(lock_path, {
    state <- read_api_rate_state(path)
    now <- as.numeric(Sys.time())
    interval_until <- api_state_number(state, "last_request", resource) + interval
    rate_limit_until <- api_state_number(state, "not_before", resource)
    scheduled <- max(
      interval_until,
      rate_limit_until
    )
    delay <- max(0, scheduled - now)
    state$last_request[[resource]] <- max(now, scheduled)
    state$updated_at <- format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
    atomic_write_json(state, path, pretty = FALSE)
    list(
      delay = delay,
      interval_delay = max(0, interval_until - now),
      rate_limit_delay = max(0, rate_limit_until - now)
    )
  }, timeout_seconds = lock_timeout, stale_seconds = stale_seconds)
  delay <- wait$delay
  max_wait <- as.numeric(setting("api", "max_rate_wait_seconds", 7200))
  if (is.finite(max_wait) && delay > max_wait) {
    stop("Limite da API do GitHub ainda ativo; aguarde o reset antes de executar novamente.", call. = FALSE)
  }
  remaining <- delay
  report_wait <- wait$rate_limit_delay > wait$interval_delay + 0.5
  if (remaining > 0 && report_wait) {
    cat(sprintf("GitHub API (%s): limite ativo; aguardando renovação.\n", resource))
  }
  while (remaining > 0) {
    chunk <- min(30, remaining)
    Sys.sleep(chunk)
    remaining <- remaining - chunk
  }
  invisible(TRUE)
}

api_record_rate_state <- function(resource, headers, fallback_delay = 0) {
  if (!is.list(headers)) return(invisible(FALSE))
  remaining <- suppressWarnings(as.numeric(headers$github_remaining %||% NA_real_))
  reset <- suppressWarnings(as.numeric(headers$github_reset %||% NA_real_))
  retry_after <- suppressWarnings(as.numeric(headers$retry_after %||% NA_real_))
  fallback_delay <- suppressWarnings(as.numeric(fallback_delay %||% 0))
  if (is.na(fallback_delay) || fallback_delay < 0) fallback_delay <- 0
  if ((is.na(remaining) || remaining > 0) && (is.na(retry_after) || retry_after <= 0) && fallback_delay <= 0) {
    return(invisible(FALSE))
  }
  path <- api_rate_state_path()
  lock_path <- paste0(path, ".lock")
  with_file_lock(lock_path, {
    state <- read_api_rate_state(path)
    now <- as.numeric(Sys.time())
    current <- api_state_number(state, "not_before", resource)
    retry_at <- if (!is.na(retry_after) && retry_after > 0) now + retry_after + 2 else 0
    reset_at <- if ((is.na(retry_after) || retry_after <= 0) &&
                    !is.na(remaining) && remaining <= 0 &&
                    !is.na(reset) && reset > now) reset + 2 else 0
    fallback_at <- if (fallback_delay > 0) now + fallback_delay else 0
    state$not_before[[resource]] <- max(current, reset_at, retry_at, fallback_at)
    state$updated_at <- format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
    atomic_write_json(state, path, pretty = FALSE)
    invisible(TRUE)
  }, timeout_seconds = as.numeric(setting("api", "lock_timeout_seconds", 30)),
  stale_seconds = as.numeric(setting("api", "lock_stale_seconds", 300)))
  invisible(TRUE)
}

api_rate_retry_at <- function(resources = c("search", "core", "graphql")) {
  state <- read_api_rate_state()
  values <- vapply(resources, function(resource) api_state_number(state, "not_before", resource), numeric(1L))
  retry_at <- max(c(values, 0), na.rm = TRUE)
  if (!is.finite(retry_at) || retry_at <= as.numeric(Sys.time())) return("")
  format(as.POSIXct(retry_at, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

parse_curl_headers <- function(lines) {
  lines <- as.character(lines %||% character())
  lines <- lines[nzchar(trimws(lines))]
  status_lines <- grep("^HTTP/", lines, ignore.case = TRUE)
  if (length(status_lines) > 1L) lines <- lines[status_lines[[length(status_lines)]]:length(lines)]
  result <- list()
  for (line in lines) {
    match <- regexec("^([^:]+):[[:space:]]*(.*)$", line, perl = TRUE)
    captured <- regmatches(line, match)[[1L]]
    if (length(captured) < 3L) next
    result[[tolower(trimws(captured[[2L]]))]] <- trimws(captured[[3L]])
  }
  result
}

api_header <- function(headers, name) as.character(headers[[tolower(name)]] %||% "")

rate_headers <- function(headers) {
  list(
    github_remaining = api_header(headers, "x-ratelimit-remaining"),
    github_reset = api_header(headers, "x-ratelimit-reset"),
    github_resource = api_header(headers, "x-ratelimit-resource"),
    retry_after = api_header(headers, "retry-after"),
    request_id = api_header(headers, "x-github-request-id")
  )
}

api_error_detail <- function(body_text, curl_error = "") {
  parsed <- tryCatch(jsonlite::fromJSON(body_text, simplifyVector = FALSE), error = function(...) NULL)
  message <- if (is.list(parsed)) scalar_text(parsed$message, "") else ""
  detail <- if (nzchar(message)) message else trimws(gsub("[[:space:]]+", " ", body_text, perl = TRUE))
  if (!nzchar(detail)) detail <- trimws(gsub("[[:space:]]+", " ", curl_error, perl = TRUE))
  substr(detail, 1L, 500L)
}

api_response_is_rate_limited <- function(status, body_text, rate) {
  if (is.na(status)) return(FALSE)
  if (status == 429L) return(TRUE)
  remaining <- suppressWarnings(as.numeric(rate$github_remaining %||% NA_real_))
  retry_after <- suppressWarnings(as.numeric(rate$retry_after %||% NA_real_))
  message <- tolower(body_text %||% "")
  has_rate_message <- grepl("rate limit|secondary rate|abuse detection", message, perl = TRUE)
  if (status == 200L && grepl('"errors"', message, fixed = TRUE)) {
    return((!is.na(remaining) && remaining <= 0) || has_rate_message)
  }
  status == 403L && (
    (!is.na(remaining) && remaining <= 0) ||
      (!is.na(retry_after) && retry_after > 0) || has_rate_message
  )
}

api_response_is_retryable <- function(status, body_text, rate) {
  if (is.na(status) || status == 0L) return(TRUE)
  if (status %in% c(408L, 425L, 429L) || status >= 500L) return(TRUE)
  api_response_is_rate_limited(status, body_text, rate)
}

api_response_ok <- function(response) {
  is.list(response) && !is.null(response$status) && !is.na(response$status) &&
    response$status >= 200L && response$status < 300L &&
    !nzchar(response$error %||% "")
}

api_error_is_rate_limited <- function(error) {
  message <- if (inherits(error, "condition")) conditionMessage(error) else as.character(error %||% "")
  grepl("rate limit|secondary rate|abuse detection|limite da api|limite de requisi", message, ignore.case = TRUE, perl = TRUE)
}

api_retry_delay <- function(status, rate, attempt, rate_limited = FALSE) {
  retry_after <- suppressWarnings(as.numeric(rate$retry_after %||% NA_real_))
  if (!is.na(retry_after) && retry_after > 0) return(retry_after)
  remaining <- suppressWarnings(as.numeric(rate$github_remaining %||% NA_real_))
  reset <- suppressWarnings(as.numeric(rate$github_reset %||% NA_real_))
  if ((!is.na(remaining) && remaining <= 0 || isTRUE(rate_limited)) &&
      !is.na(reset) && reset > as.numeric(Sys.time())) {
    return(reset - as.numeric(Sys.time()) + 2)
  }
  base <- suppressWarnings(as.numeric(setting("api", "retry_base_seconds", 2)))
  ceiling <- suppressWarnings(as.numeric(setting("api", "retry_max_seconds", 60)))
  if (is.na(base) || base <= 0) base <- 2
  if (is.na(ceiling) || ceiling < base) ceiling <- 60
  if (isTRUE(rate_limited)) return(max(60, min(ceiling, base * 2 ^ max(0, attempt - 1L))))
  min(ceiling, base * 2 ^ max(0, attempt - 1L))
}

http_request_files <- function() {
  list(
    body = tempfile(fileext = ".body"),
    status = tempfile(fileext = ".status"),
    error = tempfile(fileext = ".error"),
    headers = tempfile(fileext = ".headers")
  )
}

curl_request_arguments <- function(url, token, method, body_json, connect_timeout, request_timeout, files) {
  headers <- c(
    "Accept: application/vnd.github+json",
    "X-GitHub-Api-Version: 2022-11-28",
    paste0("User-Agent: ", setting("api", "user_agent", "github-ai-ml-privacy-research"))
  )
  if (nzchar(token)) headers <- c(headers, paste0("Authorization: Bearer ", token))
  args <- c(
    "--silent", "--show-error", "--location", "--connect-timeout", as.character(connect_timeout),
    "--max-time", as.character(request_timeout)
  )
  method <- toupper(as.character(method %||% "GET"))
  if (!method %in% c("GET", "POST")) stop(sprintf("Método HTTP não suportado: %s", method), call. = FALSE)
  if (identical(method, "POST")) {
    args <- c(args, "--request", "POST", "--header", shQuote("Content-Type: application/json"))
    if (!is.null(body_json)) args <- c(args, "--data-raw", shQuote(as.character(body_json)))
  }
  # system2() monta a linha de comando do shell; valores com espaços
  # precisam ser protegidos para que cada cabeçalho permaneça um argumento.
  for (header in headers) args <- c(args, "--header", shQuote(header))
  c(
    args,
    "--dump-header", shQuote(files$headers), "--output", shQuote(files$body),
    "--write-out", shQuote("%{http_code}"), shQuote(url)
  )
}

perform_http_request_attempt <- function(url, token, resource, method, body_json,
                                         connect_timeout, request_timeout, files) {
  api_wait_for_slot(resource)
  args <- curl_request_arguments(url, token, method, body_json, connect_timeout, request_timeout, files)
  unlink(c(files$status, files$error, files$headers))
  exit_status <- tryCatch(
    system2("curl", args, stdout = files$status, stderr = files$error),
    error = function(...) 1L
  )
  status_text <- if (file.exists(files$status)) paste(readLines(files$status, warn = FALSE), collapse = "") else ""
  status <- suppressWarnings(as.integer(trimws(status_text)))
  error_text <- if (file.exists(files$error)) paste(readLines(files$error, warn = FALSE), collapse = " ") else ""
  bytes <- if (file.exists(files$body)) readBin(files$body, "raw", n = file.info(files$body)$size) else raw(0)
  body_text <- if (length(bytes)) iconv(rawToChar(bytes), from = "UTF-8", to = "UTF-8", sub = "") else ""
  raw_headers <- if (file.exists(files$headers)) readLines(files$headers, warn = FALSE) else character()
  rate <- rate_headers(parse_curl_headers(raw_headers))
  list(
    status = status,
    body_text = body_text,
    error_text = error_text,
    raw_headers = raw_headers,
    rate = rate,
    exit_status = exit_status
  )
}

http_response <- function(status, body_text, error_text, rate, parse_json, raw_headers, return_headers) {
  if (is.na(status)) status <- 0L
  parsed_body <- tryCatch(
    if (nzchar(body_text)) jsonlite::fromJSON(body_text, simplifyVector = FALSE) else list(),
    error = function(error) list()
  )
  body <- if (parse_json) parsed_body else body_text
  error <- if (status >= 200L && status < 300L) {
    ""
  } else if (status > 0L) {
    detail <- api_error_detail(body_text, error_text)
    paste0("HTTP ", status, if (nzchar(detail)) paste0(": ", detail) else "")
  } else {
    paste0("Falha de transporte", if (nzchar(error_text)) paste0(": ", api_error_detail("", error_text)) else "")
  }
  result <- list(status = status, body = body, error = error, rate = rate)
  if (return_headers) result$headers <- raw_headers
  result
}

sleep_before_http_retry <- function(delay) {
  remaining <- delay
  while (remaining > 0) {
    chunk <- min(60, remaining)
    Sys.sleep(chunk)
    remaining <- remaining - chunk
  }
}

# Coordena tentativas, limite compartilhado e resposta HTTP final.
http_request <- function(url, token = "", parse_json = TRUE, max_attempts = NULL,
                         return_headers = FALSE, resource = "core", method = "GET",
                         body_json = NULL) {
  max_attempts <- suppressWarnings(as.integer(max_attempts %||% setting("api", "max_attempts", 4L)))
  if (is.na(max_attempts) || max_attempts < 1L) max_attempts <- 1L
  retry_budget <- suppressWarnings(as.numeric(setting("api", "retry_budget_seconds", 180)))
  if (is.na(retry_budget) || retry_budget < 0) retry_budget <- 180
  connect_timeout <- suppressWarnings(as.numeric(setting("api", "connect_timeout_seconds", 30)))
  request_timeout <- suppressWarnings(as.numeric(setting("api", "timeout_seconds", 45)))
  if (is.na(connect_timeout) || connect_timeout <= 0) connect_timeout <- 30
  if (is.na(request_timeout) || request_timeout <= 0) request_timeout <- 45
  max_rate_wait <- suppressWarnings(as.numeric(setting("api", "max_rate_wait_seconds", 7200)))
  if (is.na(max_rate_wait) || max_rate_wait < 0) max_rate_wait <- 7200

  started <- Sys.time()
  files <- http_request_files()
  on.exit(unlink(unlist(files, use.names = FALSE)), add = TRUE)
  attempt <- 1L
  repeat {
    response <- perform_http_request_attempt(
      url, token, resource, method, body_json, connect_timeout, request_timeout, files
    )
    status <- response$status
    rate <- response$rate
    rate_limited <- api_response_is_rate_limited(status, response$body_text, rate)
    retryable <- api_response_is_retryable(status, response$body_text, rate) || response$exit_status != 0L
    delay <- if (retryable) api_retry_delay(status, rate, attempt, rate_limited) else 0
    api_record_rate_state(resource, rate, fallback_delay = if (rate_limited) delay else 0)
    elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    if (rate_limited && elapsed + delay <= max_rate_wait) next
    if (retryable && attempt < max_attempts && elapsed + delay <= retry_budget) {
      cat(sprintf(
        "GitHub API: resposta transitória HTTP %s; nova tentativa %d/%d.\n",
        ifelse(is.na(status), "transporte", status), attempt, max_attempts - 1L
      ))
      sleep_before_http_retry(delay)
      attempt <- attempt + 1L
      next
    }
    return(http_response(
      status, response$body_text, response$error_text, rate,
      parse_json, response$raw_headers, return_headers
    ))
  }
}

encode_path <- function(path) {
  parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
  paste(vapply(parts, utils::URLencode, character(1L), reserved = TRUE), collapse = "/")
}

GitHubClient <- R6::R6Class(
  "GitHubClient",
  public = list(
    config = NULL,
    initialize = function(config = PROJECT_CONFIG) self$config <- config,
    api = function(path, params = list(), token = "", return_headers = FALSE) {
      query <- if (length(params)) paste(vapply(names(params), function(name) {
        paste0(utils::URLencode(name, reserved = TRUE), "=", utils::URLencode(as.character(params[[name]]), reserved = TRUE))
      }, character(1L)), collapse = "&") else ""
      base <- sub("/$", "", as.character(self$config$get("api", "github_base", "https://api.github.com")))
      url <- paste0(base, path, if (nzchar(query)) paste0("?", query) else "")
      resource <- if (grepl("/search/", path, fixed = TRUE)) "search" else "core"
      http_request(url, token, parse_json = TRUE, return_headers = return_headers, resource = resource)
    },
    graphql = function(query, token = "", variables = list()) {
      base <- sub("/$", "", as.character(self$config$get("api", "github_graphql_base", "https://api.github.com/graphql")))
      payload <- jsonlite::toJSON(list(query = query, variables = variables), auto_unbox = TRUE, null = "null", digits = 16)
      response <- http_request(base, token, parse_json = TRUE, resource = "graphql", method = "POST", body_json = payload)
      errors <- response$body$errors %||% list()
      if (length(errors)) {
        messages <- vapply(errors, function(error) scalar_text(error$message, "erro GraphQL"), character(1L))
        detail <- paste(unique(messages), collapse = "; ")
        remaining <- suppressWarnings(as.numeric(response$rate$github_remaining %||% NA_real_))
        response$error <- if ((!is.na(remaining) && remaining <= 0) || nzchar(api_rate_retry_at())) paste0("GraphQL rate limit: ", detail) else detail
      }
      response
    },
    raw = function(repository, sha, path, token = "") {
      base <- sub("/$", "", as.character(self$config$get("api", "raw_base", "https://raw.githubusercontent.com")))
      url <- paste0(base, "/", repository, "/", sha, "/", encode_path(path))
      response <- http_request(url, token, parse_json = FALSE, resource = "raw")
      list(status = response$status, text = if (response$status == 200L) response$body else "",
           note = if (response$status == 200L) "" else response$error, rate = response$rate %||% list())
    }
  )
)

GITHUB_CLIENT <- NULL
set_github_client <- function(client) GITHUB_CLIENT <<- client

github_api <- function(path, params = list(), token = "", return_headers = FALSE) {
  if (is.null(GITHUB_CLIENT)) stop("GitHubClient não foi inicializado.", call. = FALSE)
  GITHUB_CLIENT$api(path, params, token, return_headers)
}
github_graphql <- function(query, token = "", variables = list()) {
  if (is.null(GITHUB_CLIENT)) stop("GitHubClient não foi inicializado.", call. = FALSE)
  GITHUB_CLIENT$graphql(query, token, variables)
}
github_raw <- function(repository, sha, path, token = "") {
  if (is.null(GITHUB_CLIENT)) stop("GitHubClient não foi inicializado.", call. = FALSE)
  GITHUB_CLIENT$raw(repository, sha, path, token)
}

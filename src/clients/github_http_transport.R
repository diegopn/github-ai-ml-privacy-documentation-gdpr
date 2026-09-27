GitHubHttpTransport <- R6::R6Class(
  "GitHubHttpTransport",
  public = list(
    initialize = function(config, limiter, runtime, values, transport = NULL) {
      if (!inherits(config, "ProjectConfig")) stop("GitHubHttpTransport exige ProjectConfig.", call. = FALSE)
      if (!inherits(limiter, "GitHubRateLimiter")) stop("GitHubHttpTransport exige GitHubRateLimiter.", call. = FALSE)
      if (!inherits(runtime, "R6") || !is.function(runtime$now) || !is.function(runtime$sleep)) {
        stop("GitHubHttpTransport exige um relógio R6 com now() e sleep().", call. = FALSE)
      }
      if (!inherits(values, "ValueTools")) stop("GitHubHttpTransport exige ValueTools.", call. = FALSE)
      if (!is.null(transport) && (!inherits(transport, "R6") || !is.function(transport$request))) {
        stop("O transporte HTTP precisa ser um objeto R6 com request().", call. = FALSE)
      }
      private$config <- config
      private$limiter <- limiter
      private$runtime <- runtime
      private$values <- values
      private$transport <- transport
      invisible(self)
    },
    request_json = function(url, resource, method = "GET", body_json = NULL, return_headers = FALSE, token = "") {
      response <- private$http_request(url, resource, method, body_json, token)
      body_text <- sub("^\ufeff", "", response$body_text)
      parsed <- private$parse_json_body(body_text)
      private$http_response(response, parsed, return_headers)
    },
    request_text = function(url, resource, method = "GET", body_json = NULL, return_headers = FALSE, token = "") {
      response <- private$http_request(url, resource, method, body_json, token)
      private$http_response(response, list(body = response$body_text, error = ""), return_headers)
    },
    register_rate_limit = function(resource, rate) {
      delay <- private$api_retry_delay(rate, attempt = 1L, rate_limited = TRUE)
      private$limiter$record(resource, rate, delay)
      invisible(delay)
    }
  ),
  private = list(
    config = NULL,
    limiter = NULL,
    runtime = NULL,
    values = NULL,
    transport = NULL,
    request_settings = function() {
      settings <- list(
        max_attempts = private$config$get("api", "max_attempts", 4L),
        retry_budget = private$config$get("api", "retry_budget_seconds", 180),
        connect_timeout = private$config$get("api", "connect_timeout_seconds", 30),
        request_timeout = private$config$get("api", "timeout_seconds", 45),
        max_rate_wait = private$config$get("api", "max_rate_wait_seconds", 7200)
      )
      settings <- lapply(settings, as.numeric)
      settings$max_attempts <- as.integer(settings$max_attempts)
      settings
    },
    parse_curl_headers = function(lines) {
      lines <- as.character(private$values$or_else(lines, character()))
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
    },
    rate_headers = function(headers) {
      list(
        github_remaining = private$values$scalar_text(headers[["x-ratelimit-remaining"]], ""),
        github_reset = private$values$scalar_text(headers[["x-ratelimit-reset"]], ""),
        github_resource = private$values$scalar_text(headers[["x-ratelimit-resource"]], ""),
        retry_after = private$values$scalar_text(headers[["retry-after"]], ""),
        request_id = private$values$scalar_text(headers[["x-github-request-id"]], "")
      )
    },
    api_error_detail = function(body_text, curl_error = "") {
      body_text <- sub("^\ufeff", "", body_text)
      parsed <- tryCatch(jsonlite::fromJSON(body_text, simplifyVector = FALSE), error = function(...) NULL)
      message <- if (is.list(parsed)) private$values$scalar_text(parsed$message, "") else ""
      detail <- if (nzchar(message)) message else trimws(gsub("[[:space:]]+", " ", body_text, perl = TRUE))
      if (!nzchar(detail)) detail <- trimws(gsub("[[:space:]]+", " ", curl_error, perl = TRUE))
      substr(detail, 1L, 500L)
    },
    parse_json_body = function(body_text) {
      parsed <- tryCatch(
        if (nzchar(body_text)) jsonlite::fromJSON(body_text, simplifyVector = FALSE) else list(),
        error = function(error) error
      )
      if (inherits(parsed, "error")) {
        list(body = list(), error = conditionMessage(parsed))
      } else {
        list(body = parsed, error = "")
      }
    },
    response_error_message = function(status, body_text, error_text, transport_error, json_error) {
      if (nzchar(json_error)) return(paste0("Resposta JSON inválida: ", json_error))
      if (transport_error || status == 0L) {
        detail <- private$api_error_detail("", error_text)
        return(paste0("Falha de transporte", if (nzchar(detail)) paste0(": ", detail) else ""))
      }
      if (status >= 200L && status < 300L) return("")
      detail <- private$api_error_detail(body_text, error_text)
      paste0("HTTP ", status, if (nzchar(detail)) paste0(": ", detail) else "")
    },
    response_is_rate_limited = function(status, rate) {
      if (is.na(status)) return(FALSE)
      if (status == 429L) return(TRUE)
      remaining <- private$values$scalar_number(rate$github_remaining)
      retry_after <- private$values$scalar_number(rate$retry_after)
      status == 403L && ((!is.na(remaining) && remaining <= 0) || (!is.na(retry_after) && retry_after > 0))
    },
    response_is_retryable = function(status, rate_limited = FALSE) {
      is.na(status) || status == 0L || status %in% c(408L, 425L, 429L) || status >= 500L || isTRUE(rate_limited)
    },
    api_retry_delay = function(rate, attempt, rate_limited) {
      retry_after <- private$values$scalar_number(rate$retry_after)
      if (!is.na(retry_after) && retry_after > 0) return(retry_after + if (isTRUE(rate_limited)) 2 else 0)
      remaining <- private$values$scalar_number(rate$github_remaining)
      reset <- private$values$scalar_number(rate$github_reset)
      now <- private$runtime$now()
      if ((!is.na(remaining) && remaining <= 0 || isTRUE(rate_limited)) && !is.na(reset) && reset > now) {
        return(reset - now + 2)
      }
      base <- suppressWarnings(as.numeric(private$config$get("api", "retry_base_seconds", 2)))
      ceiling <- suppressWarnings(as.numeric(private$config$get("api", "retry_max_seconds", 60)))
      backoff <- min(ceiling, base * 2^max(0, attempt - 1L))
      if (isTRUE(rate_limited)) max(60, backoff) else backoff
    },
    request_headers = function(token) {
      headers <- c(
        "Accept: application/vnd.github+json",
        "X-GitHub-Api-Version: 2022-11-28",
        paste0("User-Agent: ", private$config$get("api", "user_agent", "github-ai-ml-privacy-research"))
      )
      if (nzchar(token)) headers <- c(headers, paste0("Authorization: Bearer ", token))
      headers
    },
    send_transport_request = function(url, method, body_json, settings, token) {
      headers <- private$request_headers(token)
      response <- if (is.null(private$transport)) {
        private$default_curl_transport(url, method, body_json, settings$connect_timeout, settings$request_timeout, headers)
      } else {
        private$transport$request(url, method, body_json, settings$connect_timeout, settings$request_timeout, headers)
      }
      private$normalise_transport_response(response)
    },
    default_curl_transport = function(url, method, body_json, connect_timeout, request_timeout, headers) {
      files <- list(
        body = tempfile(fileext = ".body"),
        status = tempfile(fileext = ".status"),
        error = tempfile(fileext = ".error"),
        headers = tempfile(fileext = ".headers")
      )
      on.exit(unlink(unlist(files, use.names = FALSE)), add = TRUE)
      args <- c(
        "--silent", "--show-error", "--location", "--connect-timeout", as.character(connect_timeout),
        "--max-time", as.character(request_timeout)
      )
      if (identical(toupper(method), "POST")) {
        args <- c(args, "--request", "POST", "--header", shQuote("Content-Type: application/json"))
        if (!is.null(body_json)) args <- c(args, "--data-raw", shQuote(as.character(body_json)))
      } else if (!identical(toupper(method), "GET")) {
        stop(sprintf("Método HTTP não suportado: %s", method), call. = FALSE)
      }
      for (header in headers) args <- c(args, "--header", shQuote(header))
      args <- c(
        args,
        "--dump-header", shQuote(files$headers),
        "--output", shQuote(files$body),
        "--write-out", shQuote("%{http_code}"),
        shQuote(url)
      )
      unlink(c(files$status, files$error, files$headers))
      exit_status <- tryCatch(system2("curl", args, stdout = files$status, stderr = files$error), error = function(...) 1L)
      status_text <- if (file.exists(files$status)) paste(readLines(files$status, warn = FALSE), collapse = "") else ""
      status <- suppressWarnings(as.integer(trimws(status_text)))
      error_text <- if (file.exists(files$error)) paste(readLines(files$error, warn = FALSE), collapse = " ") else ""
      bytes <- if (file.exists(files$body)) readBin(files$body, "raw", n = file.info(files$body)$size) else raw(0)
      body_text <- if (length(bytes)) iconv(rawToChar(bytes), from = "UTF-8", to = "UTF-8", sub = "") else ""
      raw_headers <- if (file.exists(files$headers)) readLines(files$headers, warn = FALSE) else character()
      list(status = status, body_text = body_text, error_text = error_text,
        raw_headers = raw_headers, exit_status = exit_status)
    },
    normalise_transport_response = function(response) {
      if (!is.list(response)) stop("O transporte HTTP precisa retornar uma lista.", call. = FALSE)
      if (is.null(response$body_text)) {
        body <- private$values$or_else(response$body, "")
        response$body_text <- if (is.character(body) && length(body) == 1L) body else {
          jsonlite::toJSON(body, auto_unbox = TRUE, null = "null", digits = 16)
        }
      }
      if (is.null(response$raw_headers)) response$raw_headers <- private$values$or_else(response$headers, character())
      if (is.null(response$error_text)) response$error_text <- ""
      if (is.null(response$exit_status)) response$exit_status <- 0L
      response$exit_status <- suppressWarnings(as.integer(response$exit_status))
      if (length(response$exit_status) != 1L || is.na(response$exit_status)) response$exit_status <- 1L
      response$status <- suppressWarnings(as.integer(private$values$or_else(response$status, 0L)))
      if (length(response$status) != 1L || is.na(response$status)) response$status <- 0L
      response
    },
    http_response = function(response, parsed, return_headers) {
      transport_error <- response$exit_status != 0L
      status <- if (transport_error) 0L else response$status
      if (nzchar(parsed$error) && status >= 200L && status < 300L) status <- 0L
      error <- private$response_error_message(status, response$body_text, response$error_text,
        transport_error, parsed$error)
      result <- list(status = status, body = parsed$body, error = error, rate = response$rate,
        rate_limited = private$response_is_rate_limited(status, response$rate))
      if (return_headers) result$headers <- response$raw_headers
      result
    },
    http_request = function(url, resource, method, body_json, token) {
      settings <- private$request_settings()
      started <- private$runtime$now()
      attempt <- 1L
      repeat {
        private$limiter$wait_for_slot(resource)
        response <- private$send_transport_request(url, method, body_json, settings, token)
        rate <- private$rate_headers(private$parse_curl_headers(response$raw_headers))
        response$rate <- rate
        rate_limited <- private$response_is_rate_limited(response$status, rate)
        transport_error <- response$exit_status != 0L
        retryable <- private$response_is_retryable(response$status, rate_limited) || transport_error
        delay <- if (retryable) private$api_retry_delay(rate, attempt, rate_limited) else 0
        private$limiter$record(resource, rate, if (rate_limited) delay else 0)
        elapsed <- private$runtime$now() - started
        if (rate_limited) {
          if (elapsed + delay <= settings$max_rate_wait) next
          return(response)
        }
        if (retryable && attempt < settings$max_attempts && elapsed + delay <= settings$retry_budget) {
          cat(sprintf("GitHub API: resposta transitória HTTP %s; nova tentativa %d/%d.\n",
            response$status, attempt, settings$max_attempts - 1L))
          private$runtime$sleep(delay)
          attempt <- attempt + 1L
          next
        }
        return(response)
      }
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

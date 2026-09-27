MockHttpTransport <- R6::R6Class(
  "MockHttpTransport",
  public = list(
    initialize = function(mode = NULL, responses = NULL) {
      private$mode <- mode
      private$responses <- responses
      private$calls <- 0L
      private$last_auth_header <- ""
      invisible(self)
    },
    request = function(url, method, body_json, connect_timeout, request_timeout, headers) {
      private$calls <- private$calls + 1L
      private$last_requested_url <- url
      authorization <- grep("^Authorization:", headers, value = TRUE)
      private$last_auth_header <- if (length(authorization)) authorization[[1L]] else ""
      if (!is.null(private$responses)) return(private$responses[[min(private$calls, length(private$responses))]])
      switch(private$mode,
        retry = if (private$calls == 1L) {
          list(status = 500L, body = list(message = "temporary"), raw_headers = character())
        } else {
          list(status = 200L, body = list(ok = TRUE), raw_headers = character())
        },
        retry_exhausted = list(status = 503L, body = list(message = "temporary"), raw_headers = character()),
        transport_error = list(status = 200L, body_text = "partial", exit_status = 28L, error_text = "timeout"),
        raw_bom = list(status = 200L, body_text = paste0("\ufeff", "text"), raw_headers = character()),
        headers = list(status = 200L, body = list(ok = TRUE), raw_headers = c(
          "HTTP/1.1 301 Moved", "X-RateLimit-Remaining: 0", "HTTP/2 200", "X-RateLimit-Remaining: 10", "Link: last")),
        rate_then_success = if (private$calls == 1L) {
          list(status = 429L, body = list(message = "limit"), raw_headers = "Retry-After: 1")
        } else list(status = 200L, body = list(ok = TRUE), raw_headers = character()),
        graphql = list(
          status = 200L,
          body = list(errors = list(list(message = "limit", extensions = list(type = "RATE_LIMITED")))),
          raw_headers = character()
        ),
        malformed_graphql_errors = list(status = 200L, body = list(errors = list("unexpected")), raw_headers = character()),
        rate_limit = list(
          status = 429L,
          body = list(message = "API rate limit exceeded"),
          raw_headers = "Retry-After: 60"
        ),
        json_bom = list(
          status = 200L,
          body_text = paste0("\ufeff", '{"ok":true}'),
          raw_headers = character()
        ),
        malformed_json = list(
          status = 200L,
          body_text = "{not-json",
          raw_headers = character()
        ),
        raw = list(status = 200L, body_text = "public README content", raw_headers = character()),
        permission = list(
          status = 403L,
          body = list(message = "Resource not accessible"),
          raw_headers = "X-RateLimit-Remaining: 10"
        ),
        stop(sprintf("Modo de transporte não suportado: %s", private$mode), call. = FALSE)
      )
    },
    request_count = function() private$calls,
    last_url = function() private$last_requested_url,
    last_authorization = function() private$last_auth_header
  ),
  private = list(mode = NULL, responses = NULL, calls = NULL, last_auth_header = NULL, last_requested_url = NULL),
  lock_class = TRUE,
  cloneable = FALSE
)

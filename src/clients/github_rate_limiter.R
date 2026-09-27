GitHubRateLimiter <- R6::R6Class(
  "GitHubRateLimiter",
  public = list(
    initialize = function(config, store, lock_manager, runtime, values) {
      if (!inherits(config, "ProjectConfig")) stop("GitHubRateLimiter exige ProjectConfig.", call. = FALSE)
      if (!inherits(store, "ArtifactStore")) stop("GitHubRateLimiter exige ArtifactStore.", call. = FALSE)
      if (!inherits(lock_manager, "ArtifactLock")) stop("GitHubRateLimiter exige ArtifactLock.", call. = FALSE)
      if (!inherits(runtime, "R6") || !is.function(runtime$now) || !is.function(runtime$sleep)) {
        stop("GitHubRateLimiter exige um relógio R6 com now() e sleep().", call. = FALSE)
      }
      if (!inherits(values, "ValueTools")) stop("GitHubRateLimiter exige ValueTools.", call. = FALSE)
      private$config <- config
      private$store <- store
      private$lock_manager <- lock_manager
      private$runtime <- runtime
      private$values <- values
      invisible(self)
    },
    wait_for_slot = function(resource) {
      resource <- private$normalise_resource(resource)
      wait <- private$reserve_api_slot(private$rate_state_path(), resource, private$api_rate_interval(resource))
      if (wait$delay > 0 && wait$rate_limit_delay > wait$interval_delay + 0.5) {
        cat(sprintf("GitHub API (%s): limite ativo; aguardando renovação.\n", resource))
      }
      private$sleep_until_slot(wait$delay)
    },
    record = function(resource, headers, fallback_delay = 0) {
      if (!is.list(headers)) return(invisible(FALSE))
      resource <- private$normalise_resource(resource)
      remaining <- private$values$scalar_number(headers$github_remaining)
      reset <- private$values$scalar_number(headers$github_reset)
      retry_after <- private$values$scalar_number(headers$retry_after)
      fallback_delay <- suppressWarnings(as.numeric(private$values$or_else(fallback_delay, 0)))
      if (length(fallback_delay) != 1L || !is.finite(fallback_delay) || fallback_delay < 0) fallback_delay <- 0
      if ((is.na(remaining) || remaining > 0) && (is.na(retry_after) || retry_after <= 0) && fallback_delay <= 0) {
        return(invisible(FALSE))
      }
      private$persist_rate_state(resource, remaining, reset, retry_after, fallback_delay)
      invisible(TRUE)
    }
  ),
  private = list(
    config = NULL,
    store = NULL,
    lock_manager = NULL,
    runtime = NULL,
    values = NULL,
    normalise_resource = function(resource) {
      resource <- private$values$scalar_text(resource, "core")
      if (resource %in% c("search", "raw", "graphql")) resource else "core"
    },
    api_rate_state_default = function() {
      now <- private$runtime$now()
      list(
        last_request = list(search = 0, core = 0, raw = 0, graphql = 0),
        not_before = list(search = 0, core = 0, raw = 0, graphql = 0),
        updated_at = format(as.POSIXct(now, origin = "1970-01-01", tz = "UTC"),
          "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      )
    },
    read_api_rate_state = function(path) {
      if (!file.exists(path)) return(private$api_rate_state_default())
      state <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(...) NULL)
      if (!is.list(state)) return(private$api_rate_state_default())
      defaults <- private$api_rate_state_default()
      last_request <- if (is.list(state$last_request)) state$last_request else list()
      not_before <- if (is.list(state$not_before)) state$not_before else list()
      state$last_request <- utils::modifyList(
        defaults$last_request, last_request
      )
      state$not_before <- utils::modifyList(
        defaults$not_before, not_before
      )
      state
    },
    api_state_number = function(state, section, resource) {
      value <- private$values$or_else(state[[section]], list())[[resource]]
      value <- tryCatch(
        suppressWarnings(as.numeric(private$values$or_else(value, 0))),
        error = function(...) NA_real_
      )
      if (length(value) != 1L || !is.finite(value)) 0 else value
    },
    api_rate_interval = function(resource) {
      key <- switch(
        resource,
        search = "search_interval_seconds",
        raw = "raw_interval_seconds",
        graphql = "graphql_interval_seconds",
        "core_interval_seconds"
      )
      default <- if (identical(resource, "search")) 2.2 else 0
      as.numeric(private$config$get("api", key, default))
    },
    rate_state_path = function() {
      path <- private$config$get("api", "rate_state_path", "outputs/metadata/github_api_rate_state.json")
      private$config$resolve(as.character(path))
    },
    reserve_api_slot = function(path, resource, interval) {
      private$lock_manager$with_lock(paste0(path, ".lock"), {
        state <- private$read_api_rate_state(path)
        now <- private$runtime$now()
        interval_until <- private$api_state_number(state, "last_request", resource) + interval
        rate_limit_until <- private$api_state_number(state, "not_before", resource)
        scheduled <- max(interval_until, rate_limit_until)
        wait <- list(delay = max(0, scheduled - now), interval_delay = max(0, interval_until - now),
          rate_limit_delay = max(0, rate_limit_until - now))
        private$assert_wait_is_allowed(wait)
        state$last_request[[resource]] <- max(now, scheduled)
        state$updated_at <- format(as.POSIXct(now, origin = "1970-01-01", tz = "UTC"),
          "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
        private$store$write_json(state, path, pretty = FALSE)
        wait
      },
      timeout_seconds = as.numeric(private$config$get("api", "lock_timeout_seconds", 30)),
      stale_seconds = as.numeric(private$config$get("api", "lock_stale_seconds", 300)))
    },
    assert_wait_is_allowed = function(wait) {
      max_wait <- as.numeric(private$config$get("api", "max_rate_wait_seconds", 7200))
      if (wait$rate_limit_delay > max_wait) {
        condition <- errorCondition(
          "Limite da API do GitHub ainda ativo; aguarde o reset antes de executar novamente.",
          class = c("github_rate_limit_error", "github_api_error"), rate_limited = TRUE, status = 429L
        )
        stop(condition)
      }
    },
    sleep_until_slot = function(delay) {
      remaining <- delay
      while (remaining > 0) {
        chunk <- min(30, remaining)
        private$runtime$sleep(chunk)
        remaining <- remaining - chunk
      }
      invisible(TRUE)
    },
    rate_state_deadline = function(now, current, remaining, reset, retry_after, fallback_delay) {
      retry_at <- if (is.finite(retry_after) && retry_after > 0) now + retry_after + 2 else 0
      reset_at <- if ((!is.finite(retry_after) || retry_after <= 0) &&
        is.finite(remaining) && remaining <= 0 && is.finite(reset) && reset > now) reset + 2 else 0
      fallback_at <- if (is.finite(fallback_delay) && fallback_delay > 0) now + fallback_delay else 0
      max(current, reset_at, retry_at, fallback_at)
    },
    persist_rate_state = function(resource, remaining, reset, retry_after, fallback_delay) {
      path <- private$rate_state_path()
      private$lock_manager$with_lock(paste0(path, ".lock"), {
        state <- private$read_api_rate_state(path)
        now <- private$runtime$now()
        current <- private$api_state_number(state, "not_before", resource)
        state$not_before[[resource]] <- private$rate_state_deadline(
          now, current, remaining, reset, retry_after, fallback_delay
        )
        state$updated_at <- format(as.POSIXct(now, origin = "1970-01-01", tz = "UTC"),
          "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
        private$store$write_json(state, path, pretty = FALSE)
        invisible(TRUE)
      },
      timeout_seconds = as.numeric(private$config$get("api", "lock_timeout_seconds", 30)),
      stale_seconds = as.numeric(private$config$get("api", "lock_stale_seconds", 300)))
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

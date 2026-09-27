MockGitHubClientBase <- R6::R6Class(
  "MockGitHubClientBase",
  public = list(
    assert_authenticated = function() invisible(TRUE),
    response_ok = function(response) {
      status <- suppressWarnings(as.integer(response$status))
      length(status) == 1L && !is.na(status) && status >= 200L && status < 300L
    },
    response_condition = function(response, fallback_message = NULL) {
      message <- if (!is.null(fallback_message) && !nzchar(response$error)) fallback_message else as.character(response$error)
      class <- if (isTRUE(response$rate_limited)) {
        c("github_rate_limit_error", "github_api_error", "error", "condition")
      } else {
        c("github_api_error", "error", "condition")
      }
      structure(list(
        message = message,
        call = NULL,
        status = response$status,
        rate_limited = isTRUE(response$rate_limited),
        rate = response$rate
      ), class = class)
    },
    is_rate_limit_error = function(error) inherits(error, "github_rate_limit_error")
  ),
  private = list(
    make_commit = function(date, sha) {
      list(
        sha = sha,
        html_url = "https://example.test/commit",
        commit = list(
          author = list(date = date),
          committer = list(date = date),
          message = "snapshot",
          tree = list(sha = paste0(sha, "-tree"))
        )
      )
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

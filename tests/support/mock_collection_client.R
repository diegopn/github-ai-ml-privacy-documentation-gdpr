MockCollectionClient <- R6::R6Class(
  "MockCollectionClient",
  inherit = MockGitHubClientBase,
  public = list(
    initialize = function(mode = "complete") {
      private$mode <- mode
      private$raw_calls <- 0L
      invisible(self)
    },
    api = function(path, params = list(), return_headers = FALSE) {
      if (identical(path, "/repos/owner/project")) return(private$repository_response())
      if (grepl("/commits$", path)) return(private$commit_response(params))
      if (grepl("/git/trees/", path, fixed = TRUE)) return(private$tree_response())
      stop(sprintf("Rota inesperada no mock de coleta: %s", path), call. = FALSE)
    },
    raw = function(repository, sha, path) {
      private$raw_calls <- private$raw_calls + 1L
      if (private$mode == "failed_download") {
        return(list(status = 503L, text = "", note = "temporary failure", rate = list(), rate_limited = FALSE))
      }
      list(
        status = 200L,
        text = "Our privacy policy: we collect personal data, including email addresses, to provide the service.",
        note = "",
        rate = list(),
        rate_limited = FALSE
      )
    },
    raw_call_count = function() private$raw_calls
  ),
  private = list(
    raw_calls = NULL,
    mode = NULL,
    repository_response = function() {
      list(status = 200L, body = list(
        full_name = "owner/project", visibility = "public", private = FALSE, fork = FALSE, archived = FALSE,
        stargazers_count = 501L, open_issues_count = 110L, created_at = "2010-01-01T00:00:00Z",
        updated_at = "2024-01-01T00:00:00Z", license = list(spdx_id = "MIT", name = "MIT")
      ), error = "", rate = list())
    },
    commit_response = function(params) {
      cutoff <- if (!is.null(params$until)) as.character(params$until) else "2026-06-30T23:59:59Z"
      sha <- if (grepl("2018-05-24", cutoff, fixed = TRUE)) "pre-sha" else "post-sha"
      list(status = 200L, body = list(private$make_commit(cutoff, sha)), error = "", rate = list())
    },
    tree_response = function() {
      if (private$mode == "malformed_tree") return(list(status = 200L,
        body = list(truncated = FALSE, tree = "invalid"), error = "", rate = list()))
      list(status = 200L, body = list(
        truncated = FALSE,
        tree = list(list(type = "blob", path = "README.md", sha = "shared-blob", size = 100L))
      ), error = "", rate = list())
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

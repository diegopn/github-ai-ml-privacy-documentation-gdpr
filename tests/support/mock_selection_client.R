MockSelectionClient <- R6::R6Class(
  "MockSelectionClient",
  inherit = MockGitHubClientBase,
  public = list(
    initialize = function(topics, page_one, page_two, rate_limited = FALSE, malformed_graphql = FALSE) {
      private$topics <- as.character(topics)
      private$page_one <- page_one
      private$page_two <- page_two
      private$rate_limited <- isTRUE(rate_limited)
      private$malformed_graphql <- isTRUE(malformed_graphql)
      invisible(self)
    },
    api = function(path, params = list(), return_headers = FALSE) {
      if (identical(path, "/search/repositories")) return(private$search_response(params))
      if (grepl("/commits$", path)) return(private$commit_response(path, params, return_headers))
      stop(sprintf("Rota inesperada no mock de seleção: %s", path), call. = FALSE)
    },
    graphql = function(query, variables = list()) {
      if (!grepl("issues(first: 1", query, fixed = TRUE)) stop("Consulta GraphQL inesperada no mock.", call. = FALSE)
      if (private$rate_limited) {
        return(list(status = 200L, body = list(errors = list(list(message = "API rate limit exceeded",
          extensions = list(type = "RATE_LIMITED")))), error = "API rate limit exceeded", rate_limited = TRUE, rate = list()))
      }
      if (private$malformed_graphql) {
        return(list(status = 200L, body = list(data = list(r1 = list(issues = list(totalCount = 101L)))), error = "", rate = list()))
      }
      list(status = 200L, body = list(data = list(
        r1 = list(issues = list(totalCount = 101L)),
        r2 = list(issues = list(totalCount = 5L)),
        r3 = list(issues = list(totalCount = 101L))
      )), error = "", rate = list())
    }
  ),
  private = list(
    topics = NULL,
    page_one = NULL,
    page_two = NULL,
    rate_limited = FALSE,
    malformed_graphql = FALSE,
    topic_from_query = function(query) {
      matches <- private$topics[vapply(private$topics, function(topic) {
        grepl(paste0("topic:", topic), query, fixed = TRUE)
      }, logical(1L))]
      if (length(matches)) matches[[1L]] else ""
    },
    search_response = function(params) {
      topic <- private$topic_from_query(as.character(if (is.null(params$q)) "" else params$q))
      if (is.null(params$sort)) {
        total <- switch(as.character(match(topic, private$topics)), "1" = 101L, "2" = 1L, 0L)
        return(list(status = 200L, body = list(total_count = total, incomplete_results = FALSE), error = "", rate = list()))
      }
      page <- as.integer(params$page)
      page <- if (length(page) == 1L) as.character(page) else ""
      unexpected_page <- sprintf("Página de busca inesperada para tópico %s", topic)
      items <- switch(as.character(match(topic, private$topics)),
        "1" = switch(page, "1" = private$page_one, "2" = private$page_two,
          stop(unexpected_page, call. = FALSE)),
        "2" = list(private$page_one[[1L]]),
        stop(unexpected_page, call. = FALSE)
      )
      list(status = 200L, body = list(items = items, incomplete_results = FALSE), error = "", rate = list())
    },
    commit_response = function(path, params, return_headers) {
      repository <- sub("/commits$", "", sub("^/repos/", "", path))
      date <- params$until
      if (is.null(date)) date <- params$since
      if (!is.null(date)) {
        commit <- private$make_commit(as.character(date), paste0(repository, "-window"))
        return(list(status = 200L, body = list(commit), error = "", rate = list()))
      }
      page <- as.integer(if (is.null(params$page)) 1L else params$page)
      if (identical(page, 2L)) {
        date <- if (identical(repository, "owner/project")) "2010-01-01T00:00:00Z" else "2024-01-01T00:00:00Z"
        return(list(status = 200L, body = list(private$make_commit(date, paste0(repository, "-old"))), error = "", rate = list()))
      }
      headers <- if (isTRUE(return_headers) && identical(repository, "owner/project")) {
        'Link: <https://api.github.com/repos/owner/project/commits?page=2>; rel="last"'
      } else character()
      list(status = 200L, body = list(private$make_commit("2024-01-01T00:00:00Z", paste0(repository, "-new"))),
        error = "", rate = list(), headers = headers)
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

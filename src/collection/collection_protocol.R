CollectionProtocol <- R6::R6Class(
  "CollectionProtocol",
  public = list(
    version = function() private$current_version,
    record_complete = function(record) {
      is.list(record) && is.null(record$fatal_error) &&
        private$period_complete(record$pre) && private$period_complete(record$post)
    },
    record_analyzable = function(record) {
      self$record_complete(record) && isFALSE(record$pre$tree_truncated) && isFALSE(record$post$tree_truncated)
    },
    record_reusable = function(record, source_hash) {
      is.list(record) && identical(record$source_sha256, source_hash) &&
        length(record$collector_protocol_version) == 1L &&
        record$collector_protocol_version %in% private$compatible_protocols
    }
  ),
  private = list(
    current_version = "open-source-no-size-limit-expanded-topics-2026-09",
    compatible_protocols = c(
      "open-source-no-size-limit-expanded-topics-2026-09",
      "open-source-no-size-limit-2026-09"
    ),
    period_complete = function(version) {
      if (!is.list(version) || !is.list(version$commit)) return(FALSE)
      sha <- version$commit$sha
      if (!is.character(sha) || length(sha) != 1L || is.na(sha) || !nzchar(sha)) return(FALSE)
      if (!identical(as.character(version$commit_request_status), "200") ||
        !identical(as.character(version$tree_request_status), "200")) return(FALSE)
      if (!isTRUE(version$tree_truncated) && !isFALSE(version$tree_truncated)) return(FALSE)
      if (!is.null(version$error) && !identical(version$error, "")) return(FALSE)
      private$documents_complete(version$document_candidates, version$documents)
    },
    documents_complete = function(candidates, documents) {
      if (!is.list(candidates) || !is.list(documents) || length(candidates) != length(documents)) return(FALSE)
      if (!all(vapply(candidates, is.list, logical(1L))) || !all(vapply(documents, is.list, logical(1L)))) return(FALSE)
      candidate_paths <- vapply(candidates, private$document_path, character(1L))
      document_paths <- vapply(documents, private$document_path, character(1L))
      if (any(!nzchar(candidate_paths)) || anyDuplicated(candidate_paths) ||
        !identical(candidate_paths, document_paths)) return(FALSE)
      all(vapply(documents, function(document) {
        identical(as.character(document$status), "200") && is.character(document$text) &&
          length(document$text) == 1L && !is.na(document$text)
      }, logical(1L)))
    },
    document_path = function(document) {
      path <- document$path
      if (is.character(path) && length(path) == 1L && !is.na(path)) path else ""
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

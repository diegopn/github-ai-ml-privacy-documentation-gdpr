RepositoryCollector <- R6::R6Class(
  "RepositoryCollector",
  public = list(
    initialize = function(config, client, store, classifier, document_catalog, values, protocol) {
      if (!inherits(config, "ProjectConfig")) stop("RepositoryCollector exige ProjectConfig.", call. = FALSE)
      if (!inherits(store, "ArtifactStore")) stop("RepositoryCollector exige ArtifactStore.", call. = FALSE)
      required_client_methods <- c("assert_authenticated", "api", "raw", "response_ok", "response_condition", "is_rate_limit_error")
      if (!inherits(client, "R6") || !all(vapply(required_client_methods, function(method) is.function(client[[method]]), logical(1L)))) {
        stop("RepositoryCollector exige um cliente GitHub R6.", call. = FALSE)
      }
      if (!inherits(classifier, "PrivacyDocumentClassifier")) stop("RepositoryCollector exige PrivacyDocumentClassifier.", call. = FALSE)
      if (!inherits(document_catalog, "PrivacyDocumentCatalog")) stop("RepositoryCollector exige PrivacyDocumentCatalog.", call. = FALSE)
      if (!inherits(values, "ValueTools")) stop("RepositoryCollector exige ValueTools.", call. = FALSE)
      if (!inherits(protocol, "CollectionProtocol")) stop("RepositoryCollector exige CollectionProtocol.", call. = FALSE)
      private$config <- config
      private$client <- client
      private$store <- store
      private$classifier <- classifier
      private$document_catalog <- document_catalog
      private$values <- values
      private$protocol <- protocol
      invisible(self)
    },
    run = function(input = private$store$sample_path()) {
      private$client$assert_authenticated()
      input <- private$config$resolve(input)
      sample <- private$store$read_sample(input)
      private$store$validate_sample(sample)
      source_hash <- private$store$hash_file(input)
      checkpoint <- private$store$raw_path()
      dir.create(dirname(checkpoint), recursive = TRUE, showWarnings = FALSE)
      temporary <- tempfile("repository-results-", tmpdir = dirname(checkpoint), fileext = ".jsonl")
      on.exit(unlink(temporary, force = TRUE), add = TRUE)
      repositories <- as.character(sample$repository)
      private$collect_sample(repositories, sample, source_hash, temporary)
      private$store$read_complete_checkpoint(sample, temporary, source_hash)
      private$store$publish_checkpoint(temporary, checkpoint)
      invisible(checkpoint)
    }
  ),
  private = list(
    config = NULL,
    client = NULL,
    store = NULL,
    classifier = NULL,
    document_catalog = NULL,
    values = NULL,
    protocol = NULL,
    collect_sample = function(repositories, sample, source_hash, temporary) {
      last_report <- Sys.time()
      for (index in seq_along(repositories)) {
        repository <- repositories[[index]]
        row <- sample[index, , drop = FALSE]
        record <- tryCatch(private$collect_one(row, source_hash), error = function(error) {
          if (private$client$is_rate_limit_error(error)) stop(error)
          list(repository = repository, input_row = as.list(row), fatal_error = conditionMessage(error))
        })
        private$store$write_jsonl(list(record), temporary, append = TRUE)
        now <- Sys.time()
        should_report <- index == length(repositories) || as.numeric(difftime(now, last_report, units = "secs")) >= 60
        if (should_report) {
          cat(sprintf("Repositórios processados: %d/%d\n", index, length(repositories)))
          last_report <- now
        }
      }
      invisible(TRUE)
    },
    compact_metadata = function(body) {
      if (!is.list(body)) return(list())
      metadata <- list()
      for (field in c("full_name", "visibility", "created_at", "updated_at")) {
        metadata[[field]] <- private$values$scalar_text(body[[field]], "")
      }
      for (field in c("private", "fork", "archived")) {
        metadata[[field]] <- private$values$scalar_bool(body[[field]], FALSE)
      }
      for (field in c("stargazers_count", "open_issues_count")) {
        metadata[[field]] <- private$values$scalar_int(body[[field]], 0L)
      }
      license <- private$values$or_else(body$license, list())
      metadata$license_spdx_id <- private$values$scalar_text(license$spdx_id, "")
      metadata$license_name <- private$values$scalar_text(license$name, "")
      metadata
    },
    commit_info = function(payload) {
      commit <- private$values$or_else(payload$commit, list())
      author <- private$values$or_else(commit$author, list())
      committer <- private$values$or_else(commit$committer, list())
      message <- private$values$scalar_text(commit$message, "")
      lines <- strsplit(message, "\\r?\\n", perl = TRUE)[[1L]]
      first_line <- if (length(lines)) lines[[1L]] else ""
      list(
        sha = private$values$scalar_text(payload$sha, ""),
        url = private$values$scalar_text(payload$html_url, ""),
        date = private$values$scalar_text(committer$date, private$values$scalar_text(author$date, "")),
        author_date = private$values$scalar_text(author$date, ""),
        committer_date = private$values$scalar_text(committer$date, ""),
        tree_sha = private$values$scalar_text(private$values$or_else(commit$tree, list())$sha, ""),
        message_first_line = substr(first_line, 1L, 300L)
      )
    },
    request_period_commit = function(repository, until) {
      response <- private$client$api(paste0("/repos/", repository, "/commits"), list(until = until, per_page = 1L))
      if (!private$client$response_ok(response) && isTRUE(response$rate_limited)) {
        stop(private$client$response_condition(response))
      }
      payload <- if (private$client$response_ok(response) && is.list(response$body) && length(response$body)) response$body[[1L]] else NULL
      commit <- if (is.null(payload)) NULL else private$commit_info(payload)
      error <- private$values$scalar_text(response$error, "")
      if (is.null(commit) && !nzchar(error)) error <- "nenhum commit disponível no limite temporal"
      list(status = response$status, rate = private$values$or_else(response$rate, list()), commit = commit, error = error)
    },
    request_commit_tree = function(repository, commit) {
      response <- private$client$api(paste0("/repos/", repository, "/git/trees/", commit$tree_sha), list(recursive = 1L))
      if (!private$client$response_ok(response) && isTRUE(response$rate_limited)) {
        stop(private$client$response_condition(response))
      }
      body <- if (is.list(response$body)) response$body else list()
      entries <- body$tree
      truncation_flag <- body$truncated
      tree_valid <- is.list(entries) && all(vapply(entries, function(entry) {
        is.list(entry) && is.character(entry$path) && length(entry$path) == 1L &&
          !is.na(entry$path) && nzchar(entry$path) &&
          is.character(entry$type) && length(entry$type) == 1L && entry$type %in% c("blob", "tree", "commit")
      }, logical(1L)))
      truncation_valid <- isTRUE(truncation_flag) || isFALSE(truncation_flag)
      error <- private$values$scalar_text(response$error, "")
      if (!tree_valid || !truncation_valid) {
        error <- "A resposta da árvore Git está incompleta ou malformada."
      }
      list(
        status = response$status,
        rate = private$values$or_else(response$rate, list()),
        truncated = if (truncation_valid) private$values$scalar_bool(truncation_flag, TRUE) else TRUE,
        body = body,
        error = error
      )
    },
    document_cache_key = function(repository, commit_sha, candidate) {
      blob_sha <- private$values$scalar_text(candidate$blob_sha, "")
      identity <- if (nzchar(blob_sha)) paste0("blob:", blob_sha) else {
        paste0("commit:", commit_sha, "::path:", private$values$scalar_text(candidate$path, ""))
      }
      paste(repository, identity, sep = "::")
    },
    download_documents = function(repository, commit, candidates, cache) {
      documents <- vector("list", length(candidates))
      for (index in seq_along(candidates)) {
        candidate <- candidates[[index]]
        key <- private$document_cache_key(repository, commit$sha, candidate)
        downloaded <- cache[[key]]
        if (is.null(downloaded)) {
          downloaded <- private$client$raw(repository, commit$sha, candidate$path)
          if (identical(as.character(downloaded$status), "200")) cache[[key]] <- downloaded
        }
        if (isTRUE(downloaded$rate_limited)) {
          stop(private$client$response_condition(list(
            status = downloaded$status, error = downloaded$note, rate_limited = TRUE,
            rate = private$values$or_else(downloaded$rate, list())
          )))
        }
        documents[[index]] <- c(candidate, list(
          status = downloaded$status,
          text = downloaded$text,
          fetch_note = downloaded$note,
          rate = private$values$or_else(downloaded$rate, list()),
          text_sha256 = if (nzchar(downloaded$text)) private$store$hash_text(downloaded$text) else ""
        ))
      }
      documents
    },
    collect_version = function(repository, until, cache) {
      version <- list(until = until)
      commit_response <- private$request_period_commit(repository, until)
      version$commit_request_status <- commit_response$status
      version$commit_rate <- commit_response$rate
      if (is.null(commit_response$commit)) {
        version$error <- commit_response$error
        return(version)
      }
      commit <- commit_response$commit
      version$commit <- commit
      tree <- private$request_commit_tree(repository, commit)
      version$tree_request_status <- tree$status
      version$tree_rate <- tree$rate
      version$tree_truncated <- tree$truncated
      if (!private$client$response_ok(list(status = tree$status)) || nzchar(tree$error)) {
        version$error <- if (nzchar(tree$error)) tree$error else "árvore Git não recuperada"
        return(version)
      }
      candidates <- private$document_catalog$discover_candidates(tree$body)
      version$document_candidates <- candidates
      version$documents <- private$download_documents(repository, commit, candidates, cache)
      version$classification_rule_version <- private$classifier$rule_version()
      version$classification <- private$classifier$classify(version$documents, commit$sha, repository)
      version
    },
    collect_one = function(row, source_hash) {
      repository <- private$values$scalar_text(row$repository, "")
      result <- list(
        repository = repository,
        input_row = as.list(row),
        source_sha256 = source_hash,
        collector_protocol_version = private$protocol$version(),
        observed_at = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%OS3Z")
      )
      metadata_response <- private$client$api(paste0("/repos/", repository), list())
      if (!private$client$response_ok(metadata_response) && isTRUE(metadata_response$rate_limited)) {
        stop(private$client$response_condition(metadata_response))
      }
      result$accessibility <- list(
        http_status = metadata_response$status,
        accessible = private$client$response_ok(metadata_response),
        metadata = private$compact_metadata(metadata_response$body),
        error = private$values$scalar_text(metadata_response$error, ""),
        rate = private$values$or_else(metadata_response$rate, list())
      )
      cache <- new.env(parent = emptyenv())
      cutoffs <- private$config$analysis_cutoffs()
      for (period in names(cutoffs)) {
        cutoff <- cutoffs[[period]]
        result[[period]] <- if (result$accessibility$accessible) {
          private$collect_version(repository, cutoff, cache)
        } else {
          list(until = cutoff, error = "versão não consultada porque a validação do repositório falhou")
        }
      }
      result
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

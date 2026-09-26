compact_metadata <- function(body) {
  if (!is.list(body)) return(list())
  metadata <- list()
  for (field in c("full_name", "visibility", "created_at", "updated_at")) metadata[[field]] <- scalar_text(body[[field]], "")
  for (field in c("private", "fork", "archived")) metadata[[field]] <- scalar_bool(body[[field]], FALSE)
  for (field in c("stargazers_count", "open_issues_count")) metadata[[field]] <- scalar_int(body[[field]], 0L)
  license <- body$license %||% list()
  metadata$license_spdx_id <- scalar_text(license$spdx_id, "")
  metadata$license_name <- scalar_text(license$name, "")
  metadata
}

commit_info <- function(payload) {
  commit <- payload$commit %||% list()
  author <- commit$author %||% list()
  committer <- commit$committer %||% list()
  date <- scalar_text(committer$date, scalar_text(author$date, ""))
  message <- scalar_text(commit$message, "")
  first_line <- strsplit(message, "\\r?\\n", perl = TRUE)[[1L]][[1L]] %||% ""
  list(
    sha = scalar_text(payload$sha, ""),
    url = scalar_text(payload$html_url, ""),
    date = date,
    author_date = scalar_text(author$date, ""),
    committer_date = scalar_text(committer$date, ""),
    tree_sha = scalar_text((commit$tree %||% list())$sha, ""),
    message_first_line = substr(first_line, 1L, 300L)
  )
}

document_cache_key <- function(repository, commit_sha, candidate) {
  blob_sha <- scalar_text(candidate$blob_sha, "")
  identity <- if (nzchar(blob_sha)) {
    paste0("blob:", blob_sha)
  } else {
    paste0("commit:", commit_sha, "::path:", scalar_text(candidate$path, ""))
  }
  paste(repository, identity, sep = "::")
}

collect_version <- function(repository, until, accessible, token, document_cache = NULL, classifier = NULL) {
  version <- list(until = until)
  if (!accessible) {
    version$error <- "versão não consultada porque a validação do repositório falhou"
    return(version)
  }
  commits <- github_api(paste0("/repos/", repository, "/commits"), list(until = until, per_page = 1), token)
  if (!api_response_ok(commits) && api_error_is_rate_limited(commits$error)) stop(commits$error, call. = FALSE)
  version$commit_request_status <- commits$status
  version$commit_rate <- commits$rate %||% list()
  first <- if (api_response_ok(commits) && is.list(commits$body) && length(commits$body)) commits$body[[1L]] else NULL
  if (is.null(first)) {
    version$error <- if (nzchar(commits$error %||% "")) commits$error else "nenhum commit disponível no limite temporal"
    return(version)
  }
  commit <- commit_info(first)
  version$commit <- commit
  tree_response <- github_api(paste0("/repos/", repository, "/git/trees/", commit$tree_sha), list(recursive = 1), token)
  if (!api_response_ok(tree_response) && api_error_is_rate_limited(tree_response$error)) stop(tree_response$error, call. = FALSE)
  version$tree_request_status <- tree_response$status
  version$tree_rate <- tree_response$rate %||% list()
  version$tree_truncated <- scalar_bool((tree_response$body %||% list())$truncated, FALSE)
  if (!api_response_ok(tree_response)) {
    version$error <- if (nzchar(tree_response$error %||% "")) tree_response$error else "árvore Git não recuperada"
    return(version)
  }
  candidates <- document_candidates(tree_response$body)
  version$document_candidates <- candidates
  documents <- list()
  for (candidate in candidates) {
    cache_key <- document_cache_key(repository, commit$sha, candidate)
    downloaded <- if (!is.null(document_cache) && exists(cache_key, envir = document_cache, inherits = FALSE)) {
      get(cache_key, envir = document_cache, inherits = FALSE)
    } else {
      value <- github_raw(repository, commit$sha, candidate$path, token)
      if (!is.null(document_cache)) assign(cache_key, value, envir = document_cache)
      value
    }
    if (api_error_is_rate_limited(downloaded$note)) stop(downloaded$note, call. = FALSE)
    document <- c(candidate, list(
      status = downloaded$status,
      text = downloaded$text,
      fetch_note = downloaded$note,
      rate = downloaded$rate %||% list(),
      text_sha256 = if (nzchar(downloaded$text)) sha256_text(downloaded$text) else ""
    ))
    documents[[length(documents) + 1L]] <- document
  }
  version$documents <- documents
  version$classification_rule_version <- RULE_VERSION
  version$classification <- if (is.null(classifier)) classify_documents(documents, commit$sha, repository) else classifier$classify(documents, commit$sha, repository)
  version
}

collect_one <- function(row, source_hash, token, classifier = NULL) {
  repository <- scalar_text(row$repository, "")
  result <- list(
    repository = repository,
    input_row = as.list(row),
    source_sha256 = source_hash,
    collector_protocol_version = COLLECTOR_PROTOCOL_VERSION,
    observed_at = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%OS3Z")
  )
  metadata_response <- github_api(paste0("/repos/", repository), list(), token)
  if (!api_response_ok(metadata_response) && api_error_is_rate_limited(metadata_response$error)) {
    stop(metadata_response$error, call. = FALSE)
  }
  result$accessibility <- list(
    http_status = metadata_response$status,
    accessible = api_response_ok(metadata_response),
    metadata = compact_metadata(metadata_response$body),
    error = metadata_response$error %||% "",
    rate = metadata_response$rate %||% list()
  )
  accessible <- api_response_ok(metadata_response)
  document_cache <- new.env(parent = emptyenv())
  result$pre <- collect_version(repository, PRE_UNTIL, accessible, token, document_cache, classifier)
  result$post <- collect_version(repository, POST_UNTIL, accessible, token, document_cache, classifier)
  result
}

is_reusable <- function(record, source_hash) {
  !is.null(record) && identical(scalar_text(record$source_sha256, ""), source_hash) &&
    scalar_text(record$collector_protocol_version, "") %in% COLLECTOR_PROTOCOL_COMPATIBLE_VERSIONS
}

assert_checkpoint_complete <- function(sample, checkpoint, source_hash) {
  records <- index_records(read_jsonl(checkpoint))
  repositories <- unique(as.character(sample$repository))
  incomplete <- repositories[!vapply(repositories, function(repository) {
    is_reusable(records[[repository]], source_hash) && is_complete_record(records[[repository]])
  }, logical(1L))]
  if (length(incomplete)) {
    preview <- paste(head(incomplete, 8L), collapse = ", ")
    suffix <- if (length(incomplete) > 8L) "..." else ""
    stop(sprintf(
      "Coleta incompleta: %d de %d repositórios não foram concluídos (%s%s). Execute novamente --select para iniciar uma coleta limpa.",
      length(incomplete), length(repositories), preview, suffix
    ), call. = FALSE)
  }
  invisible(TRUE)
}


RepositoryCollector <- R6::R6Class(
  "RepositoryCollector",
  public = list(
    config = NULL,
    client = NULL,
    store = NULL,
    classifier = NULL,
    initialize = function(config, client, store, classifier) {
      self$config <- config
      self$client <- client
      self$store <- store
      self$classifier <- classifier
    },
    run = function(input = self$store$sample_path()) {
      set_github_client(self$client)
      input <- resolve_project_path(input)
      sample <- self$store$read_sample(input)
      self$store$validate_sample(sample)
      source_hash <- self$store$hash_file(input)
      checkpoint <- self$store$raw_path()
      dir.create(dirname(checkpoint), recursive = TRUE, showWarnings = FALSE)
      temporary <- tempfile("repository-results-", tmpdir = dirname(checkpoint), fileext = ".jsonl")
      on.exit(unlink(temporary, force = TRUE), add = TRUE)

      token <- Sys.getenv("GITHUB_TOKEN", unset = "")
      if (!nzchar(token)) token <- read_dotenv_token(file.path(self$config$root, ".env"))
      if (!nzchar(token)) stop("GITHUB_TOKEN não encontrado no ambiente ou em .env.", call. = FALSE)

      repositories <- as.character(sample$repository)
      last_report <- Sys.time()
      for (index in seq_along(repositories)) {
        repository <- repositories[[index]]
        row <- sample[index, , drop = FALSE]
        record <- tryCatch(
          collect_one(row, source_hash, token, self$classifier),
          error = function(error) {
            if (api_error_is_rate_limited(error)) stop(error)
            list(repository = repository, input_row = as.list(row), fatal_error = conditionMessage(error))
          }
        )
        self$store$write_jsonl(list(record), temporary, append = TRUE)
        now <- Sys.time()
        if (index == length(repositories) || as.numeric(difftime(now, last_report, units = "secs")) >= 60) {
          cat(sprintf("Repositórios processados: %d/%d\n", index, length(repositories)))
          last_report <- now
        }
      }

      self$store$assert_checkpoint_complete(sample, temporary, source_hash)
      if (!file.rename(temporary, checkpoint)) stop("Não foi possível publicar o checkpoint completo da coleta.", call. = FALSE)
      invisible(checkpoint)
    }
  )
)

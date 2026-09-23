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

collect_version <- function(repository, until, accessible, token, document_cache = NULL) {
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
  version$classification <- classify_documents(documents, commit$sha, repository)
  version
}

collect_one <- function(row, source_hash, token) {
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
  result$pre <- collect_version(repository, PRE_UNTIL, accessible, token, document_cache)
  result$post <- collect_version(repository, POST_UNTIL, accessible, token, document_cache)
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

run_collection <- function(options) {
  input_path <- resolve_project_path(options$input %||% sample_path())
  sample <- read_sample(input_path)
  validate_open_source_sample(sample)
  source_hash <- sha256_file(input_path)
  output_dir <- resolve_project_path(options$output %||% dirname(raw_checkpoint_path()))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  checkpoint <- file.path(output_dir, "repository_results.jsonl")
  if (isTRUE(options$fresh) && file.exists(checkpoint)) {
    unlink(checkpoint)
    cat(sprintf("Checkpoint anterior removido; iniciando coleta limpa.\n  Arquivo: %s\n",
                normalizePath(checkpoint, mustWork = FALSE)))
  }
  cached <- index_records(read_jsonl(checkpoint))
  repositories <- unique(as.character(sample$repository))
  pending <- repositories[!vapply(repositories, function(repository) is_reusable(cached[[repository]], source_hash), logical(1L))]
  cat(sprintf(
    "Coleta histórica\n  Amostra:     %d repositórios\n  Checkpoint:  %d registros\n  Pendentes:   %d repositórios\n  Períodos:    pré %s | pós %s\n  Execução:    sequencial\n",
    nrow(sample), length(cached), length(pending), PRE_UNTIL, POST_UNTIL
  ))
  if (length(pending)) {
    token <- Sys.getenv("GITHUB_TOKEN", unset = "")
    if (!nzchar(token)) token <- read_dotenv_token()
    if (!nzchar(token)) stop("GITHUB_TOKEN não encontrado no ambiente ou em .env.")
    tracker <- new.env(parent = emptyenv())
    tracker$started <- Sys.time()
    tracker$last_report <- Sys.time()
    completed <- 0L
    incomplete <- 0L
    cat(sprintf("\nIniciando coleta de %d repositórios.\n", length(pending)))
    for (index in seq_along(pending)) {
      repository <- pending[[index]]
      row <- sample[match(repository, sample$repository), , drop = FALSE]
      record <- tryCatch(
        collect_one(row, source_hash, token),
        error = function(error) {
          if (api_error_is_rate_limited(error)) stop(error)
          list(repository = repository, input_row = as.list(row), fatal_error = conditionMessage(error))
        }
      )
      write_jsonl(list(record), checkpoint, append = TRUE)
      is_failure <- !is.null(record$fatal_error) || !is_complete_record(record)
      if (is_failure) incomplete <- incomplete + 1L else completed <- completed + 1L
      if (index == 1L || index == length(pending) || index %% 10L == 0L ||
          as.numeric(difftime(Sys.time(), tracker$last_report, units = "secs")) >= 60) {
        elapsed_seconds <- as.numeric(difftime(Sys.time(), tracker$started, units = "secs"))
        elapsed_minutes <- elapsed_seconds / 60
        remaining_minutes <- if (elapsed_seconds > 0) {
          (length(pending) - index) * elapsed_seconds / index / 60
        } else {
          NA_real_
        }
        estimate <- if (is.finite(remaining_minutes)) sprintf("%.1f min", remaining_minutes) else "calculando"
        cat(sprintf(
          "\n  Progresso: %d/%d | completos: %d | incompletos: %d\n  Atual: %s — %s\n  Tempo: %.1f min decorridos | restante estimado: %s\n",
          index, length(pending), completed, incomplete,
          repository, if (is_failure) "INCOMPLETO" else "OK", elapsed_minutes, estimate
        ))
        tracker$last_report <- Sys.time()
      }
    }
    cat(sprintf(
      "\nResumo da coleta\n  Processados:  %d/%d repositórios\n  Completos:    %d\n  Incompletos:  %d\n",
      length(pending), length(pending), completed, incomplete
    ))
  } else {
    cat("\nColeta histórica\n  Nenhum repositório pendente; resultados do checkpoint local mantidos.\n")
  }
  cat(sprintf("  Checkpoint salvo: %s\n", normalizePath(checkpoint, mustWork = FALSE)))
  assert_checkpoint_complete(sample, checkpoint, source_hash)
  invisible(checkpoint)
}

## Coleta histórica de documentação versionada no GitHub.

script_arg_for_source <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path_for_source <- if (length(script_arg_for_source)) sub("^--file=", "", script_arg_for_source[[1L]]) else file.path("R", "collect.R")
source(file.path(dirname(script_path_for_source), "common.R"))

parse_options <- function(args) {
  values <- list(
    input = file.path("data", "repositorios_selecionados.csv"),
    output = file.path("data", "raw"),
    workers = 1L
  )
  index <- 1L
  while (index <= length(args)) {
    option <- args[[index]]
    if (option %in% c("--input", "--output", "--workers") && index < length(args)) {
      name <- sub("^--", "", option)
      value <- args[[index + 1L]]
      values[[name]] <- if (name == "workers") max(1L, as.integer(value)) else value
      index <- index + 2L
    } else {
      stop("Uso: Rscript R/collect.R [--input arquivo.csv] [--output diretório] [--workers N]")
    }
  }
  values
}

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

collect_version <- function(repository, until, accessible, token) {
  version <- list(until = until)
  if (!accessible) {
    version$error <- "versão não consultada porque a validação do repositório falhou"
    return(version)
  }
  commits <- github_api(paste0("/repos/", repository, "/commits"), list(until = until, per_page = 1), token)
  version$commit_request_status <- commits$status
  first <- if (commits$status >= 200L && commits$status < 300L && is.list(commits$body) && length(commits$body)) commits$body[[1L]] else NULL
  if (is.null(first)) {
    version$error <- if (nzchar(commits$error)) commits$error else "nenhum commit disponível no limite temporal"
    return(version)
  }
  commit <- commit_info(first)
  version$commit <- commit
  tree_response <- github_api(paste0("/repos/", repository, "/git/trees/", commit$tree_sha), list(recursive = 1), token)
  version$tree_request_status <- tree_response$status
  version$tree_truncated <- scalar_bool((tree_response$body %||% list())$truncated, FALSE)
  if (tree_response$status != 200L) {
    version$error <- if (nzchar(tree_response$error)) tree_response$error else "árvore Git não recuperada"
    return(version)
  }
  candidates <- document_candidates(tree_response$body)
  version$document_candidates <- candidates
  documents <- list()
  for (candidate in candidates) {
    downloaded <- github_raw(repository, commit$sha, candidate$path, token)
    document <- c(candidate, list(
      status = downloaded$status,
      text = downloaded$text,
      fetch_note = downloaded$note,
      text_sha256 = if (nzchar(downloaded$text)) sha256_text(downloaded$text) else ""
    ))
    documents[[length(documents) + 1L]] <- document
  }
  version$documents <- documents
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
  result$accessibility <- list(
    http_status = metadata_response$status,
    accessible = metadata_response$status == 200L,
    metadata = compact_metadata(metadata_response$body),
    error = metadata_response$error
  )
  result$pre <- collect_version(repository, PRE_UNTIL, metadata_response$status == 200L, token)
  result$post <- collect_version(repository, POST_UNTIL, metadata_response$status == 200L, token)
  result
}

is_reusable <- function(record, source_hash) {
  !is.null(record) && identical(scalar_text(record$source_sha256, ""), source_hash) &&
    identical(scalar_text(record$collector_protocol_version, ""), COLLECTOR_PROTOCOL_VERSION)
}

run_collection <- function(options) {
  dir.create(options$output, recursive = TRUE, showWarnings = FALSE)
  checkpoint <- file.path(options$output, "repository_results.jsonl")
  sample <- read_sample(options$input)
  validate_open_source_sample(sample)
  source_hash <- sha256_file(options$input)
  cached <- index_records(read_jsonl(checkpoint))
  repositories <- as.character(sample$repository)
  pending <- repositories[!vapply(repositories, function(repository) is_reusable(cached[[repository]], source_hash), logical(1L))]
  cat(sprintf("Amostra open source=%d; checkpoint=%d; pendentes=%d; pré=%s; pós=%s; trabalhadores=%d\n", nrow(sample), length(cached), length(pending), PRE_UNTIL, POST_UNTIL, options$workers))
  if (length(pending)) {
    token <- Sys.getenv("GITHUB_TOKEN", unset = "")
    if (!nzchar(token)) token <- read_dotenv_token()
    if (!nzchar(token)) stop("GITHUB_TOKEN não encontrado no ambiente ou em .env.")
    for (index in seq_along(pending)) {
      repository <- pending[[index]]
      row <- sample[match(repository, sample$repository), , drop = FALSE]
      record <- tryCatch(
        collect_one(row, source_hash, token),
        error = function(error) list(repository = repository, input_row = as.list(row), fatal_error = conditionMessage(error))
      )
      write_jsonl(list(record), checkpoint, append = TRUE)
      cat(sprintf("coletado %d/%d: %s\n", index, length(pending), repository))
    }
  }
  cat(sprintf("Checkpoint gravado em %s\n", normalizePath(checkpoint, mustWork = FALSE)))
  invisible(checkpoint)
}

if (sys.nframe() == 0L) {
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  project_dir <- if (length(script_arg)) normalizePath(file.path(dirname(sub("^--file=", "", script_arg[[1L]])), ".."), mustWork = TRUE) else getwd()
  setwd(project_dir)
  run_collection(parse_options(commandArgs(trailingOnly = TRUE)))
}

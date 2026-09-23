SELECTION_GDPR_DATE <- as.character(setting("selection", "gdpr_date", "2018-05-25T00:00:00Z"))
SELECTION_TOPICS <- unlist(setting("selection", "topics", c("artificial-intelligence", "deep-learning", "machine-learning")), use.names = FALSE)
SELECTION_CREATED_START <- as.character(setting("selection", "search_start_date", "2008-01-01"))
SELECTION_CREATED_END <- format(as.Date(substr(SELECTION_GDPR_DATE, 1L, 10L)) - 1L, "%Y-%m-%d")
SELECTION_MIN_STARS <- as.integer(setting("selection", "min_stars", 500L))
SELECTION_MIN_ISSUES <- as.integer(setting("selection", "min_issues", 100L))
SELECTION_MIN_ACTIVITY_MONTHS <- as.numeric(setting("selection", "min_activity_months", 24))
SELECTION_SEARCH_PAGE_SIZE <- 100L
SELECTION_MAX_RESULTS_PER_QUERY <- as.integer(setting("selection", "max_results_per_query", 1000L))
SELECTION_PROTOCOL_VERSION <- "topic-expanded-partitioned-2026-09"

selection_configuration <- function() {
  list(
    protocol_version = SELECTION_PROTOCOL_VERSION,
    topics = as.character(SELECTION_TOPICS),
    gdpr_date = SELECTION_GDPR_DATE,
    search_start_date = SELECTION_CREATED_START,
    search_end_date = SELECTION_CREATED_END,
    search_page_size = SELECTION_SEARCH_PAGE_SIZE,
    max_results_per_query = SELECTION_MAX_RESULTS_PER_QUERY,
    min_stars = SELECTION_MIN_STARS,
    min_issues = SELECTION_MIN_ISSUES,
    min_activity_months = SELECTION_MIN_ACTIVITY_MONTHS
  )
}

selection_configuration_fingerprint <- function() {
  payload <- jsonlite::toJSON(selection_configuration(), auto_unbox = TRUE, null = "null", digits = 16)
  sha256_text(payload)
}

selection_timestamp <- function() format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")

new_selection_run_id <- function() {
  paste0(format(Sys.time(), tz = "UTC", format = "%Y%m%dT%H%M%S"), "-", Sys.getpid())
}

read_selection_state <- function(path = selection_state_path()) {
  if (!file.exists(path)) return(NULL)
  tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(...) NULL)
}

selection_progress_files <- function() {
  directory <- selection_progress_path()
  list(
    directory = directory,
    candidates = file.path(directory, "candidates.json"),
    evaluations = file.path(directory, "evaluations.jsonl"),
    issue_counts = file.path(directory, "issue_counts.json")
  )
}

clear_selection_progress <- function() {
  files <- selection_progress_files()
  dir.create(files$directory, recursive = TRUE, showWarnings = FALSE)
  unlink(unlist(files[c("candidates", "evaluations", "issue_counts")], use.names = FALSE), force = TRUE)
  invisible(files)
}

begin_selection_state <- function(operation, output) {
  state <- list(
    status = "running",
    operation = operation,
    run_id = new_selection_run_id(),
    pid = Sys.getpid(),
    started_at = selection_timestamp(),
    updated_at = selection_timestamp(),
    sample_path = project_relative(output),
    protocol_version = SELECTION_PROTOCOL_VERSION,
    configuration_fingerprint = selection_configuration_fingerprint(),
    evaluated_candidates = 0L,
    total_candidates = 0L
  )
  # A seleção sempre começa do zero; os arquivos transitórios servem apenas ao diagnóstico.
  clear_selection_progress()
  atomic_write_json(state, selection_state_path(), pretty = TRUE)
  state
}

fail_selection_state <- function(state, error) {
  current <- read_selection_state()
  if (is.list(current) && identical(scalar_text(current$run_id, ""), scalar_text(state$run_id, ""))) state <- current
  state$status <- "failed"
  state$updated_at <- selection_timestamp()
  state$error <- conditionMessage(error)
  atomic_write_json(state, selection_state_path(), pretty = TRUE)
  invisible(state)
}

pause_selection_state <- function(state, error) {
  current <- read_selection_state()
  if (is.list(current) && identical(scalar_text(current$run_id, ""), scalar_text(state$run_id, ""))) state <- current
  state$status <- "paused"
  state$updated_at <- selection_timestamp()
  state$error <- conditionMessage(error)
  state$rate_limit_retry_at <- api_rate_retry_at()
  atomic_write_json(state, selection_state_path(), pretty = TRUE)
  cat("Seleção pausada pelo limite da API do GitHub. A próxima execução começará do zero.\n")
  invisible(state)
}

complete_selection_state <- function(state, result) {
  if (!is.list(result) || !nzchar(scalar_text(result$sample_sha256, ""))) {
    stop("A seleção terminou sem produzir um hash de amostra válido.", call. = FALSE)
  }
  state$status <- "completed"
  state$updated_at <- selection_timestamp()
  state$completed_at <- state$updated_at
  state$sample_sha256 <- scalar_text(result$sample_sha256, "")
  state$sample_rows <- scalar_int(result$rows, 0L)
  state$evaluated_candidates <- scalar_int(result$evaluated_candidates, state$evaluated_candidates %||% 0L)
  state$total_candidates <- scalar_int(result$total_candidates, state$total_candidates %||% 0L)
  state$selection_audit <- if (file.exists(selection_audit_path())) project_relative(selection_audit_path()) else NULL
  atomic_write_json(state, selection_state_path(), pretty = TRUE)
  clear_selection_progress()
  invisible(state)
}

assert_current_selection_valid <- function(sample = sample_path()) {
  state <- read_selection_state()
  if (!is.list(state) || !identical(scalar_text(state$status, ""), "completed")) {
    stop("Não há uma seleção de amostra concluída. Execute Rscript main.R --select para gerar uma nova amostra.", call. = FALSE)
  }
  if (!identical(scalar_text(state$operation, ""), "expanded-search")) {
    stop("A amostra atual não foi gerada pelo protocolo de seleção vigente. Execute Rscript main.R para gerar uma nova amostra.", call. = FALSE)
  }
  if (!identical(scalar_text(state$protocol_version, ""), SELECTION_PROTOCOL_VERSION) ||
      !identical(scalar_text(state$configuration_fingerprint, ""), selection_configuration_fingerprint())) {
    stop("A configuração da seleção mudou desde a amostra atual. Execute Rscript main.R para gerar uma nova amostra.", call. = FALSE)
  }
  expected_path <- project_relative(sample)
  if (!identical(scalar_text(state$sample_path, ""), expected_path) || !file.exists(sample)) {
    stop("A amostra associada à seleção não está disponível no caminho esperado. Execute Rscript main.R.", call. = FALSE)
  }
  current_hash <- sha256_file(sample)
  if (!identical(current_hash, scalar_text(state$sample_sha256, ""))) {
    stop("A amostra foi alterada desde a seleção. Execute Rscript main.R para gerar uma nova amostra.", call. = FALSE)
  }
  audit_path <- selection_audit_path()
  audit <- read_selection_state(audit_path)
  if (!is.list(audit) ||
      !identical(scalar_text(audit$selection_run_id, ""), scalar_text(state$run_id, "")) ||
      !identical(scalar_text(audit$sample_sha256, ""), current_hash)) {
    stop("O manifesto não corresponde à seleção atual. Execute Rscript main.R para gerar uma nova amostra.", call. = FALSE)
  }
  invisible(state)
}

read_approved_spdx <- function(path = reference_spdx_path()) {
  if (!file.exists(path)) stop(sprintf("Lista SPDX/OSI não encontrada: %s", path))
  values <- trimws(readLines(path, encoding = "UTF-8", warn = FALSE))
  values <- values[nzchar(values) & !startsWith(values, "#")]
  if (!length(values)) stop("A lista SPDX/OSI está vazia.")
  unique(values)
}

selection_time <- function(value) {
  value <- scalar_text(value, "")
  if (!nzchar(value)) return(as.POSIXct(NA, tz = "UTC"))
  parsed <- suppressWarnings(as.POSIXct(value, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"))
  if (is.na(parsed)) parsed <- suppressWarnings(as.POSIXct(value, tz = "UTC"))
  parsed
}

format_selection_time <- function(value) {
  if (length(value) == 0L || is.na(value)) "" else format(value, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

selection_api <- function(path, params, token, return_headers = FALSE) {
  response <- github_api(path, params, token, return_headers = return_headers)
  if (!isTRUE(api_response_ok(response))) {
    status <- response$status %||% 0L
    stop(if (nzchar(response$error %||% "")) response$error else sprintf("GitHub retornou HTTP %s.", status), call. = FALSE)
  }
  response
}

last_page_from_headers <- function(headers) {
  link <- grep("^Link:", headers, value = TRUE, ignore.case = TRUE)
  if (!length(link)) return(1L)
  match <- regexec("[?&]page=([0-9]+)[^>]*>;[[:space:]]*rel=\"last\"", link[[length(link)]], perl = TRUE)
  captured <- regmatches(link[[length(link)]], match)[[1L]]
  if (length(captured) >= 2L) as.integer(captured[[2L]]) else 1L
}

commit_date <- function(commit) {
  details <- commit$commit %||% list()
  committer <- details$committer %||% list()
  author <- details$author %||% list()
  selection_time(scalar_text(committer$date, scalar_text(author$date, "")))
}

commits_path <- function(repository) paste0("/repos/", repository, "/commits")

graphql_string <- function(value) jsonlite::toJSON(as.character(value), auto_unbox = TRUE)

# GraphQL exclui pull requests e agrupa a contagem de issues.
count_real_issues_batch <- function(repositories, token, batch_size = 50L) {
  repositories <- unique(as.character(repositories))
  repositories <- repositories[nzchar(repositories)]
  if (!length(repositories)) return(setNames(integer(), character()))
  batch_size <- max(1L, min(50L, as.integer(batch_size)))
  issue_counts <- setNames(integer(length(repositories)), repositories)
  starts <- seq.int(1L, length(repositories), by = batch_size)
  for (batch_index in seq_along(starts)) {
    start <- starts[[batch_index]]
    batch <- repositories[start:min(length(repositories), start + batch_size - 1L)]
    aliases <- paste0("r", seq_along(batch))
    fields <- vapply(seq_along(batch), function(index) {
      parts <- strsplit(batch[[index]], "/", fixed = TRUE)[[1L]]
      if (length(parts) != 2L) stop(sprintf("Repositório inválido para GraphQL: %s", batch[[index]]), call. = FALSE)
      sprintf(
        "%s: repository(owner: %s, name: %s) { issues(first: 1, states: [OPEN, CLOSED]) { totalCount } }",
        aliases[[index]], graphql_string(parts[[1L]]), graphql_string(parts[[2L]])
      )
    }, character(1L))
    query <- paste0("query { ", paste(fields, collapse = " "), " }")
    response <- github_graphql(query, token)
    if (!isTRUE(api_response_ok(response))) {
      stop(if (nzchar(response$error %||% "")) response$error else "Falha ao consultar contagens de issues via GraphQL.", call. = FALSE)
    }
    data <- (response$body %||% list())$data %||% list()
    for (index in seq_along(batch)) {
      node <- data[[aliases[[index]]]] %||% list()
      issue_counts[[batch[[index]]]] <- scalar_int((node$issues %||% list())$totalCount, 0L)
    }
  }
  issue_counts
}

commit_activity <- function(repository, token) {
  path <- commits_path(repository)
  newest <- selection_api(path, list(per_page = 1L), token, return_headers = TRUE)
  values <- newest$body
  if (!is.list(values) || !length(values)) return(NULL)
  last_commit <- commit_date(values[[1L]])
  if (is.na(last_commit)) return(NULL)
  last_page <- last_page_from_headers(newest$headers %||% character())
  oldest <- if (last_page > 1L) selection_api(path, list(per_page = 1L, page = last_page), token)$body else values
  if (!is.list(oldest) || !length(oldest)) return(NULL)
  first_commit <- commit_date(oldest[[1L]])
  if (is.na(first_commit)) return(NULL)
  days <- as.numeric(difftime(last_commit, first_commit, units = "days"))
  list(
    first = first_commit,
    last = last_commit,
    months = max(0, days / 30.44)
  )
}

activity_window <- function(repository, token) {
  path <- commits_path(repository)
  pre <- selection_api(path, list(until = SELECTION_GDPR_DATE, per_page = 1L), token)$body
  post <- selection_api(path, list(since = SELECTION_GDPR_DATE, per_page = 1L), token)$body
  list(
    pre = is.list(pre) && length(pre) > 0L,
    post = is.list(post) && length(post) > 0L
  )
}

selection_repository_query <- function(topic, start_date, end_date) {
  paste(
    "is:public",
    paste0("topic:", topic),
    paste0("stars:>=", SELECTION_MIN_STARS),
    paste0("created:", start_date, "..", end_date),
    "fork:false",
    "archived:false"
  )
}

# Divide a consulta até que cada partição caiba no limite da Search API.
partition_topic_query <- function(topic, start_date = SELECTION_CREATED_START,
                                  end_date = SELECTION_CREATED_END, token) {
  start <- as.Date(start_date)
  end <- as.Date(end_date)
  if (is.na(start) || is.na(end) || end < start) {
    stop(sprintf("Janela temporal inválida para o tópico %s.", topic))
  }
  query <- selection_repository_query(topic, format(start), format(end))
  response <- selection_api("/search/repositories", list(q = query, per_page = 1L), token)
  body <- response$body %||% list()
  if (scalar_bool(body$incomplete_results, FALSE)) {
    stop(sprintf("A Search API devolveu resultados incompletos para: %s", query))
  }
  total <- scalar_int(body$total_count, 0L)
  if (total <= SELECTION_MAX_RESULTS_PER_QUERY) {
    return(list(list(query = query, start = format(start), end = format(end), total_count = total)))
  }
  if (start >= end) {
    stop(sprintf("A consulta ainda excede %d resultados em um único dia: %s", SELECTION_MAX_RESULTS_PER_QUERY, query))
  }
  midpoint <- start + floor(as.numeric(end - start) / 2)
  c(
    partition_topic_query(topic, format(start), format(midpoint), token),
    partition_topic_query(topic, format(midpoint + 1L), format(end), token)
  )
}

download_search_partition <- function(partition, token) {
  total <- scalar_int(partition$total_count, 0L)
  pages <- if (total == 0L) 0L else ceiling(total / SELECTION_SEARCH_PAGE_SIZE)
  repositories <- list()
  downloaded <- 0L
  for (page in seq_len(pages)) {
    response <- selection_api(
      "/search/repositories",
      list(q = partition$query, sort = "stars", order = "desc", per_page = SELECTION_SEARCH_PAGE_SIZE, page = page),
      token
    )
    body <- response$body %||% list()
    if (scalar_bool(body$incomplete_results, FALSE)) {
      stop(sprintf("A Search API devolveu resultados incompletos para: %s", partition$query))
    }
    items <- body$items %||% list()
    if (!is.list(items)) items <- list()
    if (page < pages && length(items) < SELECTION_SEARCH_PAGE_SIZE) {
      stop(sprintf("A consulta mudou durante a paginação e ficou incompleta: %s", partition$query))
    }
    repositories <- c(repositories, items)
    downloaded <- downloaded + length(items)
  }

  if (downloaded != total) {
    stop(sprintf("A consulta retornou %d de %d resultados esperados: %s", downloaded, total, partition$query), call. = FALSE)
  }
  list(
    repositories = repositories,
    audit = list(
      query = partition$query,
      created_start = partition$start,
      created_end = partition$end,
      total_count = total,
      pages = pages,
      downloaded = downloaded
    )
  )
}

search_by_topic <- function(topic, token) {
  partitions <- partition_topic_query(topic, token = token)
  repositories <- list()
  partition_audit <- vector("list", length(partitions))
  for (index in seq_along(partitions)) {
    result <- download_search_partition(partitions[[index]], token)
    repositories <- c(repositories, result$repositories)
    partition_audit[[index]] <- result$audit
  }
  list(
    repositories = repositories,
    audit = list(topic = topic, partitions = partition_audit, downloaded = length(repositories))
  )
}

search_candidates <- function(token) {
  candidates <- list()
  names_in_order <- character()
  topic_audit <- list()
  for (topic_index in seq_along(SELECTION_TOPICS)) {
    topic <- SELECTION_TOPICS[[topic_index]]
    cat(sprintf("Busca %d/%d | tópico: %s\n", topic_index, length(SELECTION_TOPICS), topic))
    result <- search_by_topic(topic, token)
    topic_audit[[length(topic_audit) + 1L]] <- result$audit
    for (repository in result$repositories) {
      full_name <- scalar_text(repository$full_name, "")
      if (!nzchar(full_name)) next
      if (is.null(candidates[[full_name]])) {
        candidates[[full_name]] <- list(repository = repository, topics = topic)
        names_in_order <- c(names_in_order, full_name)
      } else {
        candidates[[full_name]]$topics <- unique(c(candidates[[full_name]]$topics, topic))
      }
    }
    cat(sprintf("  Resultado: %d repositórios encontrados.\n", length(result$repositories)))
  }
  list(
    candidates = unname(candidates[names_in_order]),
    audit = list(
      protocol_version = SELECTION_PROTOCOL_VERSION,
      created_start = SELECTION_CREATED_START,
      created_end = SELECTION_CREATED_END,
      gdpr_date = SELECTION_GDPR_DATE,
      topics = SELECTION_TOPICS,
      min_stars = SELECTION_MIN_STARS,
      min_issues = SELECTION_MIN_ISSUES,
      min_activity_months = SELECTION_MIN_ACTIVITY_MONTHS,
      api_search_interval_seconds = as.numeric(setting("api", "search_interval_seconds", 2.2)),
      api_max_results_per_query = SELECTION_MAX_RESULTS_PER_QUERY,
      topic_searches = topic_audit
    )
  )
}

is_public_repository <- function(repository) {
  visibility <- scalar_text(repository$visibility, "")
  if (nzchar(visibility)) return(tolower(visibility) == "public")
  !scalar_bool(repository$private, TRUE)
}

candidate_passes_static_filters <- function(candidate, approved_ids) {
  repository <- candidate$repository
  created_at <- selection_time(repository$created_at)
  license <- repository$license %||% list()
  is_public_repository(repository) &&
    !scalar_bool(repository$fork, TRUE) &&
    !scalar_bool(repository$archived, TRUE) &&
    !is.na(created_at) && created_at < selection_time(SELECTION_GDPR_DATE) &&
    scalar_int(repository$stargazers_count, 0L) >= SELECTION_MIN_STARS &&
    scalar_text(license$spdx_id, "") %in% approved_ids
}

write_selection_candidates_checkpoint <- function(found, state) {
  files <- selection_progress_files()
  dir.create(files$directory, recursive = TRUE, showWarnings = FALSE)
  payload <- list(
    run_id = state$run_id,
    protocol_version = SELECTION_PROTOCOL_VERSION,
    configuration_fingerprint = selection_configuration_fingerprint(),
    sample_path = state$sample_path,
    candidates = found$candidates,
    audit = found$audit
  )
  atomic_write_json(payload, files$candidates, pretty = FALSE)
  atomic_write_lines(character(), files$evaluations)
  unlink(files$issue_counts, force = TRUE)
  invisible(files)
}

read_selection_candidates_checkpoint <- function(state) {
  files <- selection_progress_files()
  if (!file.exists(files$candidates)) return(NULL)
  payload <- tryCatch(jsonlite::fromJSON(files$candidates, simplifyVector = FALSE), error = function(...) NULL)
  if (!is.list(payload) ||
      !identical(scalar_text(payload$run_id, ""), scalar_text(state$run_id, "")) ||
      !identical(scalar_text(payload$protocol_version, ""), SELECTION_PROTOCOL_VERSION) ||
      !identical(scalar_text(payload$configuration_fingerprint, ""), selection_configuration_fingerprint()) ||
      !identical(scalar_text(payload$sample_path, ""), scalar_text(state$sample_path, ""))) return(NULL)
  candidates <- payload$candidates %||% list()
  if (!is.list(candidates)) return(NULL)
  list(candidates = candidates, audit = payload$audit %||% list())
}

write_selection_issue_counts <- function(state, issue_counts) {
  files <- selection_progress_files()
  dir.create(files$directory, recursive = TRUE, showWarnings = FALSE)
  atomic_write_json(
    list(
      run_id = state$run_id,
      configuration_fingerprint = selection_configuration_fingerprint(),
      issue_counts = as.list(issue_counts)
    ),
    files$issue_counts,
    pretty = FALSE
  )
  invisible(files$issue_counts)
}

read_selection_issue_counts <- function(state) {
  files <- selection_progress_files()
  if (!file.exists(files$issue_counts)) return(NULL)
  payload <- tryCatch(jsonlite::fromJSON(files$issue_counts, simplifyVector = FALSE), error = function(...) NULL)
  if (!is.list(payload) ||
      !identical(scalar_text(payload$run_id, ""), scalar_text(state$run_id, "")) ||
      !identical(scalar_text(payload$configuration_fingerprint, ""), selection_configuration_fingerprint())) return(NULL)
  values <- payload$issue_counts %||% list()
  if (!is.list(values)) return(NULL)
  vapply(values, scalar_int, integer(1L), default = 0L)
}

read_selection_evaluations <- function() {
  path <- selection_progress_files()$evaluations
  records <- read_jsonl(path)
  indexed <- list()
  for (record in records) {
    index <- scalar_int(record$index, 0L)
    if (index > 0L) indexed[[as.character(index)]] <- record
  }
  indexed
}

append_selection_evaluation <- function(index, repository, result = NULL, reason = "") {
  record <- list(
    index = index,
    repository = repository,
    status = if (is.null(result)) "rejected" else "selected"
  )
  if (is.null(result)) record$reason <- reason else record$record <- result
  write_jsonl(list(record), selection_progress_files()$evaluations, append = TRUE)
  record
}

update_selection_progress_state <- function(state, evaluated, total, repository = "") {
  previous <- selection_time(scalar_text(state$last_persisted_at, scalar_text(state$updated_at, "")))
  now <- Sys.time()
  state$evaluated_candidates <- as.integer(evaluated)
  state$total_candidates <- as.integer(total)
  state$last_repository <- repository
  state$updated_at <- format(now, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
  interval <- suppressWarnings(as.numeric(setting("selection", "progress_update_interval_seconds", 30)))
  if (is.na(interval) || interval < 0) interval <- 30
  should_write <- is.na(previous) || evaluated >= total || as.numeric(difftime(now, previous, units = "secs")) >= interval
  if (should_write) {
    state$last_persisted_at <- state$updated_at
    atomic_write_json(state, selection_state_path(), pretty = TRUE)
  }
  state
}

evaluate_candidate <- function(candidate, approved_ids, token, issue_count = NULL) {
  repository <- candidate$repository
  full_name <- scalar_text(repository$full_name, "")
  if (!candidate_passes_static_filters(candidate, approved_ids)) return(NULL)
  stars <- scalar_int(repository$stargazers_count, 0L)
  license <- repository$license %||% list()
  spdx_id <- scalar_text(license$spdx_id, "")
  issues <- scalar_int(issue_count, 0L)
  if (issues < SELECTION_MIN_ISSUES) return(NULL)
  activity <- commit_activity(full_name, token)
  if (is.null(activity) || activity$months < SELECTION_MIN_ACTIVITY_MONTHS) return(NULL)
  window <- activity_window(full_name, token)
  if (!window$pre || !window$post) return(NULL)
  list(
    repository = full_name,
    url = scalar_text(repository$html_url, ""),
    name = scalar_text(repository$name, ""),
    owner = scalar_text((repository$owner %||% list())$login, ""),
    description = scalar_text(repository$description, ""),
    language = scalar_text(repository$language, ""),
    license_spdx_id = spdx_id,
    license_name = scalar_text(license$name, ""),
    license_osi_approved = "true",
    stars = stars,
    issues = issues,
    created_at = format_selection_time(created_at),
    updated_at = format_selection_time(selection_time(repository$updated_at)),
    first_commit_at = format_selection_time(activity$first),
    last_commit_at = format_selection_time(activity$last),
    activity_months = round(activity$months, 2),
    ai_ml_match_topics = paste(candidate$topics, collapse = "; "),
    has_pre_gdpr_activity = "true",
    has_post_gdpr_activity = "true"
  )
}

records_to_selection_data_frame <- function(records) {
  columns <- c(
    "repository", "url", "name", "owner", "description", "language",
    "license_spdx_id", "license_name", "license_osi_approved", "stars", "issues",
    "created_at", "updated_at", "first_commit_at", "last_commit_at", "activity_months",
    "ai_ml_match_topics", "has_pre_gdpr_activity", "has_post_gdpr_activity"
  )
  if (!length(records)) return(as.data.frame(setNames(replicate(length(columns), character(), simplify = FALSE), columns), check.names = FALSE))
  values <- lapply(columns, function(column) vapply(records, function(record) as.character(record[[column]] %||% ""), character(1L)))
  names(values) <- columns
  as.data.frame(values, stringsAsFactors = FALSE, check.names = FALSE)
}

write_selection <- function(records, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  data <- records_to_selection_data_frame(records)
  temporary <- paste0(path, ".partial")
  write.csv(data, temporary, row.names = FALSE, fileEncoding = "UTF-8", na = "")
  if (!file.rename(temporary, path)) {
    unlink(temporary, force = TRUE)
    stop(sprintf("Não foi possível finalizar a amostra: %s", path), call. = FALSE)
  }
  hash_path <- sample_hash_path()
  atomic_write_lines(
    c(
      paste0("sha256  ", sha256_file(path)),
      paste0("source  ", basename(path)),
      paste0("rows  ", nrow(data))
    ),
    hash_path
  )
  list(path = path, sample_sha256 = sha256_file(path), rows = nrow(data))
}

load_or_search_selection_candidates <- function(token, state) {
  found <- read_selection_candidates_checkpoint(state)
  if (is.null(found)) {
    found <- search_candidates(token)
    write_selection_candidates_checkpoint(found, state)
  }
  if (!length(found$candidates)) stop("A seleção não encontrou candidatos.", call. = FALSE)
  found
}

resolve_selection_issue_counts <- function(candidates, state, token, approved_ids) {
  issue_counts <- read_selection_issue_counts(state)
  static_candidates <- candidates[vapply(
    candidates, candidate_passes_static_filters, logical(1L), approved_ids = approved_ids
  )]
  static_repositories <- vapply(
    static_candidates,
    function(candidate) scalar_text(candidate$repository$full_name, ""),
    character(1L)
  )
  cached_repositories <- names(issue_counts %||% setNames(integer(), character()))
  missing_repositories <- setdiff(static_repositories, cached_repositories)
  if (!length(missing_repositories)) return(issue_counts)

  new_counts <- count_real_issues_batch(missing_repositories, token)
  issue_counts <- c(issue_counts %||% setNames(integer(), character()), new_counts)
  write_selection_issue_counts(state, issue_counts)
  issue_counts
}

is_selected_evaluation <- function(evaluation) {
  is.list(evaluation) && identical(scalar_text(evaluation$status, ""), "selected")
}

count_selected_evaluations <- function(evaluations, candidate_count) {
  sum(vapply(seq_len(candidate_count), function(index) {
    is_selected_evaluation(evaluations[[as.character(index)]])
  }, logical(1L)))
}

evaluate_candidate_safely <- function(candidate, approved_ids, token, issue_count) {
  tryCatch(
    evaluate_candidate(candidate, approved_ids, token, issue_count = issue_count),
    error = function(error) {
      if (api_error_is_rate_limited(error)) stop(error)
      NULL
    }
  )
}

evaluate_pending_candidates <- function(candidates, issue_counts, approved_ids, token, state) {
  evaluations <- read_selection_evaluations()
  evaluated_indices <- suppressWarnings(as.integer(names(evaluations)))
  evaluated_indices <- evaluated_indices[
    !is.na(evaluated_indices) & evaluated_indices >= 1L & evaluated_indices <= length(candidates)
  ]
  pending <- setdiff(seq_along(candidates), evaluated_indices)
  approved_count <- count_selected_evaluations(evaluations, length(candidates))
  last_report <- Sys.time()

  for (index in pending) {
    candidate <- candidates[[index]]
    name <- scalar_text(candidate$repository$full_name, "")
    issue_count <- if (!is.null(issue_counts) && name %in% names(issue_counts)) issue_counts[[name]] else NULL
    result <- evaluate_candidate_safely(candidate, approved_ids, token, issue_count)
    evaluation <- append_selection_evaluation(
      index, name, result,
      if (is.null(result)) "não atende a um critério de elegibilidade" else ""
    )
    evaluations[[as.character(index)]] <- evaluation
    if (!is.null(result)) approved_count <- approved_count + 1L
    state <- update_selection_progress_state(state, length(evaluations), length(candidates), name)
    now <- Sys.time()
    should_report <- index == tail(pending, 1L) || as.numeric(difftime(now, last_report, units = "secs")) >= 60
    if (should_report) {
      cat(sprintf("Repositórios aprovados: %d/%d\n", approved_count, length(candidates)))
      last_report <- now
    }
  }
  if (!length(pending)) cat(sprintf("Repositórios aprovados: %d/%d\n", approved_count, length(candidates)))
  evaluations
}

selected_repositories_from_evaluations <- function(candidates, evaluations) {
  selected_indices <- which(vapply(seq_along(candidates), function(index) {
    is_selected_evaluation(evaluations[[as.character(index)]])
  }, logical(1L)))
  lapply(selected_indices, function(index) evaluations[[as.character(index)]]$record)
}

finalize_selection <- function(found, candidates, evaluations, output, state, selection_run_id) {
  selected <- selected_repositories_from_evaluations(candidates, evaluations)
  if (!length(selected)) {
    stop("A seleção não produziu nenhum repositório elegível; a amostra anterior foi preservada e não pode ser usada nesta execução.", call. = FALSE)
  }
  found$audit$unique_candidates <- length(candidates)
  found$audit$selected_repositories <- length(selected)
  found$audit$generated_at <- selection_timestamp()
  found$audit$selection_run_id <- scalar_text(state$run_id, selection_run_id)

  selection_written <- write_selection(selected, output)
  found$audit$sample_sha256 <- selection_written$sample_sha256
  found$audit$sample_rows <- selection_written$rows
  audit_path <- resolve_project_path(selection_audit_path())
  dir.create(dirname(audit_path), recursive = TRUE, showWarnings = FALSE)
  atomic_write_json(found$audit, audit_path, pretty = TRUE)
  selection_written$evaluated_candidates <- length(candidates)
  selection_written$total_candidates <- length(candidates)
  invisible(selection_written)
}

run_full_selection <- function(output, token, approved_ids, selection_run_id = "", state = NULL) {
  state <- state %||% read_selection_state()
  if (!is.list(state)) {
    state <- list(
      run_id = selection_run_id,
      sample_path = project_relative(output),
      protocol_version = SELECTION_PROTOCOL_VERSION,
      configuration_fingerprint = selection_configuration_fingerprint()
    )
  }
  found <- load_or_search_selection_candidates(token, state)
  candidates <- found$candidates
  state <- update_selection_progress_state(state, length(read_selection_evaluations()), length(candidates))
  issue_counts <- resolve_selection_issue_counts(candidates, state, token, approved_ids)
  evaluations <- evaluate_pending_candidates(candidates, issue_counts, approved_ids, token, state)
  finalize_selection(found, candidates, evaluations, output, state, selection_run_id)
}

run_selection <- function(options) {
  output <- resolve_project_path(options$output %||% sample_path())
  operation <- "expanded-search"
  state <- begin_selection_state(operation, output)
  tryCatch({
    token <- Sys.getenv("GITHUB_TOKEN", unset = "")
    if (!nzchar(token)) token <- read_dotenv_token()
    if (!nzchar(token)) stop("GITHUB_TOKEN não encontrado no ambiente ou em .env.")
    approved_ids <- read_approved_spdx()
    result <- run_full_selection(output, token, approved_ids, state$run_id, state = state)
    complete_selection_state(state, result)
    invisible(output)
  }, error = function(error) {
    if (identical(operation, "expanded-search") && selection_rate_limit_error(error)) {
      pause_selection_state(state, error)
    } else {
      fail_selection_state(state, error)
    }
    stop(error)
  })
}

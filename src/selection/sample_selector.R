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
    message <- if (nzchar(response$error %||% "")) response$error else sprintf("GitHub retornou HTTP %s.", status)
    condition <- structure(list(message = message, call = NULL, status = as.integer(status)),
                           class = c("github_api_error", "error", "condition"))
    stop(condition)
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
    created_at = format_selection_time(selection_time(repository$created_at)),
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



SampleSelector <- R6::R6Class(
  "SampleSelector",
  public = list(
    config = NULL,
    client = NULL,
    store = NULL,
    initialize = function(config, client, store) {
      self$config <- config
      self$client <- client
      self$store <- store
    },
    run = function(output = self$store$sample_path()) {
      set_github_client(self$client)
      token <- Sys.getenv("GITHUB_TOKEN", unset = "")
      if (!nzchar(token)) token <- read_dotenv_token(file.path(self$config$root, ".env"))
      if (!nzchar(token)) stop("GITHUB_TOKEN não encontrado no ambiente ou em .env.", call. = FALSE)

      approved_ids <- read_approved_spdx(self$config$path("reference_spdx", "inputs/reference/osi_approved_spdx_ids.txt"))
      found <- search_candidates(token)
      candidates <- found$candidates
      eligible <- candidates[vapply(candidates, candidate_passes_static_filters, logical(1L), approved_ids = approved_ids)]
      if (!length(eligible)) stop("A busca não encontrou candidatos que atendam aos filtros básicos.", call. = FALSE)

      repositories <- vapply(eligible, function(candidate) scalar_text(candidate$repository$full_name, ""), character(1L))
      issues <- count_real_issues_batch(repositories, token)
      selected <- list()
      last_report <- Sys.time()
      for (index in seq_along(eligible)) {
        candidate <- eligible[[index]]
        name <- repositories[[index]]
        issue_count <- issues[[name]] %||% 0L
        if (issue_count >= SELECTION_MIN_ISSUES) {
          result <- tryCatch(
            evaluate_candidate(candidate, approved_ids, token, issue_count),
            error = function(error) {
              if (api_error_is_rate_limited(error)) stop(error)
              if (inherits(error, "github_api_error") && identical(as.integer(error$status), 404L)) return(NULL)
              stop(error)
            }
          )
          if (!is.null(result)) selected[[length(selected) + 1L]] <- result
        }
        now <- Sys.time()
        if (index == length(eligible) || as.numeric(difftime(now, last_report, units = "secs")) >= 60) {
          cat(sprintf("Repositórios elegíveis: %d/%d
", length(selected), length(eligible)))
          last_report <- now
        }
      }
      if (!length(selected)) stop("A seleção não produziu repositórios elegíveis; a amostra anterior foi preservada.", call. = FALSE)

      run_id <- paste0(format(Sys.time(), tz = "UTC", format = "%Y%m%dT%H%M%S"), "-", Sys.getpid())
      written <- write_selection(selected, resolve_project_path(output))
      found$audit$unique_candidates <- length(candidates)
      found$audit$selected_repositories <- written$rows
      found$audit$generated_at <- format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
      found$audit$selection_run_id <- run_id
      found$audit$sample_sha256 <- written$sample_sha256
      found$audit$sample_rows <- written$rows
      found$audit$configuration_fingerprint <- selection_configuration_fingerprint()
      atomic_write_json(found$audit, self$store$audit_path(), pretty = TRUE)
      invisible(written)
    }
  )
)

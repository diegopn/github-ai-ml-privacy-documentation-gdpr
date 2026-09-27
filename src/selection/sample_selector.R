SampleSelector <- R6::R6Class(
  "SampleSelector",
  public = list(
    initialize = function(config, client, store, values, searcher) {
      if (!inherits(config, "ProjectConfig")) stop("SampleSelector exige ProjectConfig.", call. = FALSE)
      if (!inherits(store, "ArtifactStore")) stop("SampleSelector exige ArtifactStore.", call. = FALSE)
      required_client_methods <- c(
        "assert_authenticated", "api", "graphql", "response_ok", "response_condition", "is_rate_limit_error"
      )
      if (!inherits(client, "R6") || !all(vapply(required_client_methods, function(method) {
        is.function(client[[method]])
      }, logical(1L)))) {
        stop("SampleSelector exige um cliente GitHub R6.", call. = FALSE)
      }
      if (!inherits(values, "ValueTools")) stop("SampleSelector exige ValueTools.", call. = FALSE)
      if (!inherits(searcher, "RepositorySearch")) stop("SampleSelector exige RepositorySearch.", call. = FALSE)
      private$config <- config
      private$client <- client
      private$store <- store
      private$values <- values
      private$searcher <- searcher
      invisible(self)
    },
    run = function(output = private$store$sample_path()) {
      private$client$assert_authenticated()
      settings <- private$config$selection_settings()
      approved_ids <- private$read_approved_spdx(
        private$config$path("reference_spdx", "inputs/reference/osi_approved_spdx_ids.txt")
      )
      found <- private$searcher$find_candidates(settings)
      eligible <- private$eligible_candidates(found$candidates, approved_ids, settings)
      if (!length(eligible)) {
        stop("A busca não encontrou candidatos que atendam aos filtros básicos.", call. = FALSE)
      }

      selected <- private$select_eligible(eligible, settings)
      if (!length(selected)) {
        stop("A seleção não produziu repositórios elegíveis; a amostra anterior foi preservada.", call. = FALSE)
      }
      sample <- private$records_to_selection_data_frame(selected)
      audit <- private$finalize_audit(found$audit, found$candidates, sample, settings)
      private$store$publish_sample(sample, audit, output)
    }
  ),
  private = list(
    config = NULL,
    client = NULL,
    store = NULL,
    values = NULL,
    searcher = NULL,
    read_approved_spdx = function(path) {
      if (!file.exists(path)) stop(sprintf("Lista SPDX/OSI não encontrada: %s", path), call. = FALSE)
      ids <- trimws(readLines(path, encoding = "UTF-8", warn = FALSE))
      ids <- ids[nzchar(ids) & !startsWith(ids, "#")]
      if (!length(ids)) stop("A lista SPDX/OSI está vazia.", call. = FALSE)
      unique(ids)
    },
    eligible_candidates = function(candidates, approved_ids, settings) {
      candidates[vapply(candidates, private$candidate_passes_static_filters, logical(1L),
        approved_ids = approved_ids, settings = settings)]
    },
    select_eligible = function(candidates, settings) {
      repositories <- vapply(candidates, function(candidate) {
        private$values$scalar_text(candidate$repository$full_name, "")
      }, character(1L))
      issues <- private$count_real_issues_batch(repositories)
      selected <- list()
      last_report <- Sys.time()

      for (index in seq_along(candidates)) {
        candidate <- candidates[[index]]
        repository <- repositories[[index]]
        issue_count <- issues[[repository]]
        if (issue_count >= settings$min_issues) {
          result <- private$evaluate_candidate_safely(candidate, settings, issue_count)
          if (!is.null(result)) selected[[length(selected) + 1L]] <- result
        }
        now <- Sys.time()
        if (index == length(candidates) || as.numeric(difftime(now, last_report, units = "secs")) >= 60) {
          cat(sprintf("Repositórios avaliados: %d/%d | selecionados: %d\n", index, length(candidates), length(selected)))
          last_report <- now
        }
      }
      selected
    },
    evaluate_candidate_safely = function(candidate, settings, issue_count) {
      tryCatch(
        private$evaluate_candidate(candidate, settings, issue_count),
        error = function(error) {
          if (private$client$is_rate_limit_error(error)) stop(error)
          if (inherits(error, "github_api_error") && identical(as.integer(error$status), 404L)) return(NULL)
          stop(error)
        }
      )
    },
    finalize_audit = function(audit, candidates, sample, settings) {
      audit$unique_candidates <- length(candidates)
      audit$selected_repositories <- nrow(sample)
      audit$generated_at <- format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
      audit$selection_run_id <- paste0(format(Sys.time(), tz = "UTC", format = "%Y%m%dT%H%M%S"), "-", Sys.getpid())
      configuration <- jsonlite::toJSON(
        private$config$selection_configuration(settings), auto_unbox = TRUE, null = "null", digits = 16
      )
      audit$configuration_fingerprint <- private$store$hash_text(configuration)
      audit
    },
    selection_time = function(value) {
      value <- private$values$scalar_text(value, "")
      if (!nzchar(value)) return(as.POSIXct(NA, tz = "UTC"))
      parsed <- suppressWarnings(as.POSIXct(value, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"))
      if (is.na(parsed)) parsed <- suppressWarnings(as.POSIXct(value, tz = "UTC"))
      parsed
    },
    selection_api = function(path, params, return_headers = FALSE) {
      response <- private$client$api(path, params, return_headers)
      if (!isTRUE(private$client$response_ok(response))) {
        stop(private$client$response_condition(response))
      }
      response
    },
    last_page_from_headers = function(headers) {
      link <- grep("^Link:", headers, value = TRUE, ignore.case = TRUE)
      if (!length(link)) return(1L)
      match <- regexec("[?&]page=([0-9]+)[^>]*>;[[:space:]]*rel=\"last\"", link[[length(link)]], perl = TRUE)
      captured <- regmatches(link[[length(link)]], match)[[1L]]
      if (length(captured) >= 2L) as.integer(captured[[2L]]) else 1L
    },
    selection_time_from_commit = function(commit) {
      details <- private$values$or_else(commit$commit, list())
      committer <- private$values$or_else(details$committer, list())
      author <- private$values$or_else(details$author, list())
      private$selection_time(private$values$scalar_text(committer$date, private$values$scalar_text(author$date, "")))
    },
    graphql_string = function(value) jsonlite::toJSON(as.character(value), auto_unbox = TRUE),
    count_real_issues_batch = function(repositories) {
      issue_counts <- setNames(integer(length(repositories)), repositories)

      batches <- split(repositories, ceiling(seq_along(repositories) / 50L))
      for (batch in batches) {
        query <- private$issue_count_query(batch)
        response <- private$client$graphql(query)
        if (!private$client$response_ok(response)) {
          stop(private$client$response_condition(response, "Falha ao consultar contagens de issues via GraphQL."))
        }
        data <- private$values$or_else(private$values$or_else(response$body, list())$data, NULL)
        if (!is.list(data)) {
          private$stop_invalid_graphql_response(response, "A resposta GraphQL não contém o objeto data.")
        }
        aliases <- paste0("r", seq_along(batch))
        for (index in seq_along(batch)) {
          issue_counts[[batch[[index]]]] <- private$issue_count_from_graphql(data, aliases[[index]], batch[[index]], response)
        }
      }
      issue_counts
    },
    issue_count_query = function(repositories) {
      fields <- vapply(seq_along(repositories), function(index) {
        parts <- strsplit(repositories[[index]], "/", fixed = TRUE)[[1L]]
        if (length(parts) != 2L || any(!nzchar(parts))) {
          stop(sprintf("Repositório inválido para GraphQL: %s", repositories[[index]]), call. = FALSE)
        }
        sprintf(
          "%s: repository(owner: %s, name: %s) { issues(first: 1, states: [OPEN, CLOSED]) { totalCount } }",
          paste0("r", index), private$graphql_string(parts[[1L]]), private$graphql_string(parts[[2L]])
        )
      }, character(1L))
      paste0("query { ", paste(fields, collapse = " "), " }")
    },
    issue_count_from_graphql = function(data, alias, repository, response) {
      if (is.null(names(data)) || !alias %in% names(data)) {
        private$stop_invalid_graphql_response(response, sprintf("A resposta GraphQL omitiu %s.", alias))
      }
      node <- data[[alias]]
      if (is.null(node)) return(0L)
      issues <- if (is.list(node)) node$issues else NULL
      count <- if (is.list(issues)) suppressWarnings(as.numeric(issues$totalCount)) else numeric()
      if (length(count) != 1L || !is.finite(count) || count < 0 || count != floor(count)) {
        private$stop_invalid_graphql_response(
          response,
          sprintf("A resposta GraphQL não contém uma contagem válida de issues para %s.", repository)
        )
      }
      as.integer(count)
    },
    stop_invalid_graphql_response = function(response, message) {
      response$error <- message
      stop(private$client$response_condition(response, message))
    },
    commits_path = function(repository) paste0("/repos/", repository, "/commits"),
    commit_activity = function(repository) {
      path <- private$commits_path(repository)
      newest <- private$selection_api(path, list(per_page = 1L), return_headers = TRUE)
      if (!is.list(newest$body) || !length(newest$body)) return(NULL)
      last_commit <- private$selection_time_from_commit(newest$body[[1L]])
      if (is.na(last_commit)) return(NULL)
      last_page <- private$last_page_from_headers(private$values$or_else(newest$headers, character()))
      oldest <- if (last_page > 1L) {
        private$selection_api(path, list(per_page = 1L, page = last_page))$body
      } else newest$body
      if (!is.list(oldest) || !length(oldest)) return(NULL)
      first_commit <- private$selection_time_from_commit(oldest[[1L]])
      if (is.na(first_commit)) return(NULL)
      days <- as.numeric(difftime(last_commit, first_commit, units = "days"))
      list(first = first_commit, last = last_commit, months = max(0, days / 30.44))
    },
    activity_window = function(repository, settings) {
      path <- private$commits_path(repository)
      pre <- private$selection_api(path, list(until = settings$gdpr_date, per_page = 1L))$body
      post <- private$selection_api(path, list(since = settings$gdpr_date, per_page = 1L))$body
      list(pre = is.list(pre) && length(pre) > 0L, post = is.list(post) && length(post) > 0L)
    },
    is_public_repository = function(repository) {
      visibility <- private$values$scalar_text(repository$visibility, "")
      if (nzchar(visibility)) return(tolower(visibility) == "public")
      !private$values$scalar_bool(repository$private, TRUE)
    },
    candidate_passes_static_filters = function(candidate, approved_ids, settings) {
      repository <- candidate$repository
      created_at <- private$selection_time(repository$created_at)
      license <- private$values$or_else(repository$license, list())
      private$is_public_repository(repository) &&
        !private$values$scalar_bool(repository$fork, TRUE) &&
        !private$values$scalar_bool(repository$archived, TRUE) &&
        !is.na(created_at) && created_at < private$selection_time(settings$gdpr_date) &&
        private$values$scalar_int(repository$stargazers_count, 0L) >= settings$min_stars &&
        private$values$scalar_text(license$spdx_id, "") %in% approved_ids
    },
    evaluate_candidate = function(candidate, settings, issue_count) {
      full_name <- private$values$scalar_text(candidate$repository$full_name, "")
      activity <- private$commit_activity(full_name)
      if (is.null(activity) || activity$months < settings$min_activity_months) return(NULL)
      window <- private$activity_window(full_name, settings)
      if (!window$pre || !window$post) return(NULL)
      private$selection_record(candidate, issue_count, activity)
    },
    selection_record = function(candidate, issue_count, activity) {
      repository <- candidate$repository
      full_name <- private$values$scalar_text(repository$full_name, "")
      license <- private$values$or_else(repository$license, list())
      list(
        repository = full_name,
        url = private$values$scalar_text(repository$html_url, ""),
        name = private$values$scalar_text(repository$name, ""),
        owner = private$values$scalar_text(private$values$or_else(repository$owner, list())$login, ""),
        description = private$values$scalar_text(repository$description, ""),
        language = private$values$scalar_text(repository$language, ""),
        license_spdx_id = private$values$scalar_text(license$spdx_id, ""),
        license_name = private$values$scalar_text(license$name, ""),
        license_osi_approved = "true",
        stars = private$values$scalar_int(repository$stargazers_count, 0L),
        issues = private$values$scalar_int(issue_count, 0L),
        created_at = private$values$format_utc(private$selection_time(repository$created_at)),
        updated_at = private$values$format_utc(private$selection_time(repository$updated_at)),
        first_commit_at = private$values$format_utc(activity$first),
        last_commit_at = private$values$format_utc(activity$last),
        activity_months = round(activity$months, 2),
        ai_ml_match_topics = paste(candidate$topics, collapse = "; "),
        has_pre_gdpr_activity = "true",
        has_post_gdpr_activity = "true"
      )
    },
    records_to_selection_data_frame = function(records) {
      do.call(rbind, lapply(records, function(record) {
        as.data.frame(lapply(record, as.character), stringsAsFactors = FALSE, check.names = FALSE)
      }))
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

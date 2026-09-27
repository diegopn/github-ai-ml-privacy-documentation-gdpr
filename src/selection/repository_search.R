RepositorySearch <- R6::R6Class(
  "RepositorySearch",
  public = list(
    initialize = function(client, values) {
      required_methods <- c("api", "response_ok", "response_condition")
      if (!inherits(client, "R6") || !all(vapply(required_methods, function(method) {
        is.function(client[[method]])
      }, logical(1L)))) {
        stop("RepositorySearch exige um cliente GitHub R6.", call. = FALSE)
      }
      if (!inherits(values, "ValueTools")) stop("RepositorySearch exige ValueTools.", call. = FALSE)
      private$client <- client
      private$values <- values
      invisible(self)
    },
    find_candidates = function(settings) {
      candidates <- list()
      topic_audit <- list()

      for (topic_index in seq_along(settings$topics)) {
        topic <- settings$topics[[topic_index]]
        cat(sprintf("Busca %d/%d | tópico: %s\n", topic_index, length(settings$topics), topic))
        result <- private$search_topic(topic, settings)
        topic_audit[[length(topic_audit) + 1L]] <- result$audit
        for (repository in result$repositories) {
          full_name <- private$values$scalar_text(repository$full_name, "")
          if (is.null(candidates[[full_name]])) {
            candidates[[full_name]] <- list(repository = repository, topics = topic)
          } else {
            candidates[[full_name]]$topics <- unique(c(candidates[[full_name]]$topics, topic))
          }
        }
        cat(sprintf("  Resultado: %d repositórios encontrados.\n", length(result$repositories)))
      }

      list(
        candidates = unname(candidates),
        audit = list(
          protocol_version = settings$protocol_version,
          created_start = settings$created_start,
          created_end = settings$created_end,
          gdpr_date = settings$gdpr_date,
          topics = settings$topics,
          min_stars = settings$min_stars,
          min_issues = settings$min_issues,
          min_activity_months = settings$min_activity_months,
          api_search_interval_seconds = settings$search_interval_seconds,
          api_max_results_per_query = settings$max_results_per_query,
          topic_searches = topic_audit
        )
      )
    }
  ),
  private = list(
    client = NULL,
    values = NULL,
    search_api = function(params) {
      response <- private$client$api("/search/repositories", params)
      if (!isTRUE(private$client$response_ok(response))) {
        stop(private$client$response_condition(response))
      }
      response
    },
    repository_query = function(topic, start_date, end_date, settings) {
      paste(
        "is:public", paste0("topic:", topic), paste0("stars:>=", settings$min_stars),
        paste0("created:", start_date, "..", end_date), "fork:false", "archived:false"
      )
    },
    search_total_count = function(body, query) {
      value <- suppressWarnings(as.numeric(body$total_count))
      if (length(value) != 1L || !is.finite(value) || value < 0 || value != floor(value)) {
        stop(sprintf("A Search API não retornou total_count válido para: %s", query), call. = FALSE)
      }
      as.integer(value)
    },
    partition_topic_query = function(topic, start_date, end_date, settings) {
      start <- as.Date(start_date)
      end <- as.Date(end_date)
      if (is.na(start) || is.na(end) || end < start) {
        stop(sprintf("Janela temporal inválida para o tópico %s.", topic), call. = FALSE)
      }
      query <- private$repository_query(topic, format(start), format(end), settings)
      response <- private$search_api(list(q = query, per_page = 1L))
      body <- private$values$or_else(response$body, list())
      if (private$values$scalar_bool(body$incomplete_results, TRUE)) {
        stop(sprintf("A Search API devolveu resultados incompletos para: %s", query), call. = FALSE)
      }
      total <- private$search_total_count(body, query)
      if (total <= settings$max_results_per_query) {
        return(list(list(query = query, start = format(start), end = format(end), total_count = total)))
      }
      if (start >= end) {
        stop(sprintf("A consulta ainda excede %d resultados em um único dia: %s", settings$max_results_per_query, query))
      }
      midpoint <- start + floor(as.numeric(end - start) / 2)
      c(
        private$partition_topic_query(topic, format(start), format(midpoint), settings),
        private$partition_topic_query(topic, format(midpoint + 1L), format(end), settings)
      )
    },
    download_search_partition = function(partition, settings) {
      total <- partition$total_count
      pages <- if (total == 0L) 0L else ceiling(total / settings$page_size)
      repositories <- list()
      for (page in seq_len(pages)) {
        response <- private$search_api(list(
          q = partition$query,
          sort = "stars",
          order = "desc",
          per_page = settings$page_size,
          page = page
        ))
        body <- private$values$or_else(response$body, list())
        if (private$values$scalar_bool(body$incomplete_results, TRUE)) {
          stop(sprintf("A Search API devolveu resultados incompletos para: %s", partition$query), call. = FALSE)
        }
        items <- private$values$or_else(body$items, list())
        private$validate_repository_items(items, partition$query)
        if (page < pages && length(items) < settings$page_size) {
          stop(sprintf("A consulta mudou durante a paginação e ficou incompleta: %s", partition$query))
        }
        repositories <- c(repositories, items)
      }
      downloaded <- length(repositories)
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
    },
    validate_repository_items = function(items, query) {
      if (!is.list(items) || !all(vapply(items, is.list, logical(1L)))) {
        stop(sprintf("Itens inválidos na resposta da Search API: %s", query), call. = FALSE)
      }
      identities <- vapply(items, function(item) private$values$scalar_text(item$full_name, ""), character(1L))
      if (any(!nzchar(identities)) || anyDuplicated(tolower(identities))) {
        stop(sprintf("Repositórios ausentes ou duplicados durante a paginação: %s", query), call. = FALSE)
      }
      invisible(TRUE)
    },
    search_topic = function(topic, settings) {
      partitions <- private$partition_topic_query(topic, settings$created_start, settings$created_end, settings)
      repositories <- list()
      partition_audit <- vector("list", length(partitions))
      for (index in seq_along(partitions)) {
        result <- private$download_search_partition(partitions[[index]], settings)
        repositories <- c(repositories, result$repositories)
        partition_audit[[index]] <- result$audit
      }
      private$validate_repository_items(repositories, topic)
      list(
        repositories = repositories,
        audit = list(topic = topic, partitions = partition_audit, downloaded = length(repositories))
      )
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

SelectorContract <- R6::R6Class(
  "SelectorContract",
  public = list(
    run = function(context) {
      project <- context$new_test_project()
      topics <- as.character(unlist(project$config$get("selection", "topics"), use.names = FALSE))
      page_one <- c(
        list(
          context$make_repository("owner/project"),
          context$make_repository("owner/fork", fork = TRUE),
          context$make_repository("owner/archived", archived = TRUE),
          context$make_repository("owner/late", created = "2018-05-25T00:00:00Z"),
          context$make_repository("owner/low-stars", stars = 499L),
          context$make_repository("owner/unapproved-license", license = "LicenseRef-Unknown"),
          context$make_repository("owner/low-issues"),
          context$make_repository("owner/inactive")
        ),
        lapply(seq_len(92L), function(index) context$make_repository(paste0("owner/fork-", index), fork = TRUE))
      )
      page_two <- list(context$make_repository("owner/page-two", archived = TRUE))
      client <- MockSelectionClient$new(topics, page_one, page_two)
      classes <- context$classes()
      searcher <- classes$RepositorySearch$new(client, context$values())
      selector <- classes$SampleSelector$new(project$config, client, project$store, context$values(), searcher)
      selector$run()
      sample <- project$store$assert_published_sample()
      audit <- jsonlite::fromJSON(project$store$audit_path(), simplifyVector = FALSE)
      context$check("seletor pagina, deduplica tópicos e mantém ordem e tópicos coincidentes",
        nrow(sample) == 1L && sample$repository[[1L]] == "owner/project" &&
          sample$ai_ml_match_topics[[1L]] == paste(topics[1:2], collapse = "; ") &&
          audit$unique_candidates == 101L && audit$selected_repositories == 1L)
      context$check("seletor aplica licença, estrelas, issues, atividade e filtros de repositório",
        sample$license_spdx_id[[1L]] == "MIT" && sample$stars[[1L]] == "500" &&
          sample$issues[[1L]] == "101" && sample$has_pre_gdpr_activity[[1L]] == "true")
      limited_client <- MockSelectionClient$new(topics, page_one, page_two, rate_limited = TRUE)
      limited_searcher <- classes$RepositorySearch$new(limited_client, context$values())
      limited_selector <- classes$SampleSelector$new(
        project$config, limited_client, project$store, context$values(), limited_searcher
      )
      limit_error <- tryCatch(limited_selector$run(), error = identity)
      context$check("GraphQL HTTP 200 com rate limit interrompe seleção e preserva amostra publicada",
        inherits(limit_error, "github_rate_limit_error") &&
          identical(project$store$assert_published_sample()$repository[[1L]], "owner/project"))
      malformed_client <- MockSelectionClient$new(topics, page_one, page_two, malformed_graphql = TRUE)
      malformed_searcher <- classes$RepositorySearch$new(malformed_client, context$values())
      malformed_selector <- classes$SampleSelector$new(
        project$config, malformed_client, project$store, context$values(), malformed_searcher
      )
      malformed_error <- tryCatch(malformed_selector$run(), error = identity)
      context$check("resposta GraphQL que omite aliases falha sem publicar uma contagem zero",
        inherits(malformed_error, "github_api_error") &&
          identical(project$store$assert_published_sample()$repository[[1L]], "owner/project"))
      duplicate_page <- page_one
      duplicate_page[[length(duplicate_page)]] <- page_one[[1L]]
      duplicate_client <- MockSelectionClient$new(topics, duplicate_page, page_two)
      duplicate_search <- classes$RepositorySearch$new(duplicate_client, context$values())
      context$check("paginação rejeita contagens preenchidas com repositórios duplicados",
        context$errors(duplicate_search$find_candidates(project$config$selection_settings())))
      invisible(TRUE)
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

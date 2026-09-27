CollectorContract <- R6::R6Class(
  "CollectorContract",
  public = list(
    run = function(context) {
      project <- context$new_test_project()
      sample <- data.frame(
        repository = "owner/project", url = "https://github.com/owner/project", name = "project", owner = "owner",
        description = "fixture", language = "R", license_spdx_id = "MIT", license_name = "MIT License",
        license_osi_approved = "true", stars = "501", issues = "110", created_at = "2010-01-01T00:00:00Z",
        first_commit_at = "2010-01-01T00:00:00Z", last_commit_at = "2024-01-01T00:00:00Z",
        activity_months = "168", ai_ml_match_topics = "machine-learning",
        has_pre_gdpr_activity = "true", has_post_gdpr_activity = "true", stringsAsFactors = FALSE
      )
      project$store$publish_sample(sample, list(protocol_version = "offline-test"))
      client <- MockCollectionClient$new()
      collector <- context$classes()$RepositoryCollector$new(
        project$config, client, project$store, context$services()$classifier,
        context$services()$document_catalog, context$values(), context$protocol()
      )
      collector$run()
      records <- project$store$read_jsonl()
      record <- records[[1L]]
      context$check("coletor reúne os dois cortes com serviço GitHub R6 injetado",
        all(length(records) == 1L, record$pre$commit$sha == "pre-sha", record$post$commit$sha == "post-sha",
          client$raw_call_count() == 1L))
      context$check("checkpoint mantém esquema, protocolo e validação de pares completos",
        all(identical(names(record), c("repository", "input_row", "source_sha256", "collector_protocol_version", "observed_at",
          "accessibility", "pre", "post")),
          identical(names(record$pre), c("until", "commit_request_status", "commit_rate", "commit", "tree_request_status",
            "tree_rate", "tree_truncated", "document_candidates", "documents", "classification_rule_version", "classification")),
          length(project$store$read_complete_checkpoint(project$store$assert_published_sample(),
            source_hash = project$store$hash_file(project$store$sample_path()))) == 1L,
          record$collector_protocol_version == context$protocol()$version()))
      original_hash <- project$store$hash_file(project$store$raw_path())
      for (mode in c("malformed_tree", "failed_download")) {
        failing_client <- MockCollectionClient$new(mode)
        failing_collector <- context$classes()$RepositoryCollector$new(project$config, failing_client,
          project$store, context$services()$classifier, context$services()$document_catalog, context$values(), context$protocol())
        context$check(paste("coleta preserva checkpoint anterior quando ocorre", mode),
          all(context$errors(failing_collector$run()), identical(original_hash, project$store$hash_file(project$store$raw_path()))))
        if (mode == "failed_download") context$check("falhas de download não são reutilizadas entre snapshots",
          failing_client$raw_call_count() == 2L)
      }
      invisible(TRUE)
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

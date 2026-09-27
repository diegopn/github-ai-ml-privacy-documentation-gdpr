DatasetBuilder <- R6::R6Class(
  "DatasetBuilder",
  public = list(
    initialize = function(config, classifier, document_catalog, values) {
      if (!inherits(config, "ProjectConfig")) stop("DatasetBuilder exige ProjectConfig.", call. = FALSE)
      if (!inherits(classifier, "PrivacyDocumentClassifier")) stop("DatasetBuilder exige PrivacyDocumentClassifier.", call. = FALSE)
      if (!inherits(document_catalog, "PrivacyDocumentCatalog")) stop("DatasetBuilder exige PrivacyDocumentCatalog.", call. = FALSE)
      if (!inherits(values, "ValueTools")) stop("DatasetBuilder exige ValueTools.", call. = FALSE)
      private$config <- config
      private$classifier <- classifier
      private$document_catalog <- document_catalog
      private$values <- values
      invisible(self)
    },
    build = function(sample, records, source_hash) {
      records_by_repository <- list()
      for (record in records) {
        repository <- private$values$scalar_text(record$repository, "")
        if (nzchar(repository)) records_by_repository[[repository]] <- record
      }
      rows <- lapply(seq_len(nrow(sample)), function(index) {
        row <- sample[index, , drop = FALSE]
        repository <- private$values$scalar_text(row$repository, "")
        record <- private$values$or_else(records_by_repository[[repository]], list(
          repository = repository,
          input_row = as.list(row),
          accessibility = list(http_status = 0L, accessible = FALSE, metadata = list(), error = "registro não processado"),
          pre = list(),
          post = list()
        ))
        private$flatten_record(row, record, index + 1L, source_hash)
      })
      columns <- private$dataset_columns()
      data <- lapply(columns, function(column) {
        vapply(rows, function(row) {
          value <- row[[column]]
          if (is.null(value) || !length(value)) "" else as.character(value[[1L]])
        }, character(1L))
      })
      names(data) <- columns
      list(rows = rows, data = as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE))
    },
    reclassify = function(sample, records, source_hash) {
      records <- lapply(records, function(record) {
        for (period in c("pre", "post")) {
          if (!is.null(record[[period]])) record[[period]]$classification <- NULL
        }
        record
      })
      self$build(sample, records, source_hash)

    }
  ),
  private = list(
    config = NULL,
    classifier = NULL,
    document_catalog = NULL,
    values = NULL,
    dataset_columns = function() {
      c(
        "repository", "url", "name", "owner", "description", "language", "input_license_spdx_id", "input_license_name",
        "input_license_osi_approved", "stars", "issues", "created_at", "input_first_commit_at", "input_last_commit_at",
        "input_activity_months", "input_ai_ml_match_topics", "input_row_number", "sample_source_sha256", "accessibility_http_status",
        "accessible_at_run", "current_visibility", "current_private", "current_fork", "current_archived", "current_stars",
        "current_open_issues", "current_created_at", "current_updated_at", "current_license_spdx_id", "current_license_name",
        "current_metadata_exception", "pre_cutoff", "pre_commit_sha", "pre_commit_date", "pre_tree_sha", "pre_tree_truncated",
        "pre_document_paths", "pre_D1", "pre_C1", "pre_C2", "pre_C3", "pre_C4", "pre_C5", "pre_C6", "pre_C7", "pre_score",
        "pre_D1_evidence", "pre_C1_evidence", "pre_C2_evidence", "pre_C3_evidence", "pre_C4_evidence", "pre_C5_evidence",
        "pre_C6_evidence", "pre_C7_evidence", "pre_classification_note", "pre_version_error", "post_cutoff", "post_commit_sha",
        "post_commit_date", "post_tree_sha", "post_tree_truncated", "post_document_paths", "post_D1", "post_C1", "post_C2",
        "post_C3", "post_C4", "post_C5", "post_C6", "post_C7", "post_score", "post_D1_evidence", "post_C1_evidence",
        "post_C2_evidence", "post_C3_evidence", "post_C4_evidence", "post_C5_evidence", "post_C6_evidence", "post_C7_evidence",
        "post_classification_note", "post_version_error", "observation"
      )
    },
    flatten_record = function(row, record, row_number, source_hash) {
      output <- private$flatten_input_row(row, row_number, source_hash)
      accessibility <- private$flatten_accessibility(record$accessibility)
      output <- c(output, accessibility$fields)
      observations <- accessibility$observations
      for (period in c("pre", "post")) {
        version <- private$values$or_else(record[[period]], list())
        flattened <- private$flatten_version(period, version, output$repository)
        output <- c(output, flattened$fields)
        observations <- c(observations, flattened$observations)
      }
      output$observation <- if (length(observations)) paste(observations, collapse = " | ") else "par completo processado"
      output[private$dataset_columns()]
    },
    flatten_input_row = function(row, row_number, source_hash) {
      fields <- c(
        repository = "repository", url = "url", name = "name", owner = "owner", description = "description", language = "language",
        input_license_spdx_id = "license_spdx_id", input_license_name = "license_name",
        input_license_osi_approved = "license_osi_approved", stars = "stars", issues = "issues", created_at = "created_at",
        input_first_commit_at = "first_commit_at", input_last_commit_at = "last_commit_at",
        input_activity_months = "activity_months", input_ai_ml_match_topics = "ai_ml_match_topics"
      )
      result <- lapply(fields, function(field) private$values$scalar_text(row[[field]], ""))
      result$input_row_number <- row_number
      result$sample_source_sha256 <- source_hash
      result
    },
    flatten_accessibility = function(accessibility) {
      accessibility <- private$values$or_else(accessibility, list())
      metadata <- private$values$or_else(accessibility$metadata, list())
      fields <- list(
        accessibility_http_status = private$values$scalar_text(accessibility$http_status, ""),
        accessible_at_run = private$values$scalar_bool(accessibility$accessible, FALSE),
        current_visibility = private$values$scalar_text(metadata$visibility, ""),
        current_private = if (is.null(metadata$private)) "" else private$values$scalar_bool(metadata$private, FALSE),
        current_fork = if (is.null(metadata$fork)) "" else private$values$scalar_bool(metadata$fork, FALSE),
        current_archived = if (is.null(metadata$archived)) "" else private$values$scalar_bool(metadata$archived, FALSE),
        current_stars = private$values$scalar_text(metadata$stargazers_count, ""),
        current_open_issues = private$values$scalar_text(metadata$open_issues_count, ""),
        current_created_at = private$values$scalar_text(metadata$created_at, ""),
        current_updated_at = private$values$scalar_text(metadata$updated_at, ""),
        current_license_spdx_id = private$values$scalar_text(metadata$license_spdx_id, ""),
        current_license_name = private$values$scalar_text(metadata$license_name, ""),
        current_metadata_exception = private$values$scalar_text(accessibility$error, "")
      )
      observations <- if (private$values$scalar_int(accessibility$http_status, 0L) != 200L) {
        "repositório não acessível na validação"
      } else character()
      list(fields = fields, observations = observations)
    },
    flatten_version = function(period, version, repository) {
      commit <- private$values$or_else(version$commit, list())
      documents <- private$document_catalog$relevant_documents(private$values$or_else(version$documents, list()))
      classification <- private$version_classification(version, documents, repository)
      prefix <- paste0(period, "_")
      fields <- list(
        cutoff = private$values$scalar_text(version$until, private$config$analysis_cutoffs()[[period]]),
        commit_sha = private$values$scalar_text(commit$sha, ""),
        commit_date = private$values$scalar_text(commit$date, ""),
        tree_sha = private$values$scalar_text(commit$tree_sha, ""),
        tree_truncated = private$values$scalar_bool(version$tree_truncated, TRUE),
        document_paths = paste(vapply(documents, function(document) private$values$scalar_text(document$path, ""), character(1L)), collapse = "; "),
        D1 = classification$D1
      )
      for (code in private$classifier$criterion_codes()) {
        fields[[code]] <- classification[[code]]
        fields[[paste0(code, "_evidence")]] <- private$values$scalar_text(classification$evidence[[code]], "")
      }
      fields$score <- classification$score
      fields$D1_evidence <- classification$D1_evidence
      fields$classification_note <- classification$note
      fields$version_error <- private$values$scalar_text(version$error, "")
      names(fields) <- paste0(prefix, names(fields))
      list(fields = fields, observations = private$version_observations(period, version))
    },
    version_classification = function(version, documents, repository) {
      cached_version <- private$values$scalar_text(version$classification_rule_version, "")
      if (private$classification_cache_is_usable(version$classification, cached_version)) {
        return(version$classification)
      }
      commit <- private$values$or_else(version$commit, list())
      private$classifier$classify(documents, private$values$scalar_text(commit$sha, ""), repository)
    },
    version_observations = function(period, version) {
      observations <- character()
      if (nzchar(private$values$scalar_text(version$error, ""))) {
        observations <- c(observations, paste0(period, ": ", private$values$scalar_text(version$error, "")))
      }
      if (isTRUE(private$values$scalar_bool(version$tree_truncated, TRUE))) {
        observations <- c(observations, paste0(period, ": árvore Git truncada pela API"))
      }
      if (!length(private$values$or_else(version$documents, list()))) {
        observations <- c(observations, paste0(period, ": nenhum documento candidato recuperado"))
      }
      observations
    },
    classification_cache_is_usable = function(classification, rule_version) {
      if (!is.list(classification) || !identical(rule_version, private$classifier$rule_version())) return(FALSE)
      criteria <- private$classifier$criterion_codes()
      required <- c("D1", criteria, "score", "D1_evidence", "note", "evidence")
      if (anyDuplicated(names(classification)) || !all(required %in% names(classification))) return(FALSE)
      evidence <- classification$evidence
      if (!is.list(evidence) || anyDuplicated(names(evidence))) return(FALSE)
      text_fields <- c(classification[c("D1_evidence", "note")], evidence)
      if (!all(vapply(text_fields, is.character, logical(1L))) ||
        any(lengths(text_fields) != 1L) || anyNA(unlist(text_fields))) return(FALSE)
      binary_values <- vapply(classification[c("D1", criteria)], private$cache_number, numeric(1L))
      score <- private$cache_number(classification$score)
      if (!all(c(
        all(is.finite(binary_values)), all(binary_values %in% c(0, 1)), is.finite(score)
      ))) return(FALSE)
      criterion_values <- binary_values[criteria] == 1
      criterion_evidence <- nzchar(vapply(
        evidence[criteria], private$values$scalar_text, character(1L), default = ""
      ))
      all(c(
        score == sum(criterion_values), score == 0 | binary_values[[1L]] == 1,
        all(criterion_values == criterion_evidence),
        binary_values[[1L]] == nzchar(private$values$scalar_text(classification$D1_evidence, ""))
      ))
    },
    cache_number = function(value) {
      if (!is.numeric(value) && !is.character(value)) return(NA_real_)
      parsed <- suppressWarnings(as.numeric(value))
      if (length(parsed) == 1L && is.finite(parsed)) parsed else NA_real_
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

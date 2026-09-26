dataset_columns <- function() {
  c(
    "repository", "url", "name", "owner", "description", "language", "input_license_spdx_id", "input_license_name", "input_license_osi_approved", "stars", "issues", "created_at",
    "input_first_commit_at", "input_last_commit_at", "input_activity_months", "input_ai_ml_match_topics", "input_row_number", "sample_source_sha256",
    "accessibility_http_status", "accessible_at_run", "current_visibility", "current_private", "current_fork", "current_archived", "current_stars", "current_open_issues", "current_created_at", "current_updated_at", "current_license_spdx_id", "current_license_name", "current_metadata_exception",
    "pre_cutoff", "pre_commit_sha", "pre_commit_date", "pre_tree_sha", "pre_tree_truncated", "pre_document_paths", "pre_D1", "pre_C1", "pre_C2", "pre_C3", "pre_C4", "pre_C5", "pre_C6", "pre_C7", "pre_score", "pre_D1_evidence", "pre_C1_evidence", "pre_C2_evidence", "pre_C3_evidence", "pre_C4_evidence", "pre_C5_evidence", "pre_C6_evidence", "pre_C7_evidence", "pre_classification_note", "pre_version_error",
    "post_cutoff", "post_commit_sha", "post_commit_date", "post_tree_sha", "post_tree_truncated", "post_document_paths", "post_D1", "post_C1", "post_C2", "post_C3", "post_C4", "post_C5", "post_C6", "post_C7", "post_score", "post_D1_evidence", "post_C1_evidence", "post_C2_evidence", "post_C3_evidence", "post_C4_evidence", "post_C5_evidence", "post_C6_evidence", "post_C7_evidence", "post_classification_note", "post_version_error", "observation"
  )
}

flatten_record <- function(row, record, row_number, source_hash, classifier = NULL, config = PROJECT_CONFIG) {
  input_fields <- c(
    repository = "repository", url = "url", name = "name", owner = "owner",
    description = "description", language = "language",
    input_license_spdx_id = "license_spdx_id", input_license_name = "license_name",
    input_license_osi_approved = "license_osi_approved", stars = "stars", issues = "issues",
    created_at = "created_at", input_first_commit_at = "first_commit_at",
    input_last_commit_at = "last_commit_at", input_activity_months = "activity_months",
    input_ai_ml_match_topics = "ai_ml_match_topics"
  )
  output <- lapply(input_fields, function(field) scalar_text(row[[field]], ""))
  output$input_row_number <- row_number
  output$sample_source_sha256 <- source_hash

  accessibility <- record$accessibility %||% list()
  metadata <- accessibility$metadata %||% list()
  output$accessibility_http_status <- scalar_text(accessibility$http_status, "")
  output$accessible_at_run <- scalar_bool(accessibility$accessible, FALSE)
  output$current_visibility <- scalar_text(metadata$visibility, "")
  output$current_private <- if (is.null(metadata$private)) "" else scalar_bool(metadata$private, FALSE)
  output$current_fork <- if (is.null(metadata$fork)) "" else scalar_bool(metadata$fork, FALSE)
  output$current_archived <- if (is.null(metadata$archived)) "" else scalar_bool(metadata$archived, FALSE)
  output$current_stars <- scalar_text(metadata$stargazers_count, "")
  output$current_open_issues <- scalar_text(metadata$open_issues_count, "")
  output$current_created_at <- scalar_text(metadata$created_at, "")
  output$current_updated_at <- scalar_text(metadata$updated_at, "")
  output$current_license_spdx_id <- scalar_text(metadata$license_spdx_id, "")
  output$current_license_name <- scalar_text(metadata$license_name, "")
  output$current_metadata_exception <- scalar_text(accessibility$error, "")

  observations <- character()
  if (scalar_int(accessibility$http_status, 0L) != 200L) observations <- c(observations, "repositório não acessível na validação")
  for (label in c("pre", "post")) {
    version <- record[[label]] %||% list()
    commit <- version$commit %||% list()
    documents <- relevant_documents(version$documents %||% list())
    cached_classification <- version$classification %||% NULL
    classification_version <- scalar_text(version$classification_rule_version, "")
    # Cache sem versão explícita é compatível; versões diferentes são recalculadas.
    classification <- if (is.list(cached_classification) &&
                          (identical(classification_version, RULE_VERSION) || !nzchar(classification_version))) {
      cached_classification
    } else {
      if (is.null(classifier)) classify_documents(documents, scalar_text(commit$sha, ""), output$repository) else classifier$classify(documents, scalar_text(commit$sha, ""), output$repository)
    }
    prefix <- paste0(label, "_")
    output[[paste0(prefix, "cutoff")]] <- if (label == "pre") config$get("analysis", "pre_until", PRE_UNTIL) else config$get("analysis", "post_until", POST_UNTIL)
    output[[paste0(prefix, "commit_sha")]] <- scalar_text(commit$sha, "")
    output[[paste0(prefix, "commit_date")]] <- scalar_text(commit$date, "")
    output[[paste0(prefix, "tree_sha")]] <- scalar_text(commit$tree_sha, "")
    output[[paste0(prefix, "tree_truncated")]] <- scalar_bool(version$tree_truncated, FALSE)
    output[[paste0(prefix, "document_paths")]] <- paste(vapply(documents, function(document) scalar_text(document$path, ""), character(1L)), collapse = "; ")
    output[[paste0(prefix, "D1")]] <- classification$D1
    for (code in names(RULES)) {
      output[[paste0(prefix, code)]] <- classification[[code]]
      output[[paste0(prefix, code, "_evidence")]] <- scalar_text(classification$evidence[[code]], "")
    }
    output[[paste0(prefix, "score")]] <- classification$score
    output[[paste0(prefix, "D1_evidence")]] <- classification$D1_evidence
    output[[paste0(prefix, "classification_note")]] <- classification$note
    output[[paste0(prefix, "version_error")]] <- scalar_text(version$error, "")
    if (nzchar(output[[paste0(prefix, "version_error")]])) observations <- c(observations, paste0(label, ": ", output[[paste0(prefix, "version_error")]]))
    if (scalar_bool(version$tree_truncated, FALSE)) observations <- c(observations, paste0(label, ": árvore Git truncada pela API"))
    if (!length(version$documents %||% list())) observations <- c(observations, paste0(label, ": nenhum documento candidato recuperado"))
  }
  output$observation <- if (length(observations)) paste(observations, collapse = " | ") else "par completo processado"
  output[dataset_columns()]
}

is_complete_record <- function(record) {
  pre <- record$pre %||% list()
  post <- record$post %||% list()
  nzchar(scalar_text((pre$commit %||% list())$sha, "")) &&
    nzchar(scalar_text((post$commit %||% list())$sha, "")) &&
    scalar_int(pre$tree_request_status, 0L) == 200L && scalar_int(post$tree_request_status, 0L) == 200L
}



DatasetBuilder <- R6::R6Class(
  "DatasetBuilder",
  public = list(
    config = NULL,
    classifier = NULL,
    initialize = function(config, classifier) {
      self$config <- config
      self$classifier <- classifier
    },
    build = function(sample, records, source_hash) {
      records_by_repository <- index_records(records)
      rows <- lapply(seq_len(nrow(sample)), function(index) {
        row <- sample[index, , drop = FALSE]
        repository <- scalar_text(row$repository, "")
        record <- records_by_repository[[repository]] %||% list(
          repository = repository, input_row = as.list(row),
          accessibility = list(http_status = 0L, accessible = FALSE, metadata = list(), error = "registro não processado"),
          pre = list(), post = list()
        )
        flatten_record(row, record, index + 1L, source_hash, self$classifier, self$config)
      })
      columns <- dataset_columns()
      values <- lapply(columns, function(column) vapply(rows, function(row) {
        value <- row[[column]]
        if (is.null(value) || !length(value)) "" else as.character(value[[1L]])
      }, character(1L)))
      names(values) <- columns
      list(rows = rows, data = as.data.frame(values, stringsAsFactors = FALSE, check.names = FALSE))
    }
  )
)

OfflineExperimentFixture <- R6::R6Class(
  "OfflineExperimentFixture",
  public = list(
    initialize = function(context) {
      private$context <- context
      invisible(self)
    },
    build = function() {
      classes <- private$context$classes()
      services <- private$context$services()
      repository_names <- paste0("owner/repository-", 1:5)
      sample <- private$sample_data(repository_names)
      dataset <- private$dataset_data(repository_names)
      records <- private$collection_records(repository_names)
      input_path <- services$store$sample_path()
      classifier <- classes$PrivacyDocumentClassifier$new(services$rule_set, services$values, services$document_catalog)
      statistics <- classes$PairedStatistics$new(
        private$context$config(), services$values, services$protocol
      )
      stats <- statistics$calculate(records, dataset, sample, input_path,
        classifier$criterion_codes(), classifier$rule_version())
      list(
        sample = sample,
        records = records,
        dataset = dataset,
        stats = stats,
        source_hash = "fixture-hash",
        input_path = input_path,
        raw_path = services$store$raw_path()
      )
    }
  ),
  private = list(
    context = NULL,
    sample_data = function(repository_names) {
      sample <- data.frame(
        repository = repository_names,
        url = paste0("https://github.com/", repository_names),
        name = paste0("repository-", 1:5),
        owner = "owner",
        description = "offline fixture",
        language = "R",
        license_spdx_id = "MIT",
        license_name = "MIT License",
        license_osi_approved = "true",
        stars = "500",
        issues = "100",
        created_at = "2010-01-01T00:00:00Z",
        first_commit_at = "2010-01-01T00:00:00Z",
        last_commit_at = "2024-01-01T00:00:00Z",
        activity_months = "168",
        ai_ml_match_topics = "machine-learning",
        stringsAsFactors = FALSE
      )
      sample
    },
    dataset_data = function(repository_names) {
      pre_d1 <- c("1", "0", "0", "0", "0")
      post_d1 <- c("0", "1", "1", "1", "0")
      pre_score <- c("1", "0", "0", "0", "0")
      post_score <- c("0", "1", "2", "0", "0")
      dataset <- data.frame(
        repository = repository_names,
        pre_D1 = pre_d1,
        post_D1 = post_d1,
        pre_score = pre_score,
        post_score = post_score,
        pre_D1_evidence = ifelse(pre_d1 == "1", "fixture pre evidence", ""),
        post_D1_evidence = ifelse(post_d1 == "1", "fixture post evidence", ""),
        sample_source_sha256 = rep("fixture-hash", length(repository_names)),
        stringsAsFactors = FALSE
      )
      for (code in paste0("C", 1:7)) {
        dataset[[paste0("pre_", code)]] <- "0"
        dataset[[paste0("post_", code)]] <- "0"
        dataset[[paste0("pre_", code, "_evidence")]] <- ""
        dataset[[paste0("post_", code, "_evidence")]] <- ""
      }
      dataset$pre_C1 <- c("0", "0", "0", "0", "0")
      dataset$post_C1 <- c("0", "0", "1", "0", "0")
      dataset$pre_C2 <- c("1", "0", "0", "0", "0")
      dataset$post_C2 <- c("0", "1", "1", "0", "0")
      dataset$pre_C1_evidence <- ifelse(dataset$pre_C1 == "1", "fixture C1 pre", "")
      dataset$post_C1_evidence <- ifelse(dataset$post_C1 == "1", "fixture C1 post", "")
      dataset$pre_C2_evidence <- ifelse(dataset$pre_C2 == "1", "fixture C2 pre", "")
      dataset$post_C2_evidence <- ifelse(dataset$post_C2 == "1", "fixture C2 post", "")
      dataset
    },
    collection_records = function(repository_names) {
      records <- lapply(repository_names, function(repository) list(
        repository = repository,
        collector_protocol_version = private$context$protocol()$version(),
        pre = private$context$complete_period(paste0(repository, "-pre")),
        post = private$context$complete_period(paste0(repository, "-post"), "post")
      ))
      records
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

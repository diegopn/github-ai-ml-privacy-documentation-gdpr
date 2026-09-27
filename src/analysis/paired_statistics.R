PairedStatistics <- R6::R6Class(
  "PairedStatistics",
  public = list(
    initialize = function(config, values, protocol) {
      if (!inherits(config, "ProjectConfig")) stop("PairedStatistics exige ProjectConfig.", call. = FALSE)
      if (!inherits(values, "ValueTools")) stop("PairedStatistics exige ValueTools.", call. = FALSE)
      if (!inherits(protocol, "CollectionProtocol")) stop("PairedStatistics exige CollectionProtocol.", call. = FALSE)
      private$config <- config
      private$values <- values
      private$protocol <- protocol
      invisible(self)
    },
    calculate = function(records, dataset, sample, input_path, criteria, rule_version) {
      private$validate_inputs(dataset, sample, criteria)
      collected <- private$collected_pair_rows(records, sample)
      classified <- private$classified_pair_rows(dataset, criteria)
      complete <- collected & classified
      indices <- which(complete)
      pre_d1 <- suppressWarnings(as.numeric(dataset$pre_D1[indices]))
      post_d1 <- suppressWarnings(as.numeric(dataset$post_D1[indices]))
      pre_score <- suppressWarnings(as.numeric(dataset$pre_score[indices]))
      post_score <- suppressWarnings(as.numeric(dataset$post_score[indices]))
      rq1 <- private$calculate_d1_statistics(pre_d1, post_d1)
      rq2 <- private$calculate_score_statistics(pre_score, post_score)
      pre_frequency <- private$criterion_frequency(dataset, indices, "pre", criteria)
      post_frequency <- private$criterion_frequency(dataset, indices, "post", criteria)
      c(list(
        analysis_population = paste(
          "pares com duas versões históricas recuperadas e árvores Git completas, não truncadas"
        ),
        total_input_rows = nrow(sample),
        complete_pairs = length(indices),
        incomplete_pairs = nrow(sample) - length(indices),
        incomplete_repositories = as.character(sample$repository[!complete]),
        rq1_mcnemar = rq1,
        rq2_wilcoxon = rq2,
        criterion_frequencies = list(pre = as.list(pre_frequency), post = as.list(post_frequency))
      ), private$analysis_metadata(records, dataset, sample, input_path, rule_version))
    }
  ),
  private = list(
    config = NULL,
    values = NULL,
    protocol = NULL,
    validate_inputs = function(dataset, sample, criteria) {
      if (!identical(as.character(criteria), paste0("C", 1:7))) {
        stop("A análise exige os critérios C1 a C7, nesta ordem.", call. = FALSE)
      }
      private$validate_sample(sample)
      private$validate_dataset(dataset, sample, criteria)
    },
    validate_sample = function(sample) {
      if (!is.data.frame(sample) || !"repository" %in% names(sample)) {
        stop("A amostra precisa conter a coluna repository.", call. = FALSE)
      }
      repositories <- as.character(sample$repository)
      repositories_are_valid <- !anyNA(repositories) &&
        all(nzchar(repositories) & trimws(repositories) == repositories) &&
        anyDuplicated(repositories) == 0L
      if (!repositories_are_valid) {
        stop("A amostra precisa conter identificadores de repositório únicos e sem espaços externos.", call. = FALSE)
      }
    },
    validate_dataset = function(dataset, sample, criteria) {
      if (!is.data.frame(dataset) || nrow(dataset) != nrow(sample)) {
        stop("O conjunto de dados precisa ter uma linha para cada repositório da amostra.", call. = FALSE)
      }
      if (!"repository" %in% names(dataset) ||
        !identical(as.character(dataset$repository), as.character(sample$repository))) {
        stop("As linhas do conjunto de dados precisam corresponder, na mesma ordem, à amostra.", call. = FALSE)
      }
      private$validate_dataset_columns(dataset, criteria)
      hashes <- as.character(dataset$sample_source_sha256)
      source_hash_is_valid <- length(unique(hashes)) == 1L &&
        all(!is.na(hashes) & nzchar(hashes))
      if (nrow(dataset) && !source_hash_is_valid) {
        stop("O dataset precisa ter um único hash de origem não vazio.", call. = FALSE)
      }
    },
    analysis_metadata = function(records, dataset, sample, input_path, rule_version) {
      collector_versions <- sort(unique(vapply(records, function(record) {
        private$values$scalar_text(record$collector_protocol_version, "unknown")
      }, character(1L))))

      source_hashes <- unique(as.character(dataset$sample_source_sha256))
      source_hashes <- source_hashes[!is.na(source_hashes)]

      list(
        generated_at = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
        source_csv = private$config$relative(input_path),
        significance_level = as.numeric(private$config$get("analysis", "alpha", 0.05)),
        source_sha256 = if (length(source_hashes)) source_hashes[[1L]] else "",
        pre_until = private$config$analysis_cutoffs()[["pre"]],
        post_until = private$config$analysis_cutoffs()[["post"]],
        classification_rule_version = rule_version,
        collector_protocol_version = private$protocol$version(),
        source_collector_protocol_versions = collector_versions,
        selection_topics = as.character(unlist(private$config$get("selection", "topics", character()), use.names = FALSE)),
        selection_min_stars = as.integer(private$config$get("selection", "min_stars", 500L)),
        selection_min_issues = as.integer(private$config$get("selection", "min_issues", 100L)),
        selection_min_activity_months = as.numeric(private$config$get("selection", "min_activity_months", 24)),
        sample_requires_osi_approved_license = TRUE,
        open_source_repositories = nrow(sample)
      )
    },
    validate_dataset_columns = function(dataset, criteria) {
      required <- c(
        "pre_D1", "post_D1", "pre_score", "post_score", "sample_source_sha256",
        paste0("pre_", criteria), paste0("post_", criteria)
      )
      missing <- setdiff(required, names(dataset))
      if (length(missing)) {
        stop(sprintf("Colunas ausentes para análise estatística: %s", paste(missing, collapse = ", ")), call. = FALSE)
      }
      invisible(TRUE)
    },
    collected_pair_rows = function(records, sample) {
      records_by_repository <- list()
      for (record in records) {
        repository <- private$values$scalar_text(record$repository, "")
        records_by_repository[repository] <- list(record)
      }
      vapply(as.character(sample$repository), function(repository) {
        repository <- private$values$scalar_text(repository, "")
        nzchar(repository) && private$protocol$record_analyzable(records_by_repository[[repository]])
      }, logical(1L))
    },
    classified_pair_rows = function(dataset, criteria) {
      valid <- rep(TRUE, nrow(dataset))
      for (period in c("pre", "post")) {
        columns <- paste0(period, "_", c("D1", criteria))
        binary <- as.matrix(as.data.frame(lapply(dataset[columns], function(column) suppressWarnings(as.numeric(column)))))
        score <- suppressWarnings(as.numeric(dataset[[paste0(period, "_score")]]))
        totals <- rowSums(binary[, -1L, drop = FALSE])
        valid <- valid & rowSums(!is.finite(binary) | !(binary %in% c(0, 1))) == 0L &
          is.finite(score) & score == totals & (totals == 0 | binary[, 1L] == 1)
      }
      valid
    },
    percentile_value = function(values, probability) {
      values <- as.numeric(values)
      values <- values[is.finite(values)]
      if (!length(values)) return(NA_real_)
      as.numeric(stats::quantile(values, probability, type = 7, names = FALSE))
    },
    exact_mcnemar = function(b, c) {
      n <- b + c
      if (!n) return(1)
      stats::binom.test(b, n, p = 0.5, alternative = "two.sided")$p.value
    },
    wilcoxon_exact = function(differences) {
      values <- differences[differences != 0]
      n <- length(values)
      if (!n) return(list(n_nonzero = 0L, w_plus = 0, w_minus = 0, p_two_sided_exact = 1, rank_biserial = 0))
      test <- stats::wilcox.test(values, alternative = "two.sided", exact = TRUE)
      w_plus <- unname(test$statistic)
      rank_total <- n * (n + 1) / 2
      w_minus <- rank_total - w_plus
      list(
        n_nonzero = n,
        w_plus = w_plus,
        w_minus = w_minus,
        p_two_sided_exact = test$p.value,
        rank_biserial = (w_plus - w_minus) / (w_plus + w_minus)
      )
    },
    bootstrap_median_ci = function(values) {
      values <- as.numeric(values)
      if (!length(values)) return(c(NA_real_, NA_real_))
      old_kind <- RNGkind()
      had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
      on.exit({
        do.call(RNGkind, list(kind = old_kind[[1L]], normal.kind = old_kind[[2L]], sample.kind = old_kind[[3L]]))
        if (had_seed) {
          assign(".Random.seed", old_seed, envir = .GlobalEnv)
        } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
          rm(".Random.seed", envir = .GlobalEnv)
        }
      }, add = TRUE)
      set.seed(20260908L)
      medians <- vapply(seq_len(10000L), function(...) {
        sampled <- values[sample.int(length(values), length(values), replace = TRUE)]
        stats::median(sampled)
      }, numeric(1L))
      c(private$percentile_value(medians, 0.025), private$percentile_value(medians, 0.975))
    },
    calculate_d1_statistics = function(pre_d1, post_d1) {
      losses <- sum(pre_d1 == 1 & post_d1 == 0, na.rm = TRUE)
      gains <- sum(pre_d1 == 0 & post_d1 == 1, na.rm = TRUE)
      pair_count <- length(pre_d1)
      pre_ones <- sum(pre_d1 == 1, na.rm = TRUE)
      post_ones <- sum(post_d1 == 1, na.rm = TRUE)
      proportion_denominator <- max(1L, pair_count)
      list(
        n_complete_pairs = pair_count,
        pre_ones = pre_ones,
        post_ones = post_ones,
        pre_proportion = pre_ones / proportion_denominator,
        post_proportion = post_ones / proportion_denominator,
        pre1_post0_b = losses,
        pre0_post1_c = gains,
        exact_mcnemar_p_two_sided = private$exact_mcnemar(losses, gains),
        paired_proportion_difference_post_minus_pre = (gains - losses) / proportion_denominator,
        matched_odds_ratio_c_over_b_haldane = (gains + 0.5) / (losses + 0.5)
      )
    },
    calculate_score_statistics = function(pre_score, post_score) {
      differences <- post_score - pre_score
      pre_summary <- private$score_summary(pre_score)
      post_summary <- private$score_summary(post_score)
      change_summary <- private$score_summary(differences)
      wilcoxon <- private$wilcoxon_exact(differences)
      list(
        n_complete_pairs = length(differences),
        pre_mean = pre_summary$mean,
        post_mean = post_summary$mean,
        pre_median = pre_summary$median,
        post_median = post_summary$median,
        pre_iqr = pre_summary$iqr,
        post_iqr = post_summary$iqr,
        difference_mean = change_summary$mean,
        difference_median = change_summary$median,
        difference_iqr = change_summary$iqr,
        difference_ci95_bootstrap_median = private$bootstrap_median_ci(differences),
        increased = sum(differences > 0, na.rm = TRUE),
        decreased = sum(differences < 0, na.rm = TRUE),
        unchanged = sum(differences == 0, na.rm = TRUE),
        wilcoxon_signed_rank_exact = c(
          wilcoxon,
          list(zero_differences_excluded = sum(differences == 0, na.rm = TRUE), ties_use_average_ranks = TRUE)
        )
      )
    },
    score_summary = function(values) {
      if (!length(values)) return(list(mean = 0, median = 0, iqr = c(NA_real_, NA_real_)))
      list(
        mean = mean(values),
        median = stats::median(values),
        iqr = c(private$percentile_value(values, 0.25), private$percentile_value(values, 0.75))
      )
    },
    criterion_frequency = function(dataset, indices, period, criteria) {
      values <- vapply(criteria, function(code) {
        column <- suppressWarnings(as.numeric(dataset[[paste0(period, "_", code)]][indices]))
        sum(column == 1, na.rm = TRUE)
      }, integer(1L))
      setNames(values, criteria)
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

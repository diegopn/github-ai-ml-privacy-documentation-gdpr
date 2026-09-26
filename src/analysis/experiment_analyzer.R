calculate_d1_statistics <- function(pre_d1, post_d1) {
  losses <- sum(pre_d1 == 1 & post_d1 == 0, na.rm = TRUE)
  gains <- sum(pre_d1 == 0 & post_d1 == 1, na.rm = TRUE)
  pair_count <- length(pre_d1)
  list(
    n_complete_pairs = pair_count,
    pre_ones = sum(pre_d1 == 1, na.rm = TRUE),
    post_ones = sum(post_d1 == 1, na.rm = TRUE),
    pre_proportion = if (pair_count) mean(pre_d1 == 1, na.rm = TRUE) else 0,
    post_proportion = if (pair_count) mean(post_d1 == 1, na.rm = TRUE) else 0,
    pre1_post0_b = losses,
    pre0_post1_c = gains,
    exact_mcnemar_p_two_sided = exact_mcnemar(losses, gains),
    paired_proportion_difference_post_minus_pre = if (pair_count) (gains - losses) / pair_count else 0,
    matched_odds_ratio_c_over_b_haldane = (gains + 0.5) / (losses + 0.5)
  )
}

calculate_score_statistics <- function(pre_score, post_score) {
  differences <- post_score - pre_score
  wilcoxon <- wilcoxon_exact(differences)
  list(
    n_complete_pairs = length(differences),
    pre_mean = if (length(pre_score)) mean(pre_score) else 0,
    post_mean = if (length(post_score)) mean(post_score) else 0,
    pre_median = if (length(pre_score)) median(pre_score) else 0,
    post_median = if (length(post_score)) median(post_score) else 0,
    pre_iqr = c(percentile_value(pre_score, 0.25), percentile_value(pre_score, 0.75)),
    post_iqr = c(percentile_value(post_score, 0.25), percentile_value(post_score, 0.75)),
    difference_mean = if (length(differences)) mean(differences) else 0,
    difference_median = if (length(differences)) median(differences) else 0,
    difference_iqr = c(percentile_value(differences, 0.25), percentile_value(differences, 0.75)),
    difference_ci95_bootstrap_median = bootstrap_median_ci(differences),
    increased = sum(differences > 0, na.rm = TRUE),
    decreased = sum(differences < 0, na.rm = TRUE),
    unchanged = sum(differences == 0, na.rm = TRUE),
    wilcoxon_signed_rank_exact = c(
      wilcoxon,
      list(zero_differences_excluded = sum(differences == 0, na.rm = TRUE), ties_use_average_ranks = TRUE)
    )
  )
}

criterion_frequency <- function(dataset, indices, period) {
  values <- vapply(names(RULES), function(code) {
    column <- suppressWarnings(as.numeric(dataset[[paste0(period, "_", code)]][indices]))
    sum(column == 1, na.rm = TRUE)
  }, integer(1L))
  setNames(values, names(RULES))
}

calculate_statistics <- function(records, dataset, sample, input_path = sample_path()) {
  records_by_repository <- index_records(records)
  complete <- vapply(as.character(sample$repository), function(repository) {
    repository <- scalar_text(repository, "")
    if (!nzchar(repository)) return(FALSE)
    record <- records_by_repository[[repository]]
    !is.null(record) && is_complete_record(record)
  }, logical(1L))
  indices <- which(complete)
  collector_versions <- sort(unique(vapply(
    records,
    function(record) scalar_text(record$collector_protocol_version, "unknown"),
    character(1L)
  )))
  numeric_column <- function(name) suppressWarnings(as.numeric(dataset[[name]][indices]))
  pre_d1 <- numeric_column("pre_D1")
  post_d1 <- numeric_column("post_D1")
  pre_score <- numeric_column("pre_score")
  post_score <- numeric_column("post_score")
  rq1 <- calculate_d1_statistics(pre_d1, post_d1)
  generated_at <- format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
  pre_frequency <- criterion_frequency(dataset, indices, "pre")
  post_frequency <- criterion_frequency(dataset, indices, "post")
  rq2 <- calculate_score_statistics(pre_score, post_score)
  list(
    analysis_population = "pares com as duas versões históricas e árvores Git recuperadas",
    total_input_rows = nrow(sample), complete_pairs = length(indices), incomplete_pairs = nrow(sample) - length(indices),
    incomplete_repositories = as.character(sample$repository[!complete]),
    rq1_mcnemar = rq1,
    rq2_wilcoxon = rq2,
    criterion_frequencies = list(pre = as.list(pre_frequency), post = as.list(post_frequency)),
    generated_at = generated_at,
    source_csv = project_relative(input_path),
    significance_level = ALPHA,
    source_sha256 = unique(dataset$sample_source_sha256)[[1L]] %||% "",
    pre_until = PRE_UNTIL, post_until = POST_UNTIL,
    classification_rule_version = RULE_VERSION,
    collector_protocol_version = COLLECTOR_PROTOCOL_VERSION,
    source_collector_protocol_versions = collector_versions,
    selection_topics = as.character(unlist(setting("selection", "topics", character()), use.names = FALSE)),
    selection_min_stars = as.integer(setting("selection", "min_stars", 500L)),
    selection_min_issues = as.integer(setting("selection", "min_issues", 100L)),
    selection_min_activity_months = as.numeric(setting("selection", "min_activity_months", 24)),
    sample_requires_osi_approved_license = TRUE,
    open_source_repositories = nrow(sample)
  )
}


ExperimentAnalyzer <- R6::R6Class(
  "ExperimentAnalyzer",
  public = list(
    config = NULL,
    store = NULL,
    dataset_builder = NULL,
    writer = NULL,
    initialize = function(config, store, dataset_builder, writer) {
      self$config <- config
      self$store <- store
      self$dataset_builder <- dataset_builder
      self$writer <- writer
    },
    run = function(input = self$store$sample_path(), raw = self$store$raw_path(), output = self$config$path("output_root", "outputs")) {
      input <- resolve_project_path(input)
      raw <- resolve_project_path(raw)
      sample <- self$store$assert_published_sample(input)
      source_hash <- self$store$hash_file(input)
      self$store$assert_checkpoint_complete(sample, raw, source_hash)
      records <- self$store$read_jsonl(raw)
      dataset <- self$dataset_builder$build(sample, records, source_hash)$data
      stats <- calculate_statistics(records, dataset, sample, input)
      self$writer$write_all(output_paths(output), stats, sample, source_hash, dataset, input, raw)

      rq1 <- stats$rq1_mcnemar
      rq2 <- stats$rq2_wilcoxon
      cat(sprintf(
        paste0("\nResultados finais\n",
          "Repositórios analisados: %d | pares completos: %d\n",
          "D1 = 1: %d/%d (%.2f%%) → %d/%d (%.2f%%)\n",
          "Diferença D1: %+.2f p.p. | McNemar exato: p = %.6g\n",
          "Score médio (0–7): %.3f → %.3f | Wilcoxon exato: p = %.6g\n"),
        nrow(sample), stats$complete_pairs,
        rq1$pre_ones, rq1$n_complete_pairs, 100 * rq1$pre_proportion,
        rq1$post_ones, rq1$n_complete_pairs, 100 * rq1$post_proportion,
        100 * rq1$paired_proportion_difference_post_minus_pre,
        rq1$exact_mcnemar_p_two_sided, rq2$pre_mean, rq2$post_mean,
        rq2$wilcoxon_signed_rank_exact$p_two_sided_exact
      ))
      invisible(stats)
    }
  )
)

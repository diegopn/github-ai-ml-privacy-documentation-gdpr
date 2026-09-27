ExperimentAnalyzer <- R6::R6Class(
  "ExperimentAnalyzer",
  public = list(
    initialize = function(config, store, dataset_builder, writer, statistics, classifier) {
      if (!inherits(config, "ProjectConfig")) stop("ExperimentAnalyzer exige ProjectConfig.", call. = FALSE)
      if (!inherits(store, "ArtifactStore")) stop("ExperimentAnalyzer exige ArtifactStore.", call. = FALSE)
      private$assert_dependency(dataset_builder, "build", "DatasetBuilder")
      private$assert_dependency(writer, "write_all", "ReportWriter")
      if (!inherits(statistics, "PairedStatistics")) stop("ExperimentAnalyzer exige PairedStatistics.", call. = FALSE)
      if (!inherits(classifier, "PrivacyDocumentClassifier")) stop("ExperimentAnalyzer exige PrivacyDocumentClassifier.", call. = FALSE)
      private$config <- config
      private$store <- store
      private$dataset_builder <- dataset_builder
      private$writer <- writer
      private$statistics <- statistics
      private$classifier <- classifier
      invisible(self)
    },
    run = function(input = private$store$sample_path(), raw = private$store$raw_path(),
                   output = private$config$path("output_root", "outputs")) {
      input <- private$config$resolve(input)
      raw <- private$config$resolve(raw)
      sample <- private$store$assert_published_sample(input)
      source_hash <- private$store$hash_file(input)
      records <- private$store$read_complete_checkpoint(sample, raw, source_hash)
      built <- private$dataset_builder$build(sample, records, source_hash)
      dataset <- built$data
      stats <- private$statistics$calculate(
        records, dataset, sample, input,
        private$classifier$criterion_codes(), private$classifier$rule_version()
      )
      paths <- private$config$output_paths(private$config$resolve(output))
      private$writer$write_all(paths, stats, sample, source_hash, dataset, input, raw)
      rq1 <- stats$rq1_mcnemar
      rq2 <- stats$rq2_wilcoxon
      cat(sprintf(
        paste0("\nResultados finais\n",
          "Repositórios na amostra final analisada: %d\n",
          "D1 = 1: %d/%d (%.2f%%) → %d/%d (%.2f%%)\n",
          "Diferença D1: %+.2f p.p. | McNemar exato: p = %.6g\n",
          "Score médio (0–7): %.3f → %.3f | Wilcoxon exato: p = %.6g\n"),
        stats$complete_pairs, rq1$pre_ones, rq1$n_complete_pairs,
        100 * rq1$pre_proportion, rq1$post_ones, rq1$n_complete_pairs,
        100 * rq1$post_proportion, 100 * rq1$paired_proportion_difference_post_minus_pre,
        rq1$exact_mcnemar_p_two_sided, rq2$pre_mean, rq2$post_mean,
        rq2$wilcoxon_signed_rank_exact$p_two_sided_exact))
      invisible(stats)
    }
  ),
  private = list(
    config = NULL, store = NULL, dataset_builder = NULL, writer = NULL, statistics = NULL, classifier = NULL,
    assert_dependency = function(value, method, label) {
      if (!(inherits(value, "R6") && is.function(value[[method]]))) stop(sprintf("%s precisa implementar $%s().", label, method), call. = FALSE)
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

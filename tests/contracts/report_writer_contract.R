ReportWriterContract <- R6::R6Class(
  "ReportWriterContract",
  public = list(
    run = function(context) {
      fixture <- OfflineExperimentFixture$new(context)$build()
      private$check_transition_cells(context, fixture)
      fixture$records[[1L]]$post$tree_truncated <- TRUE
      fixture$stats <- context$services()$statistics$calculate(
        fixture$records, fixture$dataset, fixture$sample, fixture$input_path,
        context$services()$classifier$criterion_codes(), context$services()$classifier$rule_version()
      )
      report_root <- context$track(tempfile("r6-report-output-"))
      paths <- context$config()$output_paths(report_root)
      writer <- context$classes()$ReportWriter$new(
        context$config(), context$store(), context$services()$classifier
      )
      writer$write_all(paths, fixture$stats, fixture$sample, fixture$source_hash,
        fixture$dataset, fixture$input_path, fixture$raw_path)
      dataset <- utils::read.csv(file.path(report_root, "tables", "final_privacy_gdpr_dataset.csv"),
        check.names = FALSE, stringsAsFactors = FALSE, colClasses = "character")
      evidence <- utils::read.csv(file.path(report_root, "tables", "positive_evidence.csv"),
        check.names = FALSE, stringsAsFactors = FALSE)
      statistics <- jsonlite::fromJSON(file.path(report_root, "metadata", "statistics.json"), simplifyVector = FALSE)
      transition <- utils::read.csv(file.path(report_root, "tables", "d1_transition_table.csv"),
        row.names = 1L, check.names = FALSE)
      report <- readLines(file.path(report_root, "reports", "privacy_documentation_experiment_gdpr.md"), warn = FALSE)
      context$check("ReportWriter exporta dataset, evidências e estatísticas da fixture",
        nrow(dataset) == 5L && nrow(evidence) == 8L && all(nzchar(evidence$evidence)) &&
          statistics$complete_pairs == 4L && sum(as.matrix(transition)) == 4L &&
          all(!evidence$included_in_paired_analysis[evidence$repository == "owner/repository-1" & evidence$period == "pre"]) &&
          all(evidence$included_in_paired_analysis[evidence$repository != "owner/repository-1"]) &&
          file.exists(file.path(report_root, "metadata", "manifest.json")))
      context$check("relatórios e gráficos descrevem os mesmos resultados calculados",
        any(grepl("Pares completos: **4**", report, fixed = TRUE)) &&
          file.info(file.path(report_root, "figures", "d1_pre_post.png"))$size > 0 &&
          file.info(file.path(report_root, "figures", "criteria_post_gdpr.png"))$size > 0)
      manifest <- jsonlite::fromJSON(file.path(paths$metadata, "manifest.json"))
      context$check("manifesto referencia os artefatos no diretório de saída efetivamente utilizado",
        file.path(report_root, "tables", "statistics_r.csv") %in% manifest$files &&
          all(file.exists(vapply(manifest$files, context$config()$resolve, character(1L)))))
      invisible(TRUE)
    }
  ),
  private = list(
    check_transition_cells = function(context, fixture) {
      # Exercise all four cells with unequal counts so swapped cells cannot pass.
      for (period in c("pre", "post")) {
        fixture$dataset[[paste0(period, "_score")]] <- "0"
        for (criterion in paste0("C", 1:7)) fixture$dataset[[paste0(period, "_", criterion)]] <- "0"
      }
      fixture$dataset$pre_D1 <- c("0", "1", "1", "1", "1")
      fixture$dataset$post_D1 <- c("1", "0", "1", "1", "0")
      fixture$records[[5L]]$post$tree_truncated <- TRUE
      fixture$stats <- context$services()$statistics$calculate(
        fixture$records, fixture$dataset, fixture$sample, fixture$input_path,
        context$services()$classifier$criterion_codes(), context$services()$classifier$rule_version()
      )
      paths <- context$config()$output_paths(context$track(tempfile("r6-transition-")))
      context$services()$writer$write_all(paths, fixture$stats, fixture$sample, fixture$source_hash,
        fixture$dataset, fixture$input_path, fixture$raw_path)
      transition <- as.matrix(utils::read.csv(file.path(paths$tables, "d1_transition_table.csv"), row.names = 1L))
      expected <- matrix(c(0L, 1L, 1L, 2L), nrow = 2L, byrow = TRUE,
        dimnames = list(c("pre_0", "pre_1"), c("post_0", "post_1")))
      context$check("transições D1 preservam cada célula e excluem pares truncados",
        identical(transition, expected))
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

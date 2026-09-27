SiteContentContract <- R6::R6Class(
  "SiteContentContract",
  public = list(
    run = function(context) {
      project <- context$new_test_project()
      settings_path <- file.path(project$root, "config", "settings.yml")
      settings <- yaml::read_yaml(settings_path)
      settings$paths$output_root <- "resultados locais"
      yaml::write_yaml(settings, settings_path)
      project$config <- context$classes()$ProjectConfig$new(project$root, context$values())
      project$store <- context$classes()$ArtifactStore$new(project$config, context$values(), context$protocol())
      dir.create(file.path(project$root, "site"))
      file.copy(file.path(context$project_root(), "site", "index-template.md"), file.path(project$root, "site", "index-template.md"))
      fixture <- OfflineExperimentFixture$new(context)$build()
      fixture$records[[1L]]$post$tree_truncated <- TRUE
      fixture$stats <- context$services()$statistics$calculate(fixture$records, fixture$dataset,
        fixture$sample, fixture$input_path, context$services()$classifier$criterion_codes(),
        context$services()$classifier$rule_version())
      project$store$publish_sample(fixture$sample, list(protocol_version = "offline-test"))
      fixture$source_hash <- project$store$hash_file(project$store$sample_path())
      fixture$stats$source_sha256 <- fixture$source_hash
      fixture$stats$significance_level <- 0.01
      paths <- project$config$output_paths()
      writer <- context$classes()$ReportWriter$new(project$config, project$store, context$services()$classifier)
      writer$write_all(paths, fixture$stats, fixture$sample, fixture$source_hash,
        fixture$dataset, fixture$input_path, fixture$raw_path)
      site <- context$classes()$SiteContentWriter$new(project$config, project$store)
      page <- paste(readLines(site$write(), warn = FALSE), collapse = "\n")
      transition <- utils::read.csv(file.path(paths$tables, "d1_transition_table.csv"), row.names = 1L)
      names(transition) <- c("Pós = 0", "Pós = 1")
      rownames(transition) <- c("Pré = 0", "Pré = 1")
      expected <- paste(knitr::kable(transition, format = "html", caption = "Matriz de transição de D1"), collapse = "\n")
      context$check("site usa a tabela publicada e a mesma população pareada das estatísticas",
        all(grepl(expected, page, fixed = TRUE), grepl('"complete_pairs":4', page, fixed = TRUE),
          grepl('"selected_repositories":5', page, fixed = TRUE),
          grepl('"excluded_repositories":1', page, fixed = TRUE),
          !grepl('"sample_rows"', page, fixed = TRUE)))
      context$check("site substitui os dados sem emitir código R ou marcadores pendentes",
        all(!grepl("`r ", page, fixed = TRUE), !grepl("```{r", page, fixed = TRUE),
          !grepl("\\{\\{[a-z_]+\\}\\}", page), grepl("0,750", page, fixed = TRUE)))
      context$check("site usa caminhos configurados e parâmetros estatísticos dos artefatos",
        all(grepl("resultados%20locais/figures/d1_pre_post.png", page, fixed = TRUE),
          grepl("alpha=0{,}01", page, fixed = TRUE)))
      output <- site$write()
      Sys.setFileTime(output, Sys.time() - 3600)
      original_mtime <- file.info(output)$mtime
      site$write()
      context$check("preparar o mesmo site preserva o arquivo e evita disparar novamente o preview",
        identical(file.info(output)$mtime, original_mtime))
      unlink(project$store$audit_path())
      page_without_audit <- paste(readLines(site$write(), warn = FALSE), collapse = "\n")
      context$check("site mantém o texto alternativo quando não existe manifesto de seleção",
        grepl("O manifesto das consultas de seleção será criado", page_without_audit, fixed = TRUE))
      output_hash <- project$store$hash_file(output)
      template_path <- file.path(project$root, "site", "index-template.md")
      cat("\n{{unknown_marker}}\n", file = template_path, append = TRUE)
      context$check("marcadores sem valor impedem a publicação e preservam a página anterior",
        all(context$errors(site$write()), identical(output_hash, project$store$hash_file(output))))
      project$store$write_csv(fixture$sample[-1L, ], project$store$sample_path())
      context$check("site rejeita estatísticas de outra amostra sem substituir a página anterior",
        all(context$errors(site$write()), identical(output_hash, project$store$hash_file(output))))
      private$check_dynamic_population(context)
      invisible(TRUE)
    }
  ),
  private = list(
    check_dynamic_population = function(context) {
      project <- context$new_test_project()
      dir.create(file.path(project$root, "site"))
      file.copy(file.path(context$project_root(), "site", "index-template.md"),
        file.path(project$root, "site", "index-template.md"))
      writer <- context$classes()$ReportWriter$new(project$config, project$store, context$services()$classifier)
      site <- context$classes()$SiteContentWriter$new(project$config, project$store)
      # Reuse the same output paths across changing populations, including none and all.
      scenarios <- list(c(4L, 3L), c(3L, 1L), c(2L, 0L), c(5L, 5L))
      for (scenario in scenarios) {
        fixture <- OfflineExperimentFixture$new(context)$build()
        selected <- scenario[[1L]]
        complete <- scenario[[2L]]
        fixture$sample <- fixture$sample[seq_len(selected), , drop = FALSE]
        fixture$dataset <- fixture$dataset[seq_len(selected), , drop = FALSE]
        fixture$records <- fixture$records[seq_len(selected)]
        included <- seq_len(selected) <= complete
        fixture$sample$ai_ml_match_topics <- ifelse(included, "included-topic", "excluded-only-topic")
        # Both collection failures and invalid classifications must follow the statistical population.
        for (index in which(!included)) fixture$records[[index]]$post$tree_truncated <- TRUE
        if (complete == 1L) {
          fixture$records[[selected]]$post$tree_truncated <- FALSE
          fixture$dataset$post_score[[selected]] <- NA_character_
        }
        project$store$publish_sample(fixture$sample, list(protocol_version = "offline-test"))
        hash <- project$store$hash_file(project$store$sample_path())
        fixture$dataset$sample_source_sha256 <- hash
        stats <- context$services()$statistics$calculate(fixture$records, fixture$dataset,
          fixture$sample, project$store$sample_path(), context$services()$classifier$criterion_codes(),
          context$services()$classifier$rule_version())
        paths <- project$config$output_paths()
        writer$write_all(paths, stats, fixture$sample, hash, fixture$dataset,
          project$store$sample_path(), fixture$raw_path)
        page <- paste(readLines(site$write(), warn = FALSE), collapse = "\n")
        cards <- strsplit(strsplit(page, "## Visão geral", fixed = TRUE)[[1L]][[2L]],
          "## Resultado principal", fixed = TRUE)[[1L]][[1L]]
        context$check(sprintf("cartão e descrição acompanham automaticamente %d pares em %d selecionados", complete, selected),
          all(grepl(sprintf("### %d\nRepositórios na amostra final analisada", complete), cards, fixed = TRUE),
            length(gregexpr(".metric-card", cards, fixed = TRUE)[[1L]]) == 3L,
            !grepl("Pares históricos completos", cards, fixed = TRUE),
            grepl(sprintf("A amostra final analisada contém %d", complete), page, fixed = TRUE),
            grepl(sprintf('"complete_pairs":%d', complete), page, fixed = TRUE),
            grepl(sprintf('"selected_repositories":%d', selected), page, fixed = TRUE),
            grepl(sprintf('"excluded_repositories":%d', selected - complete), page, fixed = TRUE)))
        analyzed <- utils::read.csv(file.path(paths$tables, "analyzed_dataset.csv"))
        sample <- utils::read.csv(file.path(paths$tables, "analyzed_sample.csv"))
        evidence <- utils::read.csv(file.path(paths$tables, "positive_evidence.csv"))
        transition <- utils::read.csv(file.path(paths$tables, "d1_transition_table.csv"), row.names = 1L)
        frequencies <- utils::read.csv(file.path(paths$tables, "criterion_frequencies.csv"))
        context$check(sprintf("tabelas, tópicos e evidências usam somente os %d pares estatísticos", complete),
          all(nrow(analyzed) == complete, nrow(sample) == complete,
            identical(as.character(analyzed$repository), fixture$sample$repository[included]),
            all(evidence$repository %in% sample$repository),
            sum(as.matrix(transition)) == complete,
            all(frequencies$pre <= complete), all(frequencies$post <= complete),
            !grepl("excluded-only-topic", page, fixed = TRUE),
            (complete == 0L || grepl("included-topic", page, fixed = TRUE))))
        for (report in c("statistical_results.md", "privacy_documentation_experiment_gdpr.md")) {
          text <- paste(readLines(file.path(paths$reports, report), warn = FALSE), collapse = "\n")
          context$check(sprintf("%s acompanha a amostra final de %d pares e mantém a rastreabilidade", report, complete),
            all(grepl(sprintf("Repositórios na amostra final analisada: **%d**", complete), text, fixed = TRUE),
              grepl(sprintf("Repositórios inicialmente selecionados: **%d**", selected), text, fixed = TRUE),
              grepl(sprintf("análise: **%d**", selected - complete), text, fixed = TRUE)))
        }
      }
      for (language in c("pt-BR", "en-US")) {
        copy <- jsonlite::read_json(file.path(context$project_root(), "site", "locales", paste0(language, ".json")))
        context$check(sprintf("%s usa um único indicador e campos dinâmicos da população", language),
          all(nzchar(copy$finalAnalyzedRepositories), is.null(copy$sampleRepositories), is.null(copy$completePairs),
            grepl("{pairs}", copy$templates[["sample-description"]], fixed = TRUE),
            all(vapply(c("{selected}", "{excluded}", "{pairs}"), function(value)
              grepl(value, copy$templates[["sample-flow"]], fixed = TRUE), logical(1L)))))
      }
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

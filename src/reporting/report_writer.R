ReportWriter <- R6::R6Class(
  "ReportWriter",
  public = list(
    initialize = function(config, store, classifier) {
      if (!inherits(config, "ProjectConfig"))
          stop("ReportWriter exige ProjectConfig.", call. = FALSE)
      if (!inherits(store, "ArtifactStore"))
          stop("ReportWriter exige ArtifactStore.", call. = FALSE)
      if (!inherits(classifier, "PrivacyDocumentClassifier"))
          stop("ReportWriter exige PrivacyDocumentClassifier.", call. = FALSE)
      private$config <- config
      private$store <- store
      private$classifier <- classifier
    },

    write_all = function(paths, stats, sample, source_hash, dataset, input_path, raw_path) {
      # Use the population already selected by PairedStatistics; do not reapply inclusion rules.
      analyzed_sample <- sample[!sample$repository %in% unlist(stats$incomplete_repositories), , drop = FALSE]
      analyzed_dataset <- dataset[match(analyzed_sample$repository, dataset$repository), , drop = FALSE]
      if (nrow(analyzed_sample) != stats$complete_pairs ||
        !identical(as.character(analyzed_dataset$repository), as.character(analyzed_sample$repository))) {
        stop("A amostra final analisada não corresponde ao resultado estatístico.", call. = FALSE)
      }
      for (directory in paths[-1L]) dir.create(directory, recursive = TRUE, showWarnings = FALSE)
      private$store$write_csv(analyzed_sample, file.path(paths$tables, "analyzed_sample.csv"))
      private$store$write_csv(analyzed_dataset, file.path(paths$tables, "analyzed_dataset.csv"))
      # Retain the complete input and classified rows as traceability artifacts.
      private$store$write_csv(sample, file.path(paths$tables, "sample_used.csv"))
      private$store$write_lines(c(paste0("sha256  ", source_hash), paste0("source  ", private$config$relative(input_path)),
          paste0("rows  ", nrow(sample))), file.path(paths$metadata, "sample_used.sha256.txt"))
      private$store$write_csv(dataset, file.path(paths$tables, "final_privacy_gdpr_dataset.csv"))
      private$store$write_csv(private$positive_evidence_table(analyzed_dataset, stats$incomplete_repositories), file.path(paths$tables,
          "positive_evidence.csv"))
      private$write_derived_outputs(paths, stats)
      private$store$write_json(stats, file.path(paths$metadata, "statistics.json"), pretty = TRUE)
      private$store$write_lines(private$stats_markdown(stats), file.path(paths$reports,
          "statistical_results.md"))
      private$store$write_lines(private$report_markdown(stats, sample, source_hash, dataset, input_path,
          raw_path), file.path(paths$reports, "privacy_documentation_experiment_gdpr.md"))
      private$store$write_json(private$manifest(stats, input_path, raw_path, paths), file.path(paths$metadata,
          "manifest.json"), pretty = TRUE)
      session_info <- sub("[[:space:]]+$", "", capture.output(sessionInfo()))
      private$store$write_lines(session_info, file.path(paths$metadata, "session_info.txt"))
      invisible(paths)
    }
  ),
  private = list(
    config = NULL,

    store = NULL,

    classifier = NULL,

    fmt_num = function(value, digits = 3L) {
      if (is.null(value) || length(value) == 0L || is.na(value))
          return("NA")
      formatC(as.numeric(value), format = "f", digits = digits, decimal.mark = ".")
    },

    fmt_p = function(value) {
      if (is.null(value) || length(value) == 0L || is.na(value))
          return("NA")
      value <- as.numeric(value)
      if (!is.finite(value) || value < 0)
          return("NA")
      if (value == 0)
          return("<5e-324")
      if (value < 0.001)
          return(formatC(value, format = "e", digits = 2L, decimal.mark = "."))
      private$fmt_num(value, 6L)
    },

    fmt_pct = function(value) paste0(private$fmt_num(as.numeric(value) * 100, 1L), "%"),

    stats_markdown = function(stats) {
      r1 <- stats$rq1_mcnemar
      r2 <- stats$rq2_wilcoxon
      w <- r2$wilcoxon_signed_rank_exact
      lines <- c("# Resultados estatísticos", "", "## Amostra final analisada", "",
          sprintf("- Repositórios na amostra final analisada: **%d**.", stats$complete_pairs), "",
          "Todos os indicadores, proporções, médias, frequências, tabelas e gráficos usam esta mesma amostra final analisada.",
          "", "## Metodologia e rastreabilidade", "",
          private$population_traceability(stats),
          sprintf("- Pré: último commit até `%s`.", stats$pre_until), sprintf("- Pós: último commit até `%s`.",
              stats$post_until), sprintf("- Nível de significância: **α = %.3f**.", stats$significance_level),
          "", "## RQ1: McNemar exato bicaudal para D1", "", "| Medida | Resultado |", "|---|---:|",
          sprintf("| Pré D1 = 1 | %d (%s) |", r1$pre_ones, private$fmt_pct(r1$pre_proportion)),
          sprintf("| Pós D1 = 1 | %d (%s) |", r1$post_ones, private$fmt_pct(r1$post_proportion)),
          sprintf("| Pré 1 → pós 0 | %d |", r1$pre1_post0_b), sprintf("| Pré 0 → pós 1 | %d |",
              r1$pre0_post1_c), sprintf("| Diferença de proporções pós−pré | %s |",
              private$fmt_pct(r1$paired_proportion_difference_post_minus_pre)), sprintf("| Odds ratio pareado com correção de Haldane | %s |",
              private$fmt_num(r1$matched_odds_ratio_c_over_b_haldane)), sprintf("| p exato bicaudal | %s |",
              private$fmt_p(r1$exact_mcnemar_p_two_sided)), "", "## RQ2: Wilcoxon pareado para score 0–7",
          "", "As diferenças iguais a zero foram excluídas dos postos; empates nos valores absolutos receberam postos médios.",
          "", "| Medida | Pré | Pós |", "|---|---:|---:|", sprintf("| Média | %s | %s |",
              private$fmt_num(r2$pre_mean), private$fmt_num(r2$post_mean)), sprintf("| Mediana | %s | %s |",
              private$fmt_num(r2$pre_median), private$fmt_num(r2$post_median)), sprintf("| IQR | %s–%s | %s–%s |",
              private$fmt_num(r2$pre_iqr[[1L]]), private$fmt_num(r2$pre_iqr[[2L]]), private$fmt_num(r2$post_iqr[[1L]]),
              private$fmt_num(r2$post_iqr[[2L]])), "", "| Medida pareada | Resultado |", "|---|---:|",
          sprintf("| Diferença média | %s |", private$fmt_num(r2$difference_mean)), sprintf("| Diferença mediana | %s |",
              private$fmt_num(r2$difference_median)), sprintf("| IQR das diferenças | %s–%s |",
              private$fmt_num(r2$difference_iqr[[1L]]), private$fmt_num(r2$difference_iqr[[2L]])),
          sprintf("| IC95%% bootstrap da mediana | %s–%s |", private$fmt_num(r2$difference_ci95_bootstrap_median[[1L]]),
              private$fmt_num(r2$difference_ci95_bootstrap_median[[2L]])), sprintf("| Aumentaram / diminuíram / iguais | %d / %d / %d |",
              r2$increased, r2$decreased, r2$unchanged), sprintf("| W+ / W− | %s / %s |",
              private$fmt_num(w$w_plus), private$fmt_num(w$w_minus)), sprintf("| p bicaudal exato | %s |",
              private$fmt_p(w$p_two_sided_exact)), sprintf("| Correlação bisserial de postos | %s |",
              private$fmt_num(w$rank_biserial, 6L)), "", paste(
            "As regras foram aplicadas de forma conservadora;",
            "menções isoladas não foram consideradas evidência suficiente."
          ))
      enc2utf8(lines)
    },

    report_markdown = function(stats, sample, source_hash, dataset, input_path, raw_path) {
      r1 <- stats$rq1_mcnemar
      r2 <- stats$rq2_wilcoxon
      selection_topics <- stats$selection_topics
      lines <- c("# Experimento: documentação de privacidade em repositórios públicos de IA/ML após a aplicação da GDPR",
          "", "## Estado da execução", "", paste(
            "Este projeto executa a coleta, a classificação textual e a análise estatística em R.",
            "A classificação final usa regras semânticas conservadoras."
          ),
          "", "## Entrada e desenho", "", sprintf("- CSV de entrada: `%s`.", private$config$relative(input_path)),
          sprintf("- SHA-256 do CSV: `%s`.", source_hash), private$population_traceability(stats), sprintf("- Protocolos de coleta presentes no checkpoint: `%s`.",
              paste(stats$source_collector_protocol_versions, collapse = "`, `")), "- Critério de licença: **SPDX/OSI aprovada para todos os repositórios**.",
          sprintf("- Versão pré-GDPR: último commit até `%s`.", stats$pre_until), sprintf("- Versão pós-GDPR: último commit até `%s`.",
              stats$post_until), sprintf("- Tópicos de descoberta usados (%d): `%s`.", length(selection_topics),
              paste(selection_topics, collapse = "`, `")), sprintf("- Filtros preservados: pelo menos %d estrelas, %d issues reais e %.0f meses de atividade.",
              stats$selection_min_stars, stats$selection_min_issues, stats$selection_min_activity_months),
          sprintf("- Manifesto das consultas particionadas: `%s`.", private$config$relative(private$config$path("selection_audit",
              "outputs/metadata/selection_search_manifest.json"))), sprintf("- Nível de significância: **α = %.3f**.",
              stats$significance_level), "", paste(
            "A comparação é pareada e observacional.",
            "O resultado mede evidência documental versionada; não demonstra causalidade da GDPR nem conformidade jurídica."
          ),
          "", "## Classificação", "", paste(
            "O coletor filtra README de raiz e documentos textuais ligados a privacidade, segurança, proteção de dados,",
            "termos, jurídico, retenção, consentimento e conformidade. O analisador exclui datasets, testes, fixtures,",
            "exemplos, dependências e listas bibliográficas."
          ),
          "", paste(
            "D1 indica presença de evidência documental contextualizada. O PDE Score soma C1 a C7: dados pessoais, finalidade,",
            "base legal, direitos, retenção ou exclusão, compartilhamento e proteção.",
            "Um valor zero significa ausência de evidência suficiente nos documentos recuperados,",
            "não ausência comprovada de práticas de privacidade."
          ),
          "", "## Resultados", "",
          sprintf("- Repositórios na amostra final analisada: **%d**.", stats$complete_pairs), "",
          "Todos os indicadores, proporções, médias, frequências, tabelas e gráficos usam esta mesma amostra final analisada.", "",
          sprintf("- D1 pré: **%d/%d** (%s).", r1$pre_ones, stats$complete_pairs,
              private$fmt_pct(r1$pre_proportion)), sprintf("- D1 pós: **%d/%d** (%s).", r1$post_ones,
              stats$complete_pairs, private$fmt_pct(r1$post_proportion)), sprintf("- Score médio pré/pós: **%s / %s**.", private$fmt_num(r2$pre_mean),
              private$fmt_num(r2$post_mean)), sprintf("- McNemar exato bicaudal: **p=%s**.",
              private$fmt_p(r1$exact_mcnemar_p_two_sided)), sprintf("- Wilcoxon exato bicaudal: **p=%s**.",
              private$fmt_p(r2$wilcoxon_signed_rank_exact$p_two_sided_exact)), "", "## Limitações",
          "", paste(
            "Documentos externos ao repositório, práticas não versionadas e textos que não correspondem às expressões das regras podem não ser detectados.",
            "O score é discreto e concentrado em zero.",
            "Os p-valores devem ser interpretados junto com as contagens, evidências e tamanho das diferenças."
          ),
          "", "## Reprodução", "", sprintf(paste(
            "A coleta grava um checkpoint JSONL em `%s`.",
            "A análise pode ser repetida sem acesso à API usando o mesmo CSV e esse checkpoint.",
            "O dataset, as estatísticas e os relatórios são derivados desses arquivos."
          ),
              private$config$relative(raw_path)), "", sprintf("Versão das regras: `%s`.",
              stats$classification_rule_version), sprintf("Repositórios no dataset da amostra final analisada: **%d**.",
              stats$complete_pairs))
      enc2utf8(lines)
    },

    population_traceability = function(stats) {
      excluded <- as.character(unlist(stats$incomplete_repositories, use.names = FALSE))
      c(
        sprintf("- Repositórios inicialmente selecionados: **%d**.", stats$total_input_rows),
        sprintf("- Repositórios excluídos por ausência de par histórico completo válido para análise: **%d**.", stats$incomplete_pairs),
        if (length(excluded)) sprintf("- Repositórios excluídos: `%s`.", paste(excluded, collapse = "`, `")),
        "- A amostra final analisada corresponde aos pares históricos completos efetivamente usados na análise estatística.",
        "- A seleção inicial e todas as linhas classificadas são preservadas em `sample_used.csv` e `final_privacy_gdpr_dataset.csv` para rastreabilidade."
      )
    },

    manifest = function(stats, input_path, raw_path, paths) {
      audit_path <- private$config$path("selection_audit", "outputs/metadata/selection_search_manifest.json")
      manifest <- list(generated_at = stats$generated_at, project_name = as.character(private$config$get("project",
          "name", "privacy-documentation-experiment")), input_csv = private$config$relative(input_path),
          input_sha256 = stats$source_sha256, input_rows = stats$total_input_rows, dataset_rows = stats$total_input_rows,
          complete_pairs = stats$complete_pairs, incomplete_pairs = stats$incomplete_pairs,
          analyzed_sample_rows = stats$complete_pairs, analyzed_dataset_rows = stats$complete_pairs,
          incomplete_repositories = stats$incomplete_repositories,
          significance_level = stats$significance_level, classification_rule_version = stats$classification_rule_version,
          collector_protocol_version = stats$collector_protocol_version, source_collector_protocol_versions = stats$source_collector_protocol_versions,
          wilcoxon_method = "exact-two-sided-average-ranks-zero-differences-excluded", sample_requires_osi_approved_license = TRUE,
          selection_topics = stats$selection_topics, selection_audit = if (file.exists(audit_path)) private$config$relative(audit_path) else NULL,
          files = c(private$config$relative(input_path), private$config$relative(raw_path),
            private$artifact_files(paths),
            if (file.exists(audit_path)) private$config$relative(audit_path) else character()))

      manifest
    },

    artifact_files = function(paths) {
      files <- list(
        tables = c("sample_used.csv", "final_privacy_gdpr_dataset.csv", "analyzed_sample.csv", "analyzed_dataset.csv", "positive_evidence.csv",
          "statistics_r.csv", "criterion_frequencies.csv", "d1_transition_table.csv"),
        figures = c("d1_pre_post.png", "criteria_post_gdpr.png"),
        metadata = c("statistics.json", "sample_used.sha256.txt", "session_info.txt", "manifest.json"),
        reports = c("statistical_results.md", "privacy_documentation_experiment_gdpr.md")
      )
      unlist(lapply(names(files), function(section) {
        vapply(file.path(paths[[section]], files[[section]]), private$config$relative, character(1L))
      }), use.names = FALSE)
    },

    positive_evidence_table = function(dataset, incomplete_repositories) {
      columns <- unlist(lapply(c("pre", "post"), function(period) {
        paste0(period, "_", c("D1", private$classifier$criterion_codes()))
      }), use.names = FALSE)
      positions <- which(as.matrix(dataset[columns]) == "1", arr.ind = TRUE)
      positions <- positions[order(positions[, 1L], positions[, 2L]), , drop = FALSE]
      repositories <- dataset$repository[positions[, 1L]]
      indicators <- columns[positions[, 2L]]
      data.frame(
        repository = repositories,
        period = sub("_.*$", "", indicators),
        indicator = sub("^[^_]+_", "", indicators),
        evidence = as.matrix(dataset[paste0(columns, "_evidence")])[positions],
        included_in_paired_analysis = !repositories %in% incomplete_repositories,
        stringsAsFactors = FALSE
      )
    },
    write_derived_outputs = function(paths, stats) {
      criteria <- private$classifier$criterion_codes()
      r1 <- stats$rq1_mcnemar
      private$store$write_csv(private$d1_transition_table(r1), file.path(paths$tables, "d1_transition_table.csv"),
          row.names = TRUE)
      frequencies <- data.frame(criterion = criteria, pre = vapply(criteria, function(code) stats$criterion_frequencies$pre[[code]],
          numeric(1L)), post = vapply(criteria, function(code) stats$criterion_frequencies$post[[code]],
          numeric(1L)), stringsAsFactors = FALSE)
      private$store$write_csv(frequencies, file.path(paths$tables, "criterion_frequencies.csv"))
      summary <- private$statistics_table(stats)
      private$store$write_csv(summary, file.path(paths$tables, "statistics_r.csv"))
      private$write_barplot(file.path(paths$figures, "d1_pre_post.png"),
        c(r1$pre_ones, r1$post_ones), c("Pré-GDPR", "Pós-GDPR"),
        "Repositórios com D1 = 1", "Presença documental de privacidade", c("#f2a36b", "#c84b18"))
      private$write_barplot(file.path(paths$figures, "criteria_post_gdpr.png"),
        frequencies$post, frequencies$criterion, "Repositórios",
        "Critérios documentais no período pós-GDPR", "#d85f2a")
    },

    d1_transition_table = function(result) {
      data.frame(
        post_0 = c(result$n_complete_pairs - result$pre_ones - result$pre0_post1_c, result$pre1_post0_b),
        post_1 = c(result$pre0_post1_c, result$pre_ones - result$pre1_post0_b),
        row.names = c("pre_0", "pre_1")
      )
    },

    statistics_table = function(stats) {
      r1 <- stats$rq1_mcnemar
      r2 <- stats$rq2_wilcoxon
      w <- r2$wilcoxon_signed_rank_exact
      summary <- data.frame(indicador = c("repositorios_na_amostra_final_analisada",
          "D1_pre", "D1_pos", "pre_1_pos_0", "pre_0_pos_1", "p_McNemar_exato_bicaudal", "media_score_pre",
          "media_score_pos", "diferenca_media_score", "aumentos_score", "reducoes_score", "empates_score",
          "diferencas_nao_nulas", "W_mais", "W_menos", "p_Wilcoxon_exato_bicaudal", "correlacao_bisserial"),
          valor = c(stats$complete_pairs, r1$pre_ones,
              r1$post_ones, r1$pre1_post0_b, r1$pre0_post1_c, r1$exact_mcnemar_p_two_sided,
              r2$pre_mean, r2$post_mean, r2$difference_mean, r2$increased, r2$decreased, r2$unchanged,
              w$n_nonzero, w$w_plus, w$w_minus, w$p_two_sided_exact, w$rank_biserial), stringsAsFactors = FALSE)
      summary
    },

    write_barplot = function(path, heights, labels, ylab, title, colors) {
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      temporary <- tempfile(paste0(".", basename(path), "-"), tmpdir = dirname(path), fileext = ".png")
      on.exit(unlink(temporary, force = TRUE), add = TRUE)
      grDevices::png(temporary, width = 1000, height = 650, res = 120)
      device_id <- grDevices::dev.cur()
      on.exit(if (device_id %in% grDevices::dev.list()) grDevices::dev.off(device_id), add = TRUE)
      graphics::barplot(heights, names.arg = labels, ylab = ylab, main = title, col = colors)
      grDevices::dev.off(device_id)
      if (!file.rename(temporary, path)) {
        stop(sprintf("Não foi possível finalizar o gráfico: %s", path), call. = FALSE)
      }
      invisible(path)
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

fmt_num <- function(value, digits = 3L) {
  if (is.null(value) || length(value) == 0L || is.na(value)) return("NA")
  formatC(as.numeric(value), format = "f", digits = digits, decimal.mark = ".")
}

fmt_p <- function(value) {
  if (is.null(value) || length(value) == 0L || is.na(value)) return("NA")
  value <- as.numeric(value)
  if (!is.finite(value) || value < 0) return("NA")
  if (value == 0) return("<5e-324")
  if (value < 0.001) return(formatC(value, format = "e", digits = 2L, decimal.mark = "."))
  fmt_num(value, 6L)
}

fmt_pct <- function(value) paste0(fmt_num(as.numeric(value) * 100, 1L), "%")

write_stats_markdown <- function(path, stats) {
  r1 <- stats$rq1_mcnemar
  r2 <- stats$rq2_wilcoxon
  w <- r2$wilcoxon_signed_rank_exact
  lines <- c(
    "# Resultados estatísticos", "", "## População", "",
    sprintf("- Linhas de entrada: **%d**.", stats$total_input_rows),
    sprintf("- Repositórios com licença SPDX/OSI aprovada: **%d**.", stats$open_source_repositories),
    sprintf("- Pares completos: **%d**.", stats$complete_pairs),
    sprintf("- Pares incompletos preservados e excluídos dos testes: **%d**.", stats$incomplete_pairs),
    sprintf("- Pré: último commit até `%s`.", PRE_UNTIL), sprintf("- Pós: último commit até `%s`.", POST_UNTIL),
    sprintf("- Nível de significância: **α = %.3f**.", ALPHA), "",
    "## RQ1: McNemar exato bicaudal para D1", "",
    "| Medida | Resultado |", "|---|---:|",
    sprintf("| Pré D1 = 1 | %d (%s) |", r1$pre_ones, fmt_pct(r1$pre_proportion)),
    sprintf("| Pós D1 = 1 | %d (%s) |", r1$post_ones, fmt_pct(r1$post_proportion)),
    sprintf("| Pré 1 → pós 0 | %d |", r1$pre1_post0_b), sprintf("| Pré 0 → pós 1 | %d |", r1$pre0_post1_c),
    sprintf("| Diferença de proporções pós−pré | %s |", fmt_pct(r1$paired_proportion_difference_post_minus_pre)),
    sprintf("| Odds ratio pareado com correção de Haldane | %s |", fmt_num(r1$matched_odds_ratio_c_over_b_haldane)),
    sprintf("| p exato bicaudal | %s |", fmt_p(r1$exact_mcnemar_p_two_sided)), "",
    "## RQ2: Wilcoxon pareado para score 0–7", "",
    "As diferenças iguais a zero foram excluídas dos postos; empates nos valores absolutos receberam postos médios.", "",
    "| Medida | Pré | Pós |", "|---|---:|---:|",
    sprintf("| Média | %s | %s |", fmt_num(r2$pre_mean), fmt_num(r2$post_mean)),
    sprintf("| Mediana | %s | %s |", fmt_num(r2$pre_median), fmt_num(r2$post_median)),
    sprintf("| IQR | %s–%s | %s–%s |", fmt_num(r2$pre_iqr[[1L]]), fmt_num(r2$pre_iqr[[2L]]), fmt_num(r2$post_iqr[[1L]]), fmt_num(r2$post_iqr[[2L]])), "",
    "| Medida pareada | Resultado |", "|---|---:|",
    sprintf("| Diferença média | %s |", fmt_num(r2$difference_mean)), sprintf("| Diferença mediana | %s |", fmt_num(r2$difference_median)),
    sprintf("| IQR das diferenças | %s–%s |", fmt_num(r2$difference_iqr[[1L]]), fmt_num(r2$difference_iqr[[2L]])),
    sprintf("| IC95%% bootstrap da mediana | %s–%s |", fmt_num(r2$difference_ci95_bootstrap_median[[1L]]), fmt_num(r2$difference_ci95_bootstrap_median[[2L]])),
    sprintf("| Aumentaram / diminuíram / iguais | %d / %d / %d |", r2$increased, r2$decreased, r2$unchanged),
    sprintf("| W+ / W− | %s / %s |", fmt_num(w$w_plus), fmt_num(w$w_minus)), sprintf("| p bicaudal exato | %s |", fmt_p(w$p_two_sided_exact)),
    sprintf("| Correlação bisserial de postos | %s |", fmt_num(w$rank_biserial, 6L)), "",
    "As regras foram aplicadas de forma conservadora; menções isoladas não foram consideradas evidência suficiente."
  )
  writeLines(enc2utf8(lines), path, useBytes = TRUE)
}

write_report <- function(path, stats, sample, source_hash, dataset, input_path = sample_path(), raw_path = raw_checkpoint_path()) {
  r1 <- stats$rq1_mcnemar
  r2 <- stats$rq2_wilcoxon
  selection_topics <- as.character(unlist(setting("selection", "topics", character()), use.names = FALSE))
  lines <- c(
    "# Experimento: documentação de privacidade em repositórios públicos de IA/ML após a aplicação da GDPR", "",
    "## Estado da execução", "",
    "Este projeto executa a coleta, a classificação textual e a análise estatística em R. A classificação final usa regras semânticas conservadoras.", "",
    "## Entrada e desenho", "",
    sprintf("- CSV de entrada: `%s`.", project_relative(input_path)),
    sprintf("- SHA-256 do CSV: `%s`.", source_hash), sprintf("- Linhas da amostra: **%d**.", nrow(sample)),
    sprintf("- Protocolos de coleta presentes no checkpoint: `%s`.", paste(stats$source_collector_protocol_versions, collapse = "`, `")),
    "- Critério de licença: **SPDX/OSI aprovada para todos os repositórios**.",
    sprintf("- Versão pré-GDPR: último commit até `%s`.", PRE_UNTIL),
    sprintf("- Versão pós-GDPR: último commit até `%s`.", POST_UNTIL),
    sprintf("- Tópicos de descoberta usados (%d): `%s`.", length(selection_topics), paste(selection_topics, collapse = "`, `")),
    sprintf("- Filtros preservados: pelo menos %d estrelas, %d issues reais e %.0f meses de atividade.",
            as.integer(setting("selection", "min_stars", 500L)),
            as.integer(setting("selection", "min_issues", 100L)),
            as.numeric(setting("selection", "min_activity_months", 24))),
    sprintf("- Manifesto das consultas particionadas: `%s`.", project_relative(selection_audit_path())),
    sprintf("- Nível de significância: **α = %.3f**.", ALPHA), "",
    "A comparação é pareada e observacional. O resultado mede evidência documental versionada; não demonstra causalidade da GDPR nem conformidade jurídica.", "",
    "## Classificação", "",
    "O coletor filtra README de raiz e documentos textuais ligados a privacidade, segurança, proteção de dados, termos, jurídico, retenção, consentimento e conformidade. O analisador exclui datasets, testes, fixtures, exemplos, dependências e listas bibliográficas.", "",
    "D1 indica presença de evidência documental contextualizada. O PDE Score soma C1 a C7: dados pessoais, finalidade, base legal, direitos, retenção ou exclusão, compartilhamento e proteção. Um valor zero significa ausência de evidência suficiente nos documentos recuperados, não ausência comprovada de práticas de privacidade.", "",
    "## Resultados", "",
    sprintf("- D1 pré: **%d/%d** (%s).", r1$pre_ones, stats$complete_pairs, fmt_pct(r1$pre_proportion)),
    sprintf("- D1 pós: **%d/%d** (%s).", r1$post_ones, stats$complete_pairs, fmt_pct(r1$post_proportion)),
    sprintf("- Pares completos: **%d**.", stats$complete_pairs), sprintf("- Score médio pré/pós: **%s / %s**.", fmt_num(r2$pre_mean), fmt_num(r2$post_mean)),
    sprintf("- McNemar exato bicaudal: **p=%s**.", fmt_p(r1$exact_mcnemar_p_two_sided)), sprintf("- Wilcoxon exato bicaudal: **p=%s**.", fmt_p(r2$wilcoxon_signed_rank_exact$p_two_sided_exact)), "",
    "## Limitações", "",
    "Documentos externos ao repositório, práticas não versionadas e textos que não correspondem às expressões das regras podem não ser detectados. O score é discreto e concentrado em zero. Os p-valores devem ser interpretados junto com as contagens, evidências e tamanho das diferenças.", "",
    "## Reprodução", "",
    sprintf("A coleta grava um checkpoint JSONL em `%s`. A análise pode ser repetida sem acesso à API usando o mesmo CSV e esse checkpoint. O dataset, as estatísticas e os relatórios são derivados desses arquivos.", project_relative(raw_path)), "",
    sprintf("Versão das regras: `%s`.", RULE_VERSION), sprintf("Linhas no dataset final: **%d**.", nrow(dataset))
  )
  writeLines(enc2utf8(lines), path, useBytes = TRUE)
}

write_manifest <- function(path, stats, input_path = sample_path(), raw_path = raw_checkpoint_path()) {
  manifest <- list(
    generated_at = stats$generated_at,
    project_name = as.character(setting("project", "name", "privacy-documentation-experiment")),
    input_csv = project_relative(input_path),
    input_sha256 = stats$source_sha256,
    input_rows = stats$total_input_rows,
    dataset_rows = stats$total_input_rows,
    complete_pairs = stats$complete_pairs,
    incomplete_pairs = stats$incomplete_pairs,
    significance_level = ALPHA,
    classification_rule_version = RULE_VERSION,
    collector_protocol_version = COLLECTOR_PROTOCOL_VERSION,
    source_collector_protocol_versions = stats$source_collector_protocol_versions,
    wilcoxon_method = "exact-two-sided-average-ranks-zero-differences-excluded",
    sample_requires_osi_approved_license = TRUE,
    selection_topics = as.character(unlist(setting("selection", "topics", character()), use.names = FALSE)),
    selection_audit = if (file.exists(selection_audit_path())) project_relative(selection_audit_path()) else NULL,
    files = c(
      project_relative(input_path), project_relative(raw_path),
      "outputs/tables/sample_used.csv",
      "outputs/tables/final_privacy_gdpr_dataset.csv",
      "outputs/tables/positive_evidence.csv",
      "outputs/tables/statistics_r.csv",
      "outputs/tables/criterion_frequencies.csv",
      "outputs/tables/d1_transition_table.csv",
      "outputs/figures/d1_pre_post.png",
      "outputs/figures/criteria_post_gdpr.png",
      "outputs/metadata/statistics.json",
      "outputs/metadata/sample_used.sha256.txt",
      "outputs/metadata/session_info.txt",
      if (file.exists(selection_audit_path())) project_relative(selection_audit_path()) else character(),
      "outputs/reports/statistical_results.md",
      "outputs/reports/privacy_documentation_experiment_gdpr.md",
      "outputs/metadata/manifest.json"
    )
  )
  jsonlite::write_json(manifest, path, auto_unbox = TRUE, pretty = TRUE, na = "null", digits = 16)
}

write_positive_evidence <- function(path, dataset) {
  values <- list()
  for (index in seq_len(nrow(dataset))) {
    for (period in c("pre", "post")) {
      for (indicator in c("D1", names(RULES))) {
        column <- paste0(period, "_", indicator)
        if (identical(as.character(dataset[[column]][[index]]), "1")) {
          evidence_column <- paste0(column, "_evidence")
          values[[length(values) + 1L]] <- data.frame(
            repository = dataset$repository[[index]], period = period, indicator = indicator,
            evidence = dataset[[evidence_column]][[index]], stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  result <- if (length(values)) do.call(rbind, values) else data.frame(repository = character(), period = character(), indicator = character(), evidence = character())
  write.csv(result, path, row.names = FALSE, fileEncoding = "UTF-8")
}

write_derived_outputs <- function(paths, stats, dataset) {
  complete <- dataset$pre_D1 != "" & dataset$post_D1 != ""
  transition <- table(
    factor(suppressWarnings(as.numeric(dataset$pre_D1[complete])), levels = c(0, 1)),
    factor(suppressWarnings(as.numeric(dataset$post_D1[complete])), levels = c(0, 1))
  )
  colnames(transition) <- c("post_0", "post_1")
  rownames(transition) <- c("pre_0", "pre_1")
  write.csv(as.data.frame.matrix(transition), file.path(paths$tables, "d1_transition_table.csv"), fileEncoding = "UTF-8")

  frequencies <- data.frame(
    criterion = names(RULES),
    pre = vapply(names(RULES), function(code) stats$criterion_frequencies$pre[[code]], numeric(1L)),
    post = vapply(names(RULES), function(code) stats$criterion_frequencies$post[[code]], numeric(1L)),
    stringsAsFactors = FALSE
  )
  write.csv(frequencies, file.path(paths$tables, "criterion_frequencies.csv"), row.names = FALSE, fileEncoding = "UTF-8")

  r1 <- stats$rq1_mcnemar
  r2 <- stats$rq2_wilcoxon
  w <- r2$wilcoxon_signed_rank_exact
  summary <- data.frame(
    indicador = c("linhas_lidas", "pares_completos_D1", "pares_completos_score", "D1_pre", "D1_pos", "pre_1_pos_0", "pre_0_pos_1", "p_McNemar_exato_bicaudal", "media_score_pre", "media_score_pos", "diferenca_media_score", "aumentos_score", "reducoes_score", "empates_score", "diferencas_nao_nulas", "W_mais", "W_menos", "p_Wilcoxon_exato_bicaudal", "correlacao_bisserial"),
    valor = c(stats$total_input_rows, stats$complete_pairs, stats$complete_pairs, r1$pre_ones, r1$post_ones, r1$pre1_post0_b, r1$pre0_post1_c, r1$exact_mcnemar_p_two_sided, r2$pre_mean, r2$post_mean, r2$difference_mean, r2$increased, r2$decreased, r2$unchanged, w$n_nonzero, w$w_plus, w$w_minus, w$p_two_sided_exact, w$rank_biserial),
    stringsAsFactors = FALSE
  )
  write.csv(summary, file.path(paths$tables, "statistics_r.csv"), row.names = FALSE, fileEncoding = "UTF-8")

  png(file.path(paths$figures, "d1_pre_post.png"), width = 1000, height = 650, res = 120)
  barplot(c(r1$pre_ones, r1$post_ones), names.arg = c("Pré-GDPR", "Pós-GDPR"), ylab = "Repositórios com D1 = 1", main = "Presença documental de privacidade", col = c("#f2a36b", "#c84b18"))
  dev.off()
  png(file.path(paths$figures, "criteria_post_gdpr.png"), width = 1000, height = 650, res = 120)
  barplot(frequencies$post, names.arg = frequencies$criterion, ylab = "Repositórios", main = "Critérios documentais no período pós-GDPR", col = "#d85f2a")
  dev.off()
}


ReportWriter <- R6::R6Class(
  "ReportWriter",
  public = list(
    config = NULL,
    store = NULL,
    initialize = function(config, store) {
      self$config <- config
      self$store <- store
    },
    write_all = function(paths, stats, sample, source_hash, dataset, input_path, raw_path) {
      for (directory in paths[-1L]) dir.create(directory, recursive = TRUE, showWarnings = FALSE)
      write.csv(sample, file.path(paths$tables, "sample_used.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      self$store$write_lines(
        c(paste0("sha256  ", source_hash), paste0("source  ", self$config$relative(input_path)), paste0("rows  ", nrow(sample))),
        file.path(paths$metadata, "sample_used.sha256.txt")
      )
      write.csv(dataset, file.path(paths$tables, "final_privacy_gdpr_dataset.csv"), row.names = FALSE, fileEncoding = "UTF-8", na = "")
      write_positive_evidence(file.path(paths$tables, "positive_evidence.csv"), dataset)
      write_derived_outputs(paths, stats, dataset)
      self$store$write_json(stats, file.path(paths$metadata, "statistics.json"), pretty = TRUE)
      write_stats_markdown(file.path(paths$reports, "statistical_results.md"), stats)
      write_report(file.path(paths$reports, "privacy_documentation_experiment_gdpr.md"), stats, sample, source_hash, dataset, input_path, raw_path)
      write_manifest(file.path(paths$metadata, "manifest.json"), stats, input_path, raw_path)
      session_info <- sub("[[:space:]]+$", "", capture.output(sessionInfo()))
      self$store$write_lines(session_info, file.path(paths$metadata, "session_info.txt"))
      invisible(paths)
    }
  )
)

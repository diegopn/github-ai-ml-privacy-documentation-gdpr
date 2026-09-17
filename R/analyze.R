## Achatamento, classificação final e análise estatística.
## Esta etapa não consulta a API: lê o CSV e o checkpoint JSONL, recalcula as
## regras sobre os documentos recuperados e produz os artefatos analisáveis.

script_arg_for_source <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path_for_source <- if (length(script_arg_for_source)) sub("^--file=", "", script_arg_for_source[[1L]]) else file.path("R", "analyze.R")
source(file.path(dirname(script_path_for_source), "common.R"))

# Define os arquivos de entrada e saída usados quando o analisador é executado
# diretamente pelo Rscript.
parse_options <- function(args) {
  values <- list(
    input = file.path("data", "repositorios_selecionados.csv"),
    raw = file.path("data", "raw", "repository_results.jsonl"),
    output = "outputs"
  )
  index <- 1L
  while (index <= length(args)) {
    option <- args[[index]]
    if (option %in% c("--input", "--raw", "--output") && index < length(args)) {
      values[[sub("^--", "", option)]] <- args[[index + 1L]]
      index <- index + 2L
    } else {
      stop("Uso: Rscript R/analyze.R [--input arquivo.csv] [--raw arquivo.jsonl] [--output diretório]")
    }
  }
  values
}

missing_record <- function(row) {
  # Mantém a linha da amostra mesmo quando não existe registro no checkpoint;
  # assim a ausência fica explícita no dataset final.
  list(
    repository = scalar_text(row$repository, ""), input_row = as.list(row),
    accessibility = list(http_status = 0L, accessible = FALSE, metadata = list(), error = "registro não processado"),
    pre = list(), post = list()
  )
}

records_to_dataset <- function(sample, records, source_hash) {
  # Indexa o JSONL e restaura a ordem original da amostra antes de achatar os
  # objetos aninhados em uma linha por repositório.
  by_repository <- index_records(records)
  rows <- lapply(seq_len(nrow(sample)), function(index) {
    row <- sample[index, , drop = FALSE]
    repository <- scalar_text(row$repository, "")
    record <- by_repository[[repository]] %||% missing_record(row)
    flatten_record(row, record, index + 1L, source_hash)
  })
  values <- lapply(dataset_columns(), function(column) {
    vapply(rows, function(row) {
      value <- row[[column]]
      if (is.null(value) || length(value) == 0L) "" else as.character(value[[1L]])
    }, character(1L))
  })
  names(values) <- dataset_columns()
  list(rows = rows, data = as.data.frame(values, stringsAsFactors = FALSE, check.names = FALSE))
}

calculate_statistics <- function(records, dataset, sample) {
  # Os testes usam somente pares completos; contagens e repositórios incompletos
  # continuam no relatório para não desaparecerem silenciosamente.
  complete <- vapply(records, is_complete_record, logical(1L))
  indices <- which(complete)
  # Converte as colunas gravadas como texto no CSV para os vetores numéricos dos testes.
  numeric_column <- function(name) suppressWarnings(as.numeric(dataset[[name]][indices]))
  pre_d1 <- numeric_column("pre_D1")
  post_d1 <- numeric_column("post_D1")
  pre_score <- numeric_column("pre_score")
  post_score <- numeric_column("post_score")
  differences <- post_score - pre_score
  # b e c são as duas transições discordantes exigidas pelo McNemar pareado.
  b <- sum(pre_d1 == 1 & post_d1 == 0, na.rm = TRUE)
  c_value <- sum(pre_d1 == 0 & post_d1 == 1, na.rm = TRUE)
  # O Wilcoxon usa a diferença de score pós menos pré para cada par.
  wilcoxon <- wilcoxon_exact(differences)
  generated_at <- format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
  pre_frequency <- setNames(integer(length(RULES)), names(RULES))
  post_frequency <- pre_frequency
  for (code in names(RULES)) {
    pre_frequency[[code]] <- sum(suppressWarnings(as.numeric(dataset[[paste0("pre_", code)]][indices])) == 1, na.rm = TRUE)
    post_frequency[[code]] <- sum(suppressWarnings(as.numeric(dataset[[paste0("post_", code)]][indices])) == 1, na.rm = TRUE)
  }
  list(
    analysis_population = "pares com as duas versões históricas e árvores Git recuperadas",
    total_input_rows = nrow(sample), complete_pairs = length(indices), incomplete_pairs = nrow(sample) - length(indices),
    incomplete_repositories = as.character(sample$repository[!complete]),
    rq1_mcnemar = list(
      n_complete_pairs = length(indices), pre_ones = sum(pre_d1 == 1, na.rm = TRUE), post_ones = sum(post_d1 == 1, na.rm = TRUE),
      pre_proportion = if (length(indices)) mean(pre_d1 == 1, na.rm = TRUE) else 0,
      post_proportion = if (length(indices)) mean(post_d1 == 1, na.rm = TRUE) else 0,
      pre1_post0_b = b, pre0_post1_c = c_value,
      exact_mcnemar_p_two_sided = exact_mcnemar(b, c_value),
      paired_proportion_difference_post_minus_pre = if (length(indices)) (c_value - b) / length(indices) else 0,
      matched_odds_ratio_c_over_b_haldane = (c_value + 0.5) / (b + 0.5)
    ),
    rq2_wilcoxon = list(
      n_complete_pairs = length(indices),
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
      increased = sum(differences > 0, na.rm = TRUE), decreased = sum(differences < 0, na.rm = TRUE), unchanged = sum(differences == 0, na.rm = TRUE),
      wilcoxon_signed_rank_exact = c(wilcoxon, list(zero_differences_excluded = sum(differences == 0, na.rm = TRUE), ties_use_average_ranks = TRUE))
    ),
    criterion_frequencies = list(pre = as.list(pre_frequency), post = as.list(post_frequency)),
    generated_at = generated_at,
    source_csv = normalizePath(file.path("data", "repositorios_selecionados.csv"), mustWork = FALSE),
    source_sha256 = unique(dataset$sample_source_sha256)[[1L]] %||% "",
    pre_until = PRE_UNTIL, post_until = POST_UNTIL,
    classification_rule_version = RULE_VERSION,
    collector_protocol_version = COLLECTOR_PROTOCOL_VERSION,
    sample_requires_osi_approved_license = TRUE,
    open_source_repositories = nrow(sample)
  )
}

# Formatação centralizada para que tabelas, relatório e console usem os mesmos
# separadores e casas decimais.
fmt_num <- function(value, digits = 3L) {
  if (is.null(value) || length(value) == 0L || is.na(value)) return("NA")
  formatC(as.numeric(value), format = "f", digits = digits, decimal.mark = ".")
}

fmt_pct <- function(value) paste0(fmt_num(as.numeric(value) * 100, 1L), "%")

write_stats_markdown <- function(path, stats) {
  # Relatório legível dos testes, das contagens e das regras de interpretação.
  r1 <- stats$rq1_mcnemar
  r2 <- stats$rq2_wilcoxon
  w <- r2$wilcoxon_signed_rank_exact
  lines <- c(
    "# Resultados estatísticos", "", "## População", "",
    sprintf("- Linhas de entrada: **%d**.", stats$total_input_rows),
    sprintf("- Repositórios com licença SPDX/OSI aprovada: **%d**.", stats$open_source_repositories),
    sprintf("- Pares completos: **%d**.", stats$complete_pairs),
    sprintf("- Pares incompletos preservados e excluídos dos testes: **%d**.", stats$incomplete_pairs),
    sprintf("- Pré: último commit até `%s`.", PRE_UNTIL), sprintf("- Pós: último commit até `%s`.", POST_UNTIL), "",
    "## RQ1: McNemar exato bicaudal para D1", "",
    "| Medida | Resultado |", "|---|---:|",
    sprintf("| Pré D1 = 1 | %d (%s) |", r1$pre_ones, fmt_pct(r1$pre_proportion)),
    sprintf("| Pós D1 = 1 | %d (%s) |", r1$post_ones, fmt_pct(r1$post_proportion)),
    sprintf("| Pré 1 → pós 0 | %d |", r1$pre1_post0_b), sprintf("| Pré 0 → pós 1 | %d |", r1$pre0_post1_c),
    sprintf("| Diferença de proporções pós−pré | %s |", fmt_pct(r1$paired_proportion_difference_post_minus_pre)),
    sprintf("| Odds ratio pareado com correção de Haldane | %s |", fmt_num(r1$matched_odds_ratio_c_over_b_haldane)),
    sprintf("| p exato bicaudal | %s |", fmt_num(r1$exact_mcnemar_p_two_sided, 6L)), "",
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
    sprintf("| W+ / W− | %s / %s |", fmt_num(w$w_plus), fmt_num(w$w_minus)), sprintf("| p bicaudal exato | %s |", fmt_num(w$p_two_sided_exact, 6L)),
    sprintf("| Correlação bisserial de postos | %s |", fmt_num(w$rank_biserial, 6L)), "",
    "As regras foram aplicadas de forma conservadora; menções isoladas não foram consideradas evidência suficiente."
  )
  writeLines(enc2utf8(lines), path, useBytes = TRUE)
}

write_report <- function(path, stats, sample, source_hash, dataset) {
  # Descreve desenho, população, limitações e resultados em linguagem de artigo.
  r1 <- stats$rq1_mcnemar
  r2 <- stats$rq2_wilcoxon
  lines <- c(
    "# Experimento: documentação de privacidade em repositórios públicos de IA/ML após a aplicação da GDPR", "",
    "## Estado da execução", "",
    "Este projeto executa a coleta, a classificação textual e a análise estatística em R. A classificação final usa regras semânticas conservadoras.", "",
    "## Entrada e desenho", "",
    sprintf("- CSV de entrada: `%s`.", normalizePath(file.path("data", "repositorios_selecionados.csv"), mustWork = FALSE)),
    sprintf("- SHA-256 do CSV: `%s`.", source_hash), sprintf("- Linhas da amostra: **%d**.", nrow(sample)),
    "- Critério de licença: **SPDX/OSI aprovada para todos os repositórios**.",
    sprintf("- Versão pré-GDPR: último commit até `%s`.", PRE_UNTIL), sprintf("- Versão pós-GDPR: último commit até `%s`.", POST_UNTIL), "",
    "A comparação é pareada e observacional. O resultado mede evidência documental versionada; não demonstra causalidade da GDPR nem conformidade jurídica.", "",
    "## Classificação", "",
    "O coletor filtra README de raiz e documentos textuais ligados a privacidade, segurança, proteção de dados, termos, jurídico, retenção, consentimento e conformidade. O analisador exclui datasets, testes, fixtures, exemplos, dependências e listas bibliográficas.", "",
    "D1 indica presença de evidência documental contextualizada. O PDE Score soma C1 a C7: dados pessoais, finalidade, base legal, direitos, retenção ou exclusão, compartilhamento e proteção. Um valor zero significa ausência de evidência suficiente nos documentos recuperados, não ausência comprovada de práticas de privacidade.", "",
    "## Resultados", "",
    sprintf("- D1 pré: **%d/%d** (%s).", r1$pre_ones, stats$complete_pairs, fmt_pct(r1$pre_proportion)),
    sprintf("- D1 pós: **%d/%d** (%s).", r1$post_ones, stats$complete_pairs, fmt_pct(r1$post_proportion)),
    sprintf("- Pares completos: **%d**.", stats$complete_pairs), sprintf("- Score médio pré/pós: **%s / %s**.", fmt_num(r2$pre_mean), fmt_num(r2$post_mean)),
    sprintf("- McNemar exato bicaudal: **p=%s**.", fmt_num(r1$exact_mcnemar_p_two_sided, 6L)), sprintf("- Wilcoxon exato bicaudal: **p=%s**.", fmt_num(r2$wilcoxon_signed_rank_exact$p_two_sided_exact, 6L)), "",
    "## Limitações", "",
    "Documentos externos ao repositório, práticas não versionadas e textos que não correspondem às expressões das regras podem não ser detectados. O score é discreto e concentrado em zero. Os p-valores devem ser interpretados junto com as contagens, evidências e tamanho das diferenças.", "",
    "## Reprodução", "",
    "A coleta grava um checkpoint JSONL em `data/raw/repository_results.jsonl`. A análise pode ser repetida sem acesso à API usando o mesmo CSV e esse checkpoint. O dataset, as estatísticas e os relatórios são derivados desses arquivos.", "",
    sprintf("Versão das regras: `%s`.", RULE_VERSION), sprintf("Linhas no dataset final: **%d**.", nrow(dataset))
  )
  writeLines(enc2utf8(lines), path, useBytes = TRUE)
}

write_manifest <- function(path, stats) {
  # Registra hashes, versões e nomes dos artefatos para reconstruir a execução.
  manifest <- list(
    generated_at = stats$generated_at,
    input_csv = stats$source_csv,
    input_sha256 = stats$source_sha256,
    input_rows = stats$total_input_rows,
    dataset_rows = stats$total_input_rows,
    complete_pairs = stats$complete_pairs,
    incomplete_pairs = stats$incomplete_pairs,
    classification_rule_version = RULE_VERSION,
    collector_protocol_version = COLLECTOR_PROTOCOL_VERSION,
    wilcoxon_method = "exact-two-sided-average-ranks-zero-differences-excluded",
    sample_requires_osi_approved_license = TRUE,
    files = c("amostra_utilizada.csv", "amostra_utilizada.sha256.txt", "dataset_final_privacidade_gdpr.csv", "evidencias_positivas.csv", "estatisticas.json", "resultados_estatisticos.md", "experimento_privacidade_gdpr.md", "../data/raw/repository_results.jsonl")
  )
  jsonlite::write_json(manifest, path, auto_unbox = TRUE, pretty = TRUE, na = "null", digits = 16)
}

write_positive_evidence <- function(path, dataset) {
  # Exporta cada evidência positiva em formato tabular para revisão manual.
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

write_derived_outputs <- function(output_dir, stats, dataset) {
  # Gera tabelas, RDS e gráficos derivados sem novas chamadas à API.
  complete <- dataset$pre_D1 != "" & dataset$post_D1 != ""
  transition <- table(
    factor(suppressWarnings(as.numeric(dataset$pre_D1[complete])), levels = c(0, 1)),
    factor(suppressWarnings(as.numeric(dataset$post_D1[complete])), levels = c(0, 1))
  )
  colnames(transition) <- c("post_0", "post_1")
  rownames(transition) <- c("pre_0", "pre_1")
  write.csv(as.data.frame.matrix(transition), file.path(output_dir, "tabela_transicao_D1.csv"), fileEncoding = "UTF-8")

  frequencies <- data.frame(
    criterion = names(RULES),
    pre = vapply(names(RULES), function(code) stats$criterion_frequencies$pre[[code]], numeric(1L)),
    post = vapply(names(RULES), function(code) stats$criterion_frequencies$post[[code]], numeric(1L)),
    stringsAsFactors = FALSE
  )
  write.csv(frequencies, file.path(output_dir, "frequencia_criterios.csv"), row.names = FALSE, fileEncoding = "UTF-8")

  r1 <- stats$rq1_mcnemar
  r2 <- stats$rq2_wilcoxon
  w <- r2$wilcoxon_signed_rank_exact
  summary <- data.frame(
    indicador = c("linhas_lidas", "pares_completos_D1", "pares_completos_score", "D1_pre", "D1_pos", "pre_1_pos_0", "pre_0_pos_1", "p_McNemar_exato_bicaudal", "media_score_pre", "media_score_pos", "diferenca_media_score", "aumentos_score", "reducoes_score", "empates_score", "diferencas_nao_nulas", "W_mais", "W_menos", "p_Wilcoxon_exato_bicaudal", "correlacao_bisserial"),
    valor = c(stats$total_input_rows, stats$complete_pairs, stats$complete_pairs, r1$pre_ones, r1$post_ones, r1$pre1_post0_b, r1$pre0_post1_c, r1$exact_mcnemar_p_two_sided, r2$pre_mean, r2$post_mean, r2$difference_mean, r2$increased, r2$decreased, r2$unchanged, w$n_nonzero, w$w_plus, w$w_minus, w$p_two_sided_exact, w$rank_biserial),
    stringsAsFactors = FALSE
  )
  write.csv(summary, file.path(output_dir, "estatisticas_r.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  saveRDS(stats, file.path(output_dir, "estatisticas_r.rds"))

  png(file.path(output_dir, "d1_pre_pos.png"), width = 1000, height = 650, res = 120)
  barplot(c(r1$pre_ones, r1$post_ones), names.arg = c("Pré-GDPR", "Pós-GDPR"), ylab = "Repositórios com D1 = 1", main = "Presença documental de privacidade", col = "grey35")
  dev.off()
  png(file.path(output_dir, "criterios_pos_gdpr.png"), width = 1000, height = 650, res = 120)
  barplot(frequencies$post, names.arg = frequencies$criterion, ylab = "Repositórios", main = "Critérios documentais no período pós-GDPR", col = "grey45")
  dev.off()
}

run_analysis <- function(options) {
  # Pipeline offline: valida entradas, achata o checkpoint, calcula estatísticas
  # e grava todos os arquivos de reprodução no diretório de saída.
  dir.create(options$output, recursive = TRUE, showWarnings = FALSE)
  sample <- read_sample(options$input)
  validate_open_source_sample(sample)
  source_hash <- sha256_file(options$input)
  records <- read_jsonl(options$raw)
  flattened <- records_to_dataset(sample, records, source_hash)
  dataset <- flattened$data
  write.csv(sample, file.path(options$output, "amostra_utilizada.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  writeLines(c(paste0("sha256  ", source_hash), paste0("source  ", normalizePath(options$input, mustWork = FALSE)), paste0("rows  ", nrow(sample))), file.path(options$output, "amostra_utilizada.sha256.txt"), useBytes = TRUE)
  write.csv(dataset, file.path(options$output, "dataset_final_privacidade_gdpr.csv"), row.names = FALSE, fileEncoding = "UTF-8", na = "")
  write_positive_evidence(file.path(options$output, "evidencias_positivas.csv"), dataset)
  # O dataset é salvo antes das estatísticas para que mesmo uma falha posterior
  # deixe disponível a tabela principal da execução.
  stats <- calculate_statistics(records, dataset, sample)
  write_derived_outputs(options$output, stats, dataset)
  jsonlite::write_json(stats, file.path(options$output, "estatisticas.json"), auto_unbox = TRUE, pretty = TRUE, na = "null", digits = 16)
  write_stats_markdown(file.path(options$output, "resultados_estatisticos.md"), stats)
  write_report(file.path(options$output, "experimento_privacidade_gdpr.md"), stats, sample, source_hash, dataset)
  write_manifest(file.path(options$output, "manifest.json"), stats)
  writeLines(capture.output(sessionInfo()), file.path(options$output, "session_info.txt"), useBytes = TRUE)
  cat(sprintf("Linhas lidas: %d\nPares completos: %d\nMcNemar exato: p = %.15g\nWilcoxon exato: p = %.15g\n", nrow(sample), stats$complete_pairs, stats$rq1_mcnemar$exact_mcnemar_p_two_sided, stats$rq2_wilcoxon$wilcoxon_signed_rank_exact$p_two_sided_exact))
  invisible(stats)
}

# Evita executar a análise quando este arquivo é apenas carregado por um wrapper.
if (sys.nframe() == 0L) {
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  project_dir <- if (length(script_arg)) normalizePath(file.path(dirname(sub("^--file=", "", script_arg[[1L]])), ".."), mustWork = TRUE) else getwd()
  setwd(project_dir)
  run_analysis(parse_options(commandArgs(trailingOnly = TRUE)))
}

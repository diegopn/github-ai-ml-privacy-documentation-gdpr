## Análise estatística do dataset de documentação de privacidade.
## O script usa somente R base e não altera o arquivo de entrada.

options(stringsAsFactors = FALSE, encoding = "UTF-8")

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg) == 1L) {
  sub("^--file=", "", script_arg)
} else {
  file.path("R", "analysis.R")
}
project_dir <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
data_path <- file.path(project_dir, "data", "dataset_final_privacidade_gdpr.csv")
output_dir <- file.path(project_dir, "outputs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

required_columns <- c(
  "pre_D1", "post_D1", "pre_score", "post_score",
  paste0("pre_C", 1:7), paste0("post_C", 1:7)
)

to_numeric <- function(x, column_name) {
  value <- suppressWarnings(as.numeric(as.character(x)))
  if (any(is.na(value) & !is.na(x))) {
    stop(sprintf("A coluna '%s' possui valor não numérico.", column_name))
  }
  value
}

if (!file.exists(data_path)) stop(sprintf("Dataset não encontrado: %s", data_path))
dataset <- read.csv(
  data_path,
  header = TRUE,
  sep = ",",
  quote = "\"",
  na.strings = c("", "NA"),
  fileEncoding = "UTF-8",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

missing_columns <- setdiff(required_columns, names(dataset))
if (length(missing_columns) > 0L) {
  stop(sprintf("Colunas ausentes: %s", paste(missing_columns, collapse = ", ")))
}

pre_d1 <- to_numeric(dataset$pre_D1, "pre_D1")
post_d1 <- to_numeric(dataset$post_D1, "post_D1")
pre_score <- to_numeric(dataset$pre_score, "pre_score")
post_score <- to_numeric(dataset$post_score, "post_score")

if (any(!is.na(pre_d1) & !pre_d1 %in% c(0, 1)) ||
    any(!is.na(post_d1) & !post_d1 %in% c(0, 1))) {
  stop("D1 deve conter apenas 0 e 1.")
}
if (any(!is.na(pre_score) & (pre_score < 0 | pre_score > 7 | pre_score != floor(pre_score))) ||
    any(!is.na(post_score) & (post_score < 0 | post_score > 7 | post_score != floor(post_score)))) {
  stop("Os scores devem ser inteiros entre 0 e 7.")
}

complete_d1 <- complete.cases(pre_d1, post_d1)
complete_score <- complete.cases(pre_score, post_score)
if (!all(complete_d1) || !all(complete_score)) {
  warning("Existem pares incompletos; cada teste usará seus pares completos.")
}

## RQ1: tabela pareada e McNemar exato.
d1_table <- table(
  factor(pre_d1[complete_d1], levels = c(0, 1)),
  factor(post_d1[complete_d1], levels = c(0, 1)),
  dnn = c("pre_D1", "post_D1")
)
colnames(d1_table) <- c("post_0", "post_1")
rownames(d1_table) <- c("pre_0", "pre_1")
write.csv(
  as.data.frame.matrix(d1_table),
  file.path(output_dir, "tabela_transicao_D1.csv"),
  row.names = TRUE,
  fileEncoding = "UTF-8"
)

b <- unname(d1_table["pre_1", "post_0"])
c <- unname(d1_table["pre_0", "post_1"])
n_discordant <- b + c
p_mcnemar <- if (n_discordant == 0L) {
  1
} else {
  binom.test(min(b, c), n_discordant, p = 0.5, alternative = "two.sided")$p.value
}

## RQ2: score pareado e Wilcoxon exato por enumeração dos sinais.
score_difference <- post_score[complete_score] - pre_score[complete_score]
nonzero_difference <- score_difference[score_difference != 0]
n_nonzero <- length(nonzero_difference)

if (n_nonzero == 0L) {
  ranks <- numeric(0)
  w_plus <- 0
  w_minus <- 0
  p_wilcoxon <- 1
} else {
  ranks <- rank(abs(nonzero_difference), ties.method = "average")
  w_plus <- sum(ranks[nonzero_difference > 0])
  w_minus <- sum(ranks[nonzero_difference < 0])

  if (n_nonzero > 20L) {
    stop("A enumeração exata está limitada a 20 diferenças não nulas.")
  }

  sign_grid <- expand.grid(rep(list(c(-1, 1)), n_nonzero))
  sign_matrix <- as.matrix(sign_grid)
  total_rank <- sum(ranks)
  w_plus_distribution <- (total_rank + as.numeric(sign_matrix %*% ranks)) / 2
  observed_smallest_rank_sum <- min(w_plus, w_minus)
  smallest_rank_sum_distribution <- pmin(
    w_plus_distribution,
    total_rank - w_plus_distribution
  )
  p_wilcoxon <- mean(
    smallest_rank_sum_distribution <= observed_smallest_rank_sum + 1e-12
  )
}

rank_biserial <- if ((w_plus + w_minus) == 0) {
  0
} else {
  (w_plus - w_minus) / (w_plus + w_minus)
}

criterion_frequency <- data.frame(
  criterion = paste0("C", 1:7),
  pre = vapply(paste0("pre_C", 1:7), function(column) {
    sum(to_numeric(dataset[[column]], column) == 1, na.rm = TRUE)
  }, numeric(1)),
  post = vapply(paste0("post_C", 1:7), function(column) {
    sum(to_numeric(dataset[[column]], column) == 1, na.rm = TRUE)
  }, numeric(1))
)
write.csv(
  criterion_frequency,
  file.path(output_dir, "frequencia_criterios.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

## Estatísticas descritivas.
score_stats <- data.frame(
  indicador = c(
    "linhas_lidas", "pares_completos_D1", "pares_completos_score",
    "D1_pre", "D1_pos", "pre_1_pos_0", "pre_0_pos_1",
    "p_McNemar_exato_bicaudal", "media_score_pre", "media_score_pos",
    "diferenca_media_score", "aumentos_score", "reducoes_score",
    "empates_score", "diferencas_nao_nulas", "W_mais", "W_menos",
    "p_Wilcoxon_exato_bicaudal", "correlacao_bisserial"
  ),
  valor = c(
    nrow(dataset), sum(complete_d1), sum(complete_score),
    sum(pre_d1[complete_d1] == 1), sum(post_d1[complete_d1] == 1), b, c,
    p_mcnemar, mean(pre_score[complete_score]), mean(post_score[complete_score]),
    mean(score_difference), sum(score_difference > 0), sum(score_difference < 0),
    sum(score_difference == 0), n_nonzero, w_plus, w_minus,
    p_wilcoxon, rank_biserial
  )
)
write.csv(
  score_stats,
  file.path(output_dir, "estatisticas_r.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
saveRDS(
  list(
    dataset_rows = nrow(dataset),
    complete_d1 = sum(complete_d1),
    complete_score = sum(complete_score),
    d1_table = d1_table,
    p_mcnemar = p_mcnemar,
    score_difference = score_difference,
    ranks = ranks,
    w_plus = w_plus,
    w_minus = w_minus,
    p_wilcoxon = p_wilcoxon,
    rank_biserial = rank_biserial,
    criterion_frequency = criterion_frequency
  ),
  file.path(output_dir, "estatisticas_r.rds")
)

## Gráficos simples, sem dependências externas.
png(file.path(output_dir, "d1_pre_pos.png"), width = 1000, height = 650, res = 120)
barplot(
  c(sum(pre_d1[complete_d1] == 1), sum(post_d1[complete_d1] == 1)),
  names.arg = c("Pré-GDPR", "Pós-GDPR"),
  ylab = "Repositórios com D1 = 1",
  main = "Presença documental de privacidade",
  col = "grey35"
)
dev.off()

png(file.path(output_dir, "criterios_pos_gdpr.png"), width = 1000, height = 650, res = 120)
barplot(
  criterion_frequency$post,
  names.arg = criterion_frequency$criterion,
  ylab = "Repositórios",
  main = "Critérios documentais no período pós-GDPR",
  col = "grey45"
)
dev.off()

report_lines <- c(
  "# Análise estatística",
  "",
  sprintf("Linhas lidas: %d", nrow(dataset)),
  sprintf("Pares completos para D1: %d", sum(complete_d1)),
  sprintf("Pares completos para o score: %d", sum(complete_score)),
  "",
  "## RQ1",
  sprintf("D1 pré = %d; D1 pós = %d.", sum(pre_d1[complete_d1] == 1), sum(post_d1[complete_d1] == 1)),
  sprintf("Pares discordantes: pré 1 → pós 0 = %d; pré 0 → pós 1 = %d.", b, c),
  sprintf("McNemar exato bicaudal: p = %.15g.", p_mcnemar),
  "",
  "## RQ2",
  sprintf("Média do score: pré = %.15g; pós = %.15g.", mean(pre_score[complete_score]), mean(post_score[complete_score])),
  sprintf("Aumentos = %d; reduções = %d; empates = %d.", sum(score_difference > 0), sum(score_difference < 0), sum(score_difference == 0)),
  sprintf("Wilcoxon exato bicaudal: W+ = %.15g; W- = %.15g; p = %.15g.", w_plus, w_minus, p_wilcoxon),
  sprintf("Correlação bisserial de postos = %.15g.", rank_biserial),
  "",
  "As diferenças zero foram excluídas dos postos e os empates nos valores absolutos receberam postos médios."
)
writeLines(report_lines, file.path(output_dir, "relatorio_analise.md"), useBytes = TRUE)
writeLines(capture.output(sessionInfo()), file.path(output_dir, "session_info.txt"), useBytes = TRUE)

cat(paste(report_lines, collapse = "\n"), "\n")

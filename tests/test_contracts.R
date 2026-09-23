test_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
test_path <- sub("^--file=", "", test_file[[1L]])
project_root <- normalizePath(file.path(dirname(test_path), ".."), mustWork = TRUE)
setwd(project_root)

source(file.path(project_root, "functions", "common.R"))
source(file.path(project_root, "functions", "cli.R"))
source(file.path(project_root, "functions", "select_sample.R"))

passed <- 0L
check <- function(label, condition) {
  if (!isTRUE(condition)) stop(sprintf("Falha no teste: %s", label), call. = FALSE)
  passed <<- passed + 1L
  cat(sprintf("[OK] %s\n", label))
}

errors <- function(expression) inherits(tryCatch(force(expression), error = identity), "error")

check("modo padrão executa --run", identical(parse_main_options(character())$mode, "run"))
check("--help é reconhecido", isTRUE(parse_main_options("--help")$help))
check("--run percorre as quatro etapas", identical(pipeline_stages("run"), c("selection", "collection", "analysis", "site")))
check("--select inclui seleção e coleta", identical(pipeline_stages("select"), c("selection", "collection")))
check("--analyze verifica coleta antes da análise e do site", identical(pipeline_stages("analyze"), c("collection_check", "analysis", "site")))
check("a grafia correta --analyze é aceita", identical(parse_main_options("--analyze")$mode, "analyze"))
check("a grafia incorreta --analyize é rejeitada", errors(parse_main_options("--analyize")))
check("modos antigos são rejeitados", errors(parse_main_options("--collect")) && errors(parse_main_options("--validate")))
check("opções de modo conflitantes são rejeitadas", errors(parse_main_options(c("--run", "--select"))))
check("opções auxiliares removidas são rejeitadas", errors(parse_main_options(c("--run", "--workers", "2"))) && errors(parse_main_options(c("--run", "--skip-site"))))
check("argumentos desconhecidos são rejeitados", errors(parse_main_options("--resume")))

query <- selection_repository_query("machine-learning", "2018-01-01", "2018-05-24")
required_qualifiers <- c("is:public", "stars:>=500", "fork:false", "archived:false", "created:2018-01-01..2018-05-24")
check("consulta mantém visibilidade, estrelas, fork, arquivo e janela de criação",
      all(vapply(required_qualifiers, grepl, logical(1L), x = query, fixed = TRUE)))

candidate <- list(repository = list(
  full_name = "sample/project", visibility = "public", private = FALSE,
  fork = FALSE, archived = FALSE, created_at = "2018-05-24T23:59:59Z",
  stargazers_count = 500L, license = list(spdx_id = "MIT")
))
check("repositório nos limites mínimos passa nos filtros estáticos", candidate_passes_static_filters(candidate, "MIT"))
fork <- candidate
fork$repository$fork <- TRUE
check("fork continua excluído", !candidate_passes_static_filters(fork, "MIT"))
few_stars <- candidate
few_stars$repository$stargazers_count <- 499L
check("menos de 500 estrelas continua excluído", !candidate_passes_static_filters(few_stars, "MIT"))
late_repository <- candidate
late_repository$repository$created_at <- "2018-05-25T00:00:00Z"
check("repositório criado no corte GDPR continua excluído", !candidate_passes_static_filters(late_repository, "MIT"))

check("limite primário do GitHub é reconhecido",
      api_response_is_rate_limited(403L, '{"message":"API rate limit exceeded"}', list(github_remaining = "0")))
check("limite secundário HTTP 429 é reconhecido", api_response_is_rate_limited(429L, "", list()))
check("erro de permissão não é confundido com rate limit",
      !api_response_is_rate_limited(403L, '{"message":"Resource not accessible by integration"}', list(github_remaining = "10")))
check("erros de limite identificados interrompem a coleta", api_error_is_rate_limited("Limite da API do GitHub ainda bloqueado"))
check("erros comuns de acesso não são tratados como limite", !api_error_is_rate_limited("HTTP 403: Resource not accessible"))
check("Retry-After determina a espera", identical(api_retry_delay(403L, list(retry_after = "17"), 1L, TRUE), 17))

check("McNemar exato mantém resultado conhecido", abs(exact_mcnemar(1, 4) - 0.375) < 1e-12)
wilcoxon <- wilcoxon_exact(c(1, 2, 3))
check("Wilcoxon exato mantém resultado conhecido", abs(wilcoxon$p_two_sided_exact - 0.25) < 1e-12)

cat(sprintf("\n%d verificações locais passaram; nenhum pedido foi feito à API.\n", passed))

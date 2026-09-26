test_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
test_path <- sub("^--file=", "", test_file[[1L]])
project_root <- normalizePath(file.path(dirname(test_path), ".."), mustWork = TRUE)
setwd(project_root)

source(file.path(project_root, "src", "bootstrap.R"))
bootstrap_project(project_root)
set_github_client(GitHubClient$new(PROJECT_CONFIG))

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
request_files <- list(body = "body", status = "status", error = "error", headers = "headers")
get_arguments <- curl_request_arguments("https://example.test", "", "GET", NULL, 10, 20, request_files)
post_arguments <- curl_request_arguments("https://example.test", "", "POST", "{}", 10, 20, request_files)
check("requisições GET não adicionam corpo ou método explícito", !any(get_arguments %in% c("--request", "--data-raw")))
post_body_index <- match("--data-raw", post_arguments) + 1L
check("requisições POST incluem método e corpo",
      all(c("--request", "POST", "--data-raw") %in% post_arguments) &&
        identical(post_arguments[[post_body_index]], shQuote("{}")))
check("métodos HTTP não suportados são rejeitados", errors(curl_request_arguments("https://example.test", "", "PATCH", NULL, 10, 20, request_files)))
parsed_http_response <- http_response(200L, '{"ok":true}', "", list(), TRUE, character(), TRUE)
check("resposta HTTP preserva o parsing JSON e os headers opcionais",
      isTRUE(parsed_http_response$body$ok) && identical(parsed_http_response$headers, character()))
error_http_response <- http_response(403L, '{"message":"denied"}', "", list(), FALSE, character(), FALSE)
check("resposta HTTP preserva o corpo e o detalhe do erro", identical(error_http_response$body, '{"message":"denied"}') && identical(error_http_response$error, "HTTP 403: denied"))

check("McNemar exato mantém resultado conhecido", abs(exact_mcnemar(1, 4) - 0.375) < 1e-12)
wilcoxon <- wilcoxon_exact(c(1, 2, 3))
check("Wilcoxon exato mantém resultado conhecido", abs(wilcoxon$p_two_sided_exact - 0.25) < 1e-12)

classification <- function(d1, score) {
  criteria <- as.list(setNames(rep(0L, length(RULES)), names(RULES)))
  evidence <- as.list(setNames(rep("", length(RULES)), names(RULES)))
  c(criteria, list(D1 = as.integer(d1), score = as.integer(score), D1_evidence = "", note = "", evidence = evidence))
}
incomplete_record <- list(
  repository = "owner/incomplete",
  pre = list(commit = list(sha = ""), tree_request_status = 0L, classification = classification(0L, 0L)),
  post = list(commit = list(sha = ""), tree_request_status = 0L, classification = classification(0L, 0L))
)
complete_record <- list(
  repository = "owner/complete",
  pre = list(commit = list(sha = "pre-sha"), tree_request_status = 200L, classification = classification(1L, 1L)),
  post = list(commit = list(sha = "post-sha"), tree_request_status = 200L, classification = classification(1L, 2L))
)
alignment_sample <- data.frame(repository = c("owner/incomplete", "owner/complete"), stringsAsFactors = FALSE)
alignment_records <- list(complete_record, incomplete_record)
alignment_dataset <- DatasetBuilder$new(PROJECT_CONFIG, PrivacyDocumentClassifier$new())$build(alignment_sample, alignment_records, "test-sha")$data
alignment_stats <- calculate_statistics(
  alignment_records, alignment_dataset, alignment_sample,
  input_path = "inputs/final/selected_repositories.csv"
)
check("estatísticas alinham registros com a amostra por repositório",
      identical(alignment_stats$incomplete_repositories, "owner/incomplete") &&
        alignment_stats$rq1_mcnemar$pre_ones == 1L && alignment_stats$rq1_mcnemar$post_ones == 1L)
check("p-valores pequenos são exibidos em notação científica", grepl("e-", fmt_p(2.160668e-7), fixed = TRUE))
check("p-valores não nulos não são arredondados para zero", fmt_p(0) == "<5e-324")


mock_client <- R6::R6Class("MockGitHubClient", public = list(
  api = function(path, params = list(), token = "", return_headers = FALSE) {
    older <- identical(as.integer(params$page %||% 1L), 2L)
    date <- if (older) "2015-01-01T00:00:00Z" else "2018-05-24T12:00:00Z"
    body <- list(list(sha = if (older) "old" else "new", commit = list(
      author = list(date = date), committer = list(date = date)
    )))
    headers <- if (isTRUE(return_headers)) 'Link: <https://api.github.com/repos/sample/project/commits?page=2>; rel="last"' else character()
    list(status = 200L, body = body, error = "", rate = list(), headers = headers)
  }
))$new()
set_github_client(mock_client)
selected_record <- evaluate_candidate(candidate, "MIT", token = "", issue_count = 100L)
check("seletor grava created_at do repositório sem variável indefinida",
      identical(selected_record$created_at, "2018-05-24T23:59:59Z"))
check("componentes R6 principais foram compostos", all(vapply(
  c("ProjectConfig", "ArtifactStore", "GitHubClient", "SampleSelector", "RepositoryCollector",
    "PrivacyDocumentClassifier", "DatasetBuilder", "ExperimentAnalyzer", "ReportWriter", "ExperimentRunner"),
  exists, logical(1L), inherits = TRUE
)))
check("amostra publicada passa validação por hash e manifesto persistentes",
      nrow(ArtifactStore$new(PROJECT_CONFIG)$assert_published_sample()) == 474L)

cat(sprintf("\n%d verificações locais passaram; os testes usam cliente GitHub simulado.\n", passed))

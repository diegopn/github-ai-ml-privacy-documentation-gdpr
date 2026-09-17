## Seleciona a amostra inicial por tópicos e critérios do protocolo.
## A execução completa consulta a API; o CSV versionado mantém a amostra usada
## no artigo estável mesmo quando os metadados do GitHub mudam.

script_arg_for_source <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path_for_source <- if (length(script_arg_for_source)) sub("^--file=", "", script_arg_for_source[[1L]]) else file.path("functions", "select_sample.R")
common_candidates <- unique(c(
  file.path(dirname(script_path_for_source), "common.R"),
  file.path(dirname(script_path_for_source), "..", "functions", "common.R"),
  file.path("functions", "common.R")
))
common_path <- common_candidates[file.exists(common_candidates)][[1L]]
if (is.na(common_path) || !nzchar(common_path)) stop("functions/common.R não encontrado.")
source(common_path)

SELECTION_GDPR_DATE <- as.character(setting("selection", "gdpr_date", "2018-05-25T00:00:00Z"))
SELECTION_TOPICS <- unlist(setting("selection", "topics", c("artificial-intelligence", "deep-learning", "machine-learning")), use.names = FALSE)
SELECTION_MIN_STARS <- as.integer(setting("selection", "min_stars", 500L))
SELECTION_MIN_ISSUES <- as.integer(setting("selection", "min_issues", 100L))
SELECTION_MIN_ACTIVITY_MONTHS <- as.numeric(setting("selection", "min_activity_months", 24))
SELECTION_SEARCH_PAGE_SIZE <- 100L
SELECTION_MAX_SEARCH_PAGES <- 10L
SELECTION_SEARCH_INTERVAL <- 2.2

# Interpreta os argumentos que selecionam um novo conjunto de repositórios ou
# revalidam um CSV existente quanto à visibilidade e à licença atual.
parse_selection_options <- function(args) {
  values <- list(input = "", output = sample_path())
  index <- 1L
  while (index <= length(args)) {
    option <- args[[index]]
    if (option %in% c("--input", "--output") && index < length(args)) {
      values[[sub("^--", "", option)]] <- args[[index + 1L]]
      index <- index + 2L
    } else {
      stop("Uso: Rscript scripts/select_sample.R [--input arquivo.csv] [--output arquivo.csv]")
    }
  }
  values
}

# Lê a lista versionada de identificadores SPDX aprovados pela OSI e remove
# comentários e linhas vazias antes de ela ser usada nos filtros.
read_approved_spdx <- function(path = reference_spdx_path()) {
  if (!file.exists(path)) stop(sprintf("Lista SPDX/OSI não encontrada: %s", path))
  values <- trimws(readLines(path, encoding = "UTF-8", warn = FALSE))
  values <- values[nzchar(values) & !startsWith(values, "#")]
  if (!length(values)) stop("A lista SPDX/OSI está vazia.")
  unique(values)
}

# Converte datas da API do GitHub para POSIXct em UTC, aceitando os formatos
# completos e alternativos retornados pelos endpoints.
selection_time <- function(value) {
  value <- scalar_text(value, "")
  if (!nzchar(value)) return(as.POSIXct(NA, tz = "UTC"))
  parsed <- suppressWarnings(as.POSIXct(value, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"))
  if (is.na(parsed)) parsed <- suppressWarnings(as.POSIXct(value, tz = "UTC"))
  parsed
}

# Formata uma data POSIXct em ISO 8601 UTC para persistência no CSV da amostra.
format_selection_time <- function(value) {
  if (length(value) == 0L || is.na(value)) "" else format(value, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

# Executa uma chamada da API usada pela seleção e interrompe a seleção quando
# o GitHub devolve um status fora da faixa de sucesso.
selection_api <- function(path, params, token, return_headers = FALSE) {
  response <- github_api(path, params, token, return_headers = return_headers)
  if (response$status < 200L || response$status >= 300L) {
    stop(if (nzchar(response$error)) response$error else sprintf("GitHub retornou HTTP %d.", response$status))
  }
  response
}

# Obtém o número da última página a partir do cabeçalho Link da API, permitindo
# consultar o commit mais antigo sem baixar toda a história.
last_page_from_headers <- function(headers) {
  link <- grep("^Link:", headers, value = TRUE, ignore.case = TRUE)
  if (!length(link)) return(1L)
  match <- regexec("[?&]page=([0-9]+)[^>]*>;[[:space:]]*rel=\"last\"", link[[length(link)]], perl = TRUE)
  captured <- regmatches(link[[length(link)]], match)[[1L]]
  if (length(captured) >= 2L) as.integer(captured[[2L]]) else 1L
}

# Extrai a data do committer, usando a data do author como fallback quando
# o payload do commit não traz a primeira informação.
commit_date <- function(commit) {
  details <- commit$commit %||% list()
  committer <- details$committer %||% list()
  author <- details$author %||% list()
  selection_time(scalar_text(committer$date, scalar_text(author$date, "")))
}

# Monta o endpoint de commits de um repositório específico.
commits_path <- function(repository) paste0("/repos/", repository, "/commits")

# Consulta a contagem de issues reais, separada de pull requests, para aplicar
# o limiar mínimo definido pelo protocolo de seleção.
count_real_issues <- function(repository, token) {
  response <- selection_api(
    "/search/issues",
    list(q = paste0("repo:", repository, " type:issue"), per_page = 1L),
    token
  )
  scalar_int(response$body$total_count, 0L)
}

# Recupera o primeiro e o último commit acessíveis e calcula a duração total
# da atividade do repositório em meses.
commit_activity <- function(repository, token) {
  path <- commits_path(repository)
  newest <- selection_api(path, list(per_page = 1L), token, return_headers = TRUE)
  values <- newest$body
  if (!is.list(values) || !length(values)) return(NULL)
  last_commit <- commit_date(values[[1L]])
  if (is.na(last_commit)) return(NULL)
  last_page <- last_page_from_headers(newest$headers %||% character())
  oldest <- if (last_page > 1L) selection_api(path, list(per_page = 1L, page = last_page), token)$body else values
  if (!is.list(oldest) || !length(oldest)) return(NULL)
  first_commit <- commit_date(oldest[[1L]])
  if (is.na(first_commit)) return(NULL)
  days <- as.numeric(difftime(last_commit, first_commit, units = "days"))
  list(
    first = first_commit,
    last = last_commit,
    months = max(0, days / 30.44)
  )
}

# Verifica se existe ao menos um commit antes e outro depois da data da GDPR,
# condição necessária para a comparação histórica pareada.
activity_window <- function(repository, token) {
  path <- commits_path(repository)
  pre <- selection_api(path, list(until = SELECTION_GDPR_DATE, per_page = 1L), token)$body
  post <- selection_api(path, list(since = SELECTION_GDPR_DATE, per_page = 1L), token)$body
  list(
    pre = is.list(pre) && length(pre) > 0L,
    post = is.list(post) && length(post) > 0L
  )
}

# Busca repositórios públicos de um tópico, aplicando os filtros básicos e
# paginação limitada para evitar depender de resultados além do limite da API.
search_by_topic <- function(topic, token) {
  query <- paste0(
    "is:public topic:", topic,
    " stars:>=", SELECTION_MIN_STARS,
    " created:<2018-05-25 fork:false archived:false"
  )
  repositories <- list()
  capped <- FALSE
  for (page in seq_len(SELECTION_MAX_SEARCH_PAGES)) {
    if (length(repositories) > 0L) Sys.sleep(SELECTION_SEARCH_INTERVAL)
    response <- selection_api(
      "/search/repositories",
      list(q = query, sort = "stars", order = "desc", per_page = SELECTION_SEARCH_PAGE_SIZE, page = page),
      token
    )
    items <- response$body$items %||% list()
    if (!is.list(items) || !length(items)) break
    repositories <- c(repositories, items)
    if (length(items) < SELECTION_SEARCH_PAGE_SIZE) break
    if (page == SELECTION_MAX_SEARCH_PAGES) capped <- TRUE
  }
  list(repositories = repositories, capped = capped)
}

# Combina os resultados dos tópicos, remove duplicatas e conserva todos os
# tópicos que justificaram a entrada de cada repositório como candidato.
search_candidates <- function(token) {
  candidates <- list()
  names_in_order <- character()
  capped <- FALSE
  for (topic in SELECTION_TOPICS) {
    result <- search_by_topic(topic, token)
    capped <- capped || isTRUE(result$capped)
    for (repository in result$repositories) {
      full_name <- scalar_text(repository$full_name, "")
      if (!nzchar(full_name)) next
      if (is.null(candidates[[full_name]])) {
        candidates[[full_name]] <- list(repository = repository, topics = topic)
        names_in_order <- c(names_in_order, full_name)
      } else {
        candidates[[full_name]]$topics <- unique(c(candidates[[full_name]]$topics, topic))
      }
    }
  }
  list(candidates = unname(candidates[names_in_order]), capped = capped)
}

# Confirma a visibilidade pública usando o campo explícito ou, como fallback,
# o indicador private retornado pela API.
is_public_repository <- function(repository) {
  visibility <- scalar_text(repository$visibility, "")
  if (nzchar(visibility)) return(tolower(visibility) == "public")
  !scalar_bool(repository$private, TRUE)
}

# Registra no console o motivo pelo qual um candidato não passou na seleção.
reject_selection <- function(reason) {
  cat(sprintf("  [REJEITADO] %s\n", reason))
}

# Avalia um candidato contra todos os critérios de inclusão e devolve os
# metadados normalizados quando ele é aprovado; candidatos rejeitados retornam NULL.
evaluate_candidate <- function(candidate, approved_ids, token) {
  repository <- candidate$repository
  full_name <- scalar_text(repository$full_name, "")
  if (!is_public_repository(repository)) {
    reject_selection("não público")
    return(NULL)
  }
  if (scalar_bool(repository$fork, TRUE)) {
    reject_selection("é fork")
    return(NULL)
  }
  if (scalar_bool(repository$archived, TRUE)) {
    reject_selection("está arquivado")
    return(NULL)
  }
  created_at <- selection_time(repository$created_at)
  cutoff <- selection_time(SELECTION_GDPR_DATE)
  if (is.na(created_at) || created_at >= cutoff) {
    reject_selection("criado em ou depois de 25/05/2018")
    return(NULL)
  }
  stars <- scalar_int(repository$stargazers_count, 0L)
  if (stars < SELECTION_MIN_STARS) {
    reject_selection("menos de 500 estrelas")
    return(NULL)
  }
  license <- repository$license %||% list()
  spdx_id <- scalar_text(license$spdx_id, "")
  if (!(spdx_id %in% approved_ids)) {
    detail <- if (nzchar(spdx_id)) spdx_id else "sem SPDX reconhecido"
    reject_selection(sprintf("licença não aprovada (%s)", detail))
    return(NULL)
  }
  issues <- count_real_issues(full_name, token)
  if (issues < SELECTION_MIN_ISSUES) {
    reject_selection(sprintf("apenas %d issues", issues))
    return(NULL)
  }
  activity <- commit_activity(full_name, token)
  if (is.null(activity) || activity$months < SELECTION_MIN_ACTIVITY_MONTHS) {
    months <- if (is.null(activity)) 0 else activity$months
    reject_selection(sprintf("apenas %.1f meses de atividade", months))
    return(NULL)
  }
  window <- activity_window(full_name, token)
  if (!window$pre || !window$post) {
    reject_selection("sem atividade antes e depois de 25/05/2018")
    return(NULL)
  }
  list(
    repository = full_name,
    url = scalar_text(repository$html_url, ""),
    name = scalar_text(repository$name, ""),
    owner = scalar_text((repository$owner %||% list())$login, ""),
    description = scalar_text(repository$description, ""),
    language = scalar_text(repository$language, ""),
    license_spdx_id = spdx_id,
    license_name = scalar_text(license$name, ""),
    license_osi_approved = "true",
    stars = stars,
    issues = issues,
    created_at = format_selection_time(created_at),
    updated_at = format_selection_time(selection_time(repository$updated_at)),
    first_commit_at = format_selection_time(activity$first),
    last_commit_at = format_selection_time(activity$last),
    activity_months = round(activity$months, 2),
    ai_ml_match_topics = paste(candidate$topics, collapse = "; "),
    has_pre_gdpr_activity = "true",
    has_post_gdpr_activity = "true"
  )
}

# Converte a lista de registros selecionados em um data.frame com schema e
# ordem de colunas estáveis para o CSV versionado da amostra.
records_to_selection_data_frame <- function(records) {
  columns <- c(
    "repository", "url", "name", "owner", "description", "language",
    "license_spdx_id", "license_name", "license_osi_approved", "stars", "issues",
    "created_at", "updated_at", "first_commit_at", "last_commit_at", "activity_months",
    "ai_ml_match_topics", "has_pre_gdpr_activity", "has_post_gdpr_activity"
  )
  if (!length(records)) return(as.data.frame(setNames(replicate(length(columns), character(), simplify = FALSE), columns), check.names = FALSE))
  values <- lapply(columns, function(column) vapply(records, function(record) as.character(record[[column]] %||% ""), character(1L)))
  names(values) <- columns
  as.data.frame(values, stringsAsFactors = FALSE, check.names = FALSE)
}

# Cria o diretório de destino e grava a amostra selecionada em CSV UTF-8.
write_selection <- function(records, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(records_to_selection_data_frame(records), path, row.names = FALSE, fileEncoding = "UTF-8", na = "")
  cat(sprintf("Amostra gravada em %s (%d repositórios).\n", normalizePath(path, mustWork = FALSE), length(records)))
}

# Executa a busca e a avaliação completa, informando o progresso e persistindo
# apenas os candidatos que satisfazem todos os critérios de inclusão.
run_full_selection <- function(output, token, approved_ids) {
  found <- search_candidates(token)
  cat(sprintf("Candidatos únicos encontrados: %d\n", length(found$candidates)))
  if (found$capped) cat("Aviso: pelo menos uma busca atingiu o limite de 1.000 resultados.\n")
  selected <- list()
  for (index in seq_along(found$candidates)) {
    candidate <- found$candidates[[index]]
    name <- scalar_text(candidate$repository$full_name, "")
    cat(sprintf("[%d/%d] Analisando %s\n", index, length(found$candidates), name))
    result <- tryCatch(
      evaluate_candidate(candidate, approved_ids, token),
      error = function(error) {
        reject_selection(conditionMessage(error))
        NULL
      }
    )
    if (!is.null(result)) {
      selected[[length(selected) + 1L]] <- result
      cat("  [APROVADO]\n")
    }
  }
  write_selection(selected, output)
  invisible(output)
}

# Revalida um CSV existente quanto à visibilidade pública e à licença SPDX/OSI
# atual, mantendo as demais colunas da amostra original.
revalidate_selection <- function(input, output, token, approved_ids) {
  sample <- read_sample(input)
  if (!"repository" %in% names(sample)) stop("O CSV precisa da coluna repository.")
  records <- list()
  for (index in seq_len(nrow(sample))) {
    repository <- scalar_text(sample$repository[[index]], "")
    cat(sprintf("[%d/%d] Revalidando %s\n", index, nrow(sample), repository))
    result <- tryCatch({
      metadata <- selection_api(paste0("/repos/", repository), list(), token)$body
      license <- metadata$license %||% list()
      spdx_id <- scalar_text(license$spdx_id, "")
      if (!is_public_repository(metadata) || !(spdx_id %in% approved_ids)) NULL else {
        row <- as.list(sample[index, , drop = FALSE])
        row$license_spdx_id <- spdx_id
        row$license_name <- scalar_text(license$name, "")
        row$license_osi_approved <- "true"
        row
      }
    }, error = function(error) {
      reject_selection(conditionMessage(error))
      NULL
    })
    if (!is.null(result)) records[[length(records) + 1L]] <- result
  }
  values <- if (length(records)) do.call(rbind, lapply(records, as.data.frame, stringsAsFactors = FALSE)) else sample[0, , drop = FALSE]
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  write.csv(values, output, row.names = FALSE, fileEncoding = "UTF-8", na = "")
  invisible(output)
}

# Escolhe entre gerar uma nova amostra e revalidar uma amostra existente,
# carregando o token e a lista de licenças aprovadas necessários para a operação.
run_selection <- function(options) {
  token <- Sys.getenv("GITHUB_TOKEN", unset = "")
  if (!nzchar(token)) token <- read_dotenv_token()
  if (!nzchar(token)) stop("GITHUB_TOKEN não encontrado no ambiente ou em .env.")
  approved_ids <- read_approved_spdx()
  if (nzchar(options$input)) revalidate_selection(options$input, options$output, token, approved_ids)
  else run_full_selection(options$output, token, approved_ids)
}

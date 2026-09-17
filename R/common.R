## Funções compartilhadas da coleta, classificação e análise.
##
## Este arquivo concentra as regras que precisam ser iguais em todas as etapas.
## Assim, a coleta pode guardar dados brutos e a análise pode recalcular a
## classificação sem duplicar critérios em vários scripts.

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("O pacote 'jsonlite' é necessário. Instale-o com install.packages('jsonlite').")
}

# Os dois limites definem os snapshots históricos comparados pelo experimento.
PRE_UNTIL <- "2018-05-24T23:59:59Z"
POST_UNTIL <- "2026-06-30T23:59:59Z"
COLLECTOR_PROTOCOL_VERSION <- "open-source-no-size-limit-2026-09"
RULE_VERSION <- "semantic-conservative-2026-09-c1-c4"

TEXT_EXTENSIONS <- c(
  ".md", ".markdown", ".mdown", ".mkdn", ".rst", ".txt", ".adoc",
  ".asciidoc", ".html", ".htm", ".textile", ".xml", ".yaml", ".yml"
)

PRIVACY_MARKERS <- c(
  "privacy", "security", "gdpr", "personal-data", "personal_data",
  "data-protection", "data_protection", "policy", "legal", "terms",
  "consent", "retention", "subprocessor", "compliance"
)

# Cada regra tem termos primários, contexto próximo e grupos obrigatórios.
# Uma ocorrência só vale como evidência quando satisfaz todos esses níveis.
RULES <- list(
  C1 = list(
    label = "dados pessoais",
    primary = c(
      "\\bpersonal data\\b", "\\bpersonal information\\b",
      "personally identifiable information", "\\bPII\\b", "data subject",
      "user data", "customer data", "email address", "ip address",
      "mailing address", "usage data", "cookies?", "license plate data",
      "authentication credentials"
    ),
    context = c("data", "information", "user", "customer", "subject", "privacy"),
    required_groups = list(c(
      "\\bpersonal data\\b", "\\bpersonal information\\b",
      "personally identifiable information", "\\bPII\\b", "data subject",
      "user data", "customer data", "privacy"
    ))
  ),
  C2 = list(
    label = "finalidade do tratamento",
    primary = c(
      "\\bpurposes?\\b", "\\bused?\\s+(?:to|for)\\b",
      "\\butili[sz](?:e|ed|ing)\\b", "use (?:the |your )?(?:data|information)",
      "(?:personal|user|customer|usage) data[^.\\n]{0,160}\\b(?:to|for)\\b",
      "collect(?:ed|ing)? (?:the |your )?(?:data|information)[^.\\n]{0,160}\\b(?:to|for)\\b",
      "collect(?:ed|ing)?[^.\\n]{0,160}\\b(?:personal data|personal information|user data|your data|your information|PII|data subjects?)\\b",
      "process(?:ed|ing)?[^.\\n]{0,160}\\b(?:personal data|personal information|user data|your data|your information|PII|data subjects?)\\b",
      "why we collect", "how do we use your information"
    ),
    context = c(
      "personal", "user", "customer", "individual", "data subject", "PII",
      "privacy", "your (?:data|information)", "account"
    ),
    required_groups = list(c("data", "information", "processing", "collect"))
  ),
  C3 = list(
    label = "base legal",
    primary = c(
      "legal basis", "lawful basis", "\\bconsent\\b", "legitimate interest",
      "legal obligation", "necessary to comply"
    ),
    context = c(
      "personal", "privacy", "data subject", "PII", "your (?:data|information)",
      "account", "user"
    ),
    required_groups = list(
      c("data", "information", "processing"),
      c("\\bpersonal data\\b", "\\bpersonal information\\b", "privacy", "data subject", "PII", "user data", "customer data", "account data")
    )
  ),
  C4 = list(
    label = "direitos dos titulares",
    primary = c(
      "rights? (?:of|to) (?:data )?subjects?",
      "right to (?:access|rectification|erasure|deletion|portability|object|withdraw)",
      "access,? rectif(?:y|ication),? (?:erase|erasure|delete|deletion)",
      "data subject rights", "your right", "withdraw (?:your )?consent",
      "(?:modify|access|retrieve|correct|delete)[^.;\\n]{0,100}personal data",
      "users? can delete[^.;\\n]{0,100}(?:data|accounts?)", "data deleted from"
    ),
    context = c("data", "subject", "personal", "information", "consent", "privacy", "account"),
    required_groups = list()
  ),
  C5 = list(
    label = "retenção ou exclusão",
    primary = c(
      "data retention", "retention period",
      "retain(?:ed|ing)?[^.\\n]{0,100}(?:data|information)",
      "(?:delete|erase)(?:s|d|ion|ure)?[^.\\n]{0,100}(?:data|information|cookies?)",
      "remove(?:s|d|al)?\\s+(?:the |your )?(?:personal |user )?(?:data|information|cookies?)",
      "(?:data|account|cookies?) (?:deletion|erasure|removal)", "deleted content kept"
    ),
    context = c(
      "personal", "user", "customer", "data subject", "PII",
      "your (?:data|information)", "account", "log", "record", "retention", "cookies?"
    ),
    required_groups = list(
      c("data", "information", "storage", "log", "record", "retention", "cookies?"),
      c("\\bpersonal data\\b", "\\bpersonal information\\b", "data subject", "PII", "user data", "customer data", "account data", "cookies?")
    )
  ),
  C6 = list(
    label = "compartilhamento ou transferência",
    primary = c(
      "(?:share|transfer|disclos)(?:e|ed|ure|red|ring|d|ing)?[^.\\n]{0,80}(?:personal|user|customer|usage|training|private)? ?(?:data|information|records?)",
      "(?:personal|user|customer|usage|training|private) (?:data|information|records?)[^.\\n]{0,80}(?:share|transfer|disclos)",
      "(?:information|responses?|results?)[^.\\n]{0,80}(?:share|transfer|disclos)",
      "sharing of[^.\\n]{0,60}(?:data|information)", "share (?:it|them)",
      "third[- ]party", "service provider", "sub[- ]processor",
      "international transfer", "recipient"
    ),
    context = c(
      "personal", "user", "customer", "data subject", "PII",
      "your (?:data|information)", "account", "record", "privacy"
    ),
    required_groups = list(c("data", "information", "record", "responses?", "results?"))
  ),
  C7 = list(
    label = "proteção contra vazamento ou acesso não autorizado",
    primary = c(
      "unauthori[sz]ed access", "data breach", "security incident",
      "encrypt(?:ed|ion)?", "access control", "access[^.\\n]{0,100}(?:restricted|controlled)",
      "safeguard", "security measures", "secured networks?", "sandbox(?:ed|ing)?",
      "data never leaves", "privacy[- ]preserving",
      "(?:prevent|avoid)[^.\\n]{0,120}(?:PII|personal|private|sensitive|user|privacy)? ?(?:data |information )?leak",
      "PII leakage", "keep[^.\\n]{0,120}data private"
    ),
    context = c(
      "data", "information", "personal", "privacy", "breach", "user",
      "account", "record", "subject", "PII"
    ),
    required_groups = list(
      c("data", "information", "breach", "access", "encrypt", "PII", "privacy"),
      c("\\bpersonal data\\b", "\\bpersonal information\\b", "privacy", "user data", "customer data", "data subject", "account data", "PII", "private data")
    )
  )
)

## Valor padrão para listas JSON em que um campo pode estar ausente ou vazio.
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

## JSON pode representar o mesmo campo como lista, vetor ou valor escalar.
## Estas funções normalizam esses casos antes de qualquer comparação.
scalar <- function(x, default = "") {
  if (is.null(x) || length(x) == 0L) return(default)
  if (is.list(x)) x <- x[[1L]]
  if (length(x) == 0L || is.null(x)) return(default)
  x[[1L]]
}

scalar_text <- function(x, default = "") {
  value <- scalar(x, default)
  if (is.null(value) || length(value) == 0L || is.na(value)) return(default)
  as.character(value)
}

scalar_int <- function(x, default = 0L) {
  value <- suppressWarnings(as.integer(scalar(x, default)))
  if (is.na(value)) default else value
}

scalar_bool <- function(x, default = FALSE) {
  value <- scalar(x, default)
  if (is.logical(value)) return(isTRUE(value))
  if (is.numeric(value)) return(!is.na(value) && value != 0)
  normalized <- tolower(trimws(as.character(value)))
  if (normalized %in% c("true", "1", "yes")) TRUE
  else if (normalized %in% c("false", "0", "no", "")) FALSE
  else default
}

# Executa uma expressão regular sem interromper a análise por uma expressão
# inválida ou por um campo textual ausente.
safe_grepl <- function(pattern, text) {
  if (is.null(text) || !length(text) || is.na(text)) return(FALSE)
  tryCatch(grepl(pattern, text, ignore.case = TRUE, perl = TRUE), error = function(...) FALSE)
}

matches_any <- function(patterns, text) {
  length(patterns) > 0L && any(vapply(patterns, safe_grepl, logical(1L), text = text))
}

# O hash identifica exatamente a amostra usada para gerar cada checkpoint.
sha256_file <- function(path) {
  result <- system2("sha256sum", c(path), stdout = TRUE, stderr = TRUE)
  if (!length(result)) stop(sprintf("Não foi possível calcular SHA-256 de %s.", path))
  sub("[[:space:]].*$", "", result[[1L]])
}

# O conteúdo de cada documento também recebe hash para auditoria posterior.
sha256_text <- function(text) {
  temporary <- tempfile(fileext = ".txt")
  on.exit(unlink(temporary), add = TRUE)
  writeBin(charToRaw(enc2utf8(text %||% "")), temporary)
  sha256_file(temporary)
}

# Lê a amostra fechada, preservando os nomes das colunas e os textos UTF-8.
read_sample <- function(path) {
  if (!file.exists(path)) stop(sprintf("Arquivo de entrada não encontrado: %s", path))
  result <- read.csv(
    path, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE,
    na.strings = "NA", fileEncoding = "UTF-8", quote = "\""
  )
  names(result)[[1L]] <- sub("^\\ufeff", "", names(result)[[1L]])
  result[is.na(result)] <- ""
  result
}

# Impede que uma amostra diferente dos critérios de seleção seja processada.
validate_open_source_sample <- function(sample) {
  required <- c("repository", "license_spdx_id", "license_osi_approved")
  missing <- setdiff(required, names(sample))
  if (length(missing)) stop(sprintf("Colunas ausentes na amostra: %s", paste(missing, collapse = ", ")))
  invalid <- is.na(sample$license_spdx_id) |
    trimws(as.character(sample$license_spdx_id)) == "" |
    tolower(as.character(sample$license_osi_approved)) != "true"
  if (any(invalid)) {
    repository <- as.character(sample$repository[which(invalid)[[1L]]])
    stop(sprintf("A amostra exige licença SPDX/OSI aprovada. Repositório inválido: %s", repository))
  }
  invisible(TRUE)
}

# O formato JSONL permite gravar cada repositório assim que ele termina.
# Se a execução for interrompida, os registros já escritos continuam válidos.
write_jsonl <- function(records, path, append = TRUE) {
  con <- file(path, open = if (append) "a" else "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  for (record in records) {
    line <- jsonlite::toJSON(record, auto_unbox = TRUE, null = "null", dataframe = "rows", pretty = FALSE, digits = 16)
    writeLines(enc2utf8(line), con, useBytes = TRUE)
  }
}

# Reabre o checkpoint linha a linha para permitir retomada e análise offline.
read_jsonl <- function(path) {
  if (!file.exists(path)) return(list())
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  if (!length(lines)) return(list())
  lapply(seq_along(lines), function(index) {
    tryCatch(
      jsonlite::fromJSON(lines[[index]], simplifyVector = FALSE),
      error = function(error) stop(sprintf("JSONL inválido na linha %d: %s", index, error$message))
    )
  })
}

# Indexa o checkpoint por repositório e restaura a ordem da amostra depois.
index_records <- function(records) {
  result <- list()
  for (record in records) {
    repository <- scalar_text(record$repository, "")
    if (nzchar(repository)) result[[repository]] <- record
  }
  result
}

# Permite usar um token local sem colocá-lo no código-fonte.
read_dotenv_token <- function(path = ".env") {
  if (!file.exists(path)) return("")
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  match <- grep("^GITHUB_TOKEN=", lines, value = TRUE)
  if (!length(match)) return("")
  token <- sub("^GITHUB_TOKEN=", "", match[[1L]])
  sub("^[\"']|[\"']$", "", trimws(token))
}

# Cliente HTTP mínimo. A API pode responder com limites temporários ou erros
# transitórios; por isso a função repete a chamada com espera crescente.
http_request <- function(url, token = "", parse_json = TRUE, max_attempts = 4L) {
  body_path <- tempfile(fileext = ".body")
  status_path <- tempfile(fileext = ".status")
  error_path <- tempfile(fileext = ".error")
  on.exit(unlink(c(body_path, status_path, error_path)), add = TRUE)

  for (attempt in seq_len(max_attempts)) {
    headers <- c(
      "Accept: application/vnd.github+json",
      "X-GitHub-Api-Version: 2022-11-28",
      "User-Agent: github-privacy-experiment"
    )
    if (nzchar(token)) headers <- c(headers, paste0("Authorization: Bearer ", token))
    args <- c(
      "--silent", "--show-error", "--location", "--connect-timeout", "30",
      "--max-time", "45"
    )
    for (header in headers) args <- c(args, "--header", header)
    args <- c(args, "--dump-header", tempfile(), "--output", body_path,
              "--write-out", "%{http_code}", url)
    unlink(c(status_path, error_path))
    exit_status <- tryCatch(
      system2("curl", args, stdout = status_path, stderr = error_path),
      error = function(...) 1L
    )
    status_text <- if (file.exists(status_path)) paste(readLines(status_path, warn = FALSE), collapse = "") else ""
    status <- suppressWarnings(as.integer(trimws(status_text)))
    error_text <- if (file.exists(error_path)) paste(readLines(error_path, warn = FALSE), collapse = " ") else ""
    bytes <- if (file.exists(body_path)) readBin(body_path, "raw", n = file.info(body_path)$size) else raw(0)
    body_text <- if (length(bytes)) iconv(rawToChar(bytes), from = "UTF-8", to = "UTF-8", sub = "") else ""
    retryable <- is.na(status) || exit_status != 0L || status %in% c(403L, 429L) || status >= 500L
    if (retryable && attempt < max_attempts) {
      Sys.sleep(min(60, 2 ^ attempt))
      next
    }
    if (is.na(status)) status <- 0L
    if (parse_json && status >= 200L && status < 300L) {
      body <- tryCatch(
        if (nzchar(body_text)) jsonlite::fromJSON(body_text, simplifyVector = FALSE) else list(),
        error = function(error) list()
      )
    } else {
      body <- body_text
    }
    error <- if (status >= 200L && status < 300L) "" else {
      detail <- substr(body_text, 1L, 300L)
      paste0("HTTP ", status, if (nzchar(detail)) paste0(": ", detail) else if (nzchar(error_text)) paste0(": ", error_text) else "")
    }
    return(list(status = status, body = body, error = error))
  }
  list(status = 0L, body = if (parse_json) list() else "", error = "requisição sem resposta")
}

# Codifica cada componente do caminho sem transformar as barras em texto.
encode_path <- function(path) {
  parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
  paste(vapply(parts, utils::URLencode, character(1L), reserved = TRUE), collapse = "/")
}

# Monta chamadas JSON autenticadas para a API do GitHub.
github_api <- function(path, params = list(), token = "") {
  query <- if (length(params)) {
    paste(
      vapply(names(params), function(name) paste0(utils::URLencode(name, reserved = TRUE), "=", utils::URLencode(as.character(params[[name]]), reserved = TRUE)), character(1L)),
      collapse = "&"
    )
  } else ""
  url <- paste0("https://api.github.com", path, if (nzchar(query)) paste0("?", query) else "")
  http_request(url, token = token, parse_json = TRUE)
}

# Baixa o arquivo fixado por commit, evitando usar o estado atual do branch.
github_raw <- function(repository, sha, path, token = "") {
  url <- paste0("https://raw.githubusercontent.com/", repository, "/", sha, "/", encode_path(path))
  response <- http_request(url, token = token, parse_json = FALSE)
  list(
    status = response$status,
    text = if (response$status == 200L) response$body else "",
    note = if (response$status == 200L) "" else response$error
  )
}

## Retorna somente o nome final de um caminho Git.
file_name <- function(path) {
  parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
  parts[[length(parts)]]
}

# Apenas documentos textuais podem ser analisados; código binário é ignorado.
is_text_document <- function(path) {
  lower <- tolower(path)
  name <- file_name(lower)
  name %in% c("readme", "license", "copying") || any(vapply(TEXT_EXTENSIONS, function(extension) endsWith(lower, extension), logical(1L)))
}

# Seleciona README e arquivos cujo nome indica privacidade, segurança ou
# conformidade. A ordenação torna a coleta determinística.
document_candidates <- function(tree) {
  entries <- tree$tree %||% list()
  if (!is.list(entries) || !length(entries)) return(list())
  values <- list()
  for (entry in entries) {
    if (scalar_text(entry$type, "") != "blob") next
    path <- scalar_text(entry$path, "")
    if (!is_text_document(path)) next
    lower <- tolower(path)
    name <- file_name(lower)
    root <- !grepl("/", lower, fixed = TRUE)
    priority <- Inf
    if (root && (name == "readme" || startsWith(name, "readme."))) priority <- 0L
    else if (any(vapply(PRIVACY_MARKERS, grepl, logical(1L), x = name, fixed = TRUE))) priority <- 1L
    else if ((startsWith(lower, "docs/") || startsWith(lower, "doc/") || grepl("/docs/|/doc/", lower)) &&
             any(vapply(PRIVACY_MARKERS, grepl, logical(1L), x = lower, fixed = TRUE))) priority <- 2L
    if (!is.infinite(priority)) {
      values[[length(values) + 1L]] <- list(
        path = path,
        size = scalar_int(entry$size, -1L),
        blob_sha = scalar_text(entry$sha, ""),
        priority = priority
      )
    }
  }
  if (!length(values)) return(list())
  order_index <- order(vapply(values, function(x) x$priority, numeric(1L)), vapply(values, function(x) x$path, character(1L)))
  values[order_index]
}

# Divide o documento em unidades curtas para que o contexto seja local.
text_units <- function(text) {
  if (is.null(text) || !nzchar(text)) return(character())
  value <- gsub("\\r", " ", text, fixed = TRUE)
  units <- unlist(strsplit(value, "\\n+|(?<=[.!?])\\s+", perl = TRUE), use.names = FALSE)
  units <- trimws(gsub("\\s+", " ", units, perl = TRUE))
  units[nzchar(units)]
}

# Remove URLs, imagens e marcação antes de procurar termos semânticos.
clean_for_matching <- function(text) {
  value <- gsub("https?://[^\\s\"')>]+", " ", text, perl = TRUE)
  value <- gsub("!\\[[^]]*\\]\\([^)]*\\)", " ", value, perl = TRUE)
  value <- gsub("<[^>]+>", " ", value, perl = TRUE)
  value <- gsub("[*`]+", " ", value, perl = TRUE)
  trimws(gsub("\\s+", " ", value, perl = TRUE))
}

# Guarda um trecho centrado no termo encontrado, útil para revisão humana.
centered_snippet <- function(value, primary) {
  starts <- integer()
  ends <- integer()
  for (pattern in primary) {
    match <- tryCatch(regexpr(pattern, value, ignore.case = TRUE, perl = TRUE), error = function(...) -1L)
    if (length(match) && match[[1L]] > 0L) {
      starts <- c(starts, as.integer(match[[1L]]))
      ends <- c(ends, as.integer(match[[1L]] + attr(match, "match.length") - 1L))
    }
  }
  if (!length(starts)) return(substr(value, 1L, 650L))
  start <- max(1L, min(starts) - 300L)
  end <- min(nchar(value), max(max(ends) + 350L, start + 650L - 1L))
  if ((end - start + 1L) > 650L) end <- start + 650L - 1L
  trimws(substr(value, start, end))
}

# Procura o primeiro bloco que contém o termo primário, o contexto e todos os
# grupos obrigatórios. A janela inclui duas unidades antes e duas depois.
find_evidence <- function(text_values, primary, context = character(), required_groups = list(), accepted = function(...) TRUE) {
  if (!length(text_values)) return("")
  for (index in seq_along(text_values)) {
    unit <- text_values[[index]]
    if (!matches_any(primary, unit)) next
    first <- max(1L, index - 2L)
    last <- min(length(text_values), index + 2L)
    neighborhood <- paste(text_values[first:last], collapse = " ")
    if (length(context) && !matches_any(context, neighborhood)) next
    groups_match <- all(vapply(required_groups, function(group) matches_any(group, neighborhood), logical(1L)))
    if (!groups_match || !isTRUE(accepted(neighborhood))) next
    return(centered_snippet(neighborhood, primary))
  }
  ""
}

# C1 aceita categorias concretas ou dados nomeados associados a uma operação
# de tratamento, reduzindo falsos positivos causados por menções genéricas.
is_c1_evidence <- function(snippet) {
  concrete <- safe_grepl("\\b(?:email|e-mail|ip address|mailing address|cookies?|authentication credentials|license plate(?: data)?|health data|location data)\\b", snippet)
  named <- safe_grepl("\\b(?:personal|user|customer|usage|account) data\\b|\\bpersonal information\\b|\\bPII\\b|data subject", snippet)
  processing <- safe_grepl("\\b(?:collect(?:ed|ing)?|store(?:d|s)?|process(?:ed|ing)?|use(?:d|s)?|access(?:ed|ing)?|retain(?:ed|ing)?|shar(?:e|ed|ing)|delet(?:e|ed|ion))\\b", snippet)
  concrete || (named && processing)
}

# C4 exige um direito ou procedimento explicitamente ligado ao titular.
is_c4_evidence <- function(snippet) {
  safe_grepl("right[s]?\\s+(?:of|to)\\s+(?:data )?subjects?|right to (?:access|rectification|erasure|deletion|portability|object|withdraw)|data subject rights|your right|withdraw (?:your )?consent|(?:modify|access|retrieve|correct|delete)[^.;\\n]{0,100}personal data|users? can delete[^.;\\n]{0,100}(?:data|accounts?)|data deleted from", snippet)
}

# Fallback para padrões que não dependem da janela normal de unidades textuais.
direct_snippet <- function(text, pattern) {
  match <- tryCatch(regexpr(pattern, text, ignore.case = TRUE, perl = TRUE), error = function(...) -1L)
  if (length(match) && match[[1L]] > 0L) {
    start <- max(1L, as.integer(match[[1L]]) - 220L)
    end <- min(nchar(text), as.integer(match[[1L]] + attr(match, "match.length") + 360L - 1L))
    return(trimws(gsub("\\s+", " ", substr(text, start, end), perl = TRUE)))
  }
  ""
}

# O nome do arquivo também pode identificar uma política dedicada.
is_dedicated_privacy_document <- function(path) {
  safe_grepl("privacy|gdpr|data[-_ ]?protection|personal[-_ ]?data|cookie[-_ ]?policy", file_name(tolower(path)))
}

# C2-C6 exigem vínculo explícito com privacidade, dados pessoais ou titulares.
has_privacy_criterion_context <- function(snippet, path) {
  is_dedicated_privacy_document(path) ||
    safe_grepl("privacy|gdpr|personal data|personal information|personally identifiable|\\bPII\\b|data subject|user data|customer data|private data|anonymous user data|license plate data", snippet) ||
    safe_grepl("security[-_ ]and[-_ ]privacy|privacy[-_ ]and[-_ ]security|(?:^|/)privacy(?:[-_/]|$)", path)
}

# Dados declarados apenas como anônimos não são contados como dados pessoais.
is_anonymous_only <- function(snippet) {
  if (!safe_grepl("\\b(?:anonymous|anonymized|anonymised) (?:user |usage )?data\\b", snippet)) return(FALSE)
  !safe_grepl("personal|personally identifiable|\\bPII\\b|data subject|email address|ip address|cookies?|license plate|authentication credentials", snippet)
}

# Expiração de token ou cookie não é tratada como exclusão de dados pessoais.
is_false_deletion_context <- function(snippet) {
  (safe_grepl("jwt|auth[_ ]manager|revoke[_ ]token|token expiration|cookie deletion", snippet) &&
     !safe_grepl("(?:personal|user|customer|account) data|data deleted|delete accounts?|retention", snippet))
}

# Compartilhar bibliotecas ou dependências não é compartilhamento de dados.
is_false_sharing_context <- function(snippet) {
  software_only <- safe_grepl("shared librar|third[- ]party dependenc|share\\.sh|build_release.*share", snippet)
  data_disclosure <- safe_grepl("(?:share|transfer|disclos)[^.]{0,50}(?:personal|user|customer|usage|training|private)? ?data|(?:personal|user|customer|usage|training|private) data[^.]{0,50}(?:share|transfer|disclos)", snippet)
  software_only && !data_disclosure
}

# Citações acadêmicas não constituem evidência sobre o próprio projeto.
is_bibliographic_context <- function(snippet) {
  safe_grepl("proceedings|conference|journal|association for computing machinery|\\bdoi\\b|\\bet al\\.|\\bvolume\\s+\\d|\\bpages?\\s+\\d", snippet)
}

# Exclui listas de links e bibliografias sem contexto operacional do projeto.
is_external_resource_unit <- function(text) {
  has_link <- safe_grepl("https?://|!\\[|\\]\\(", text)
  if (!has_link) return(FALSE)
  if (safe_grepl("doi\\.org|sciencedirect|arxiv\\.org|proceedings\\.|conference|journal", text)) return(TRUE)
  project_language <- "\\b(?:we|our|this (?:application|project|service|site)|your app|collect(?:ed|ing)?|store|share|protect|encrypt|delete|privacy policy|security measures|data privacy|personal data collected)\\b"
  !safe_grepl(project_language, text)
}

# Exige linguagem do próprio projeto, uma política dedicada ou um mecanismo técnico.
is_project_contextual <- function(snippet, path, repository) {
  if (is_dedicated_privacy_document(path)) return(TRUE)
  if (safe_grepl("(?:^|/)privacy(?:[-_/]|$)|security[-_ ]and[-_ ]privacy|privacy[-_ ]and[-_ ]security", path)) return(TRUE)
  project_name <- if (grepl("/", repository, fixed = TRUE)) sub("^[^/]*/", "", repository) else repository
  parts <- strsplit(project_name, "[-_\\s]+", perl = TRUE)[[1L]]
  parts <- parts[nzchar(parts)]
  if (length(parts)) {
    project_regex <- paste(vapply(parts, function(part) paste0("\\Q", part, "\\E"), character(1L)), collapse = "[\\s_-]*")
    if (safe_grepl(project_regex, snippet)) return(TRUE)
  }
  if (safe_grepl("\\b(?:we|our|ours|you|your|users?|visitors?|this (?:project|application|app|service|site|website|framework|library|tool)|the (?:project|application|app|service|site|website))\\b", snippet)) return(TRUE)
  safe_grepl("privacy[- ]preserv|homomorphic encrypt|secure multi[- ]party|encrypted data|data never leaves|on[- ]device|training data[^.]{0,120}this project|redact[^.]{0,100}(?:private|personal) data|(?:protect|secure|sandbox)[^.]{0,100}(?:model|framework|application)|(?:model|framework|application)[^.]{0,100}(?:protect|secure|sandbox)", snippet)
}

# Mantém README de raiz e políticas relevantes; exclui testes, datasets, exemplos
# e dependências, que podem conter texto sem relação com a documentação do projeto.
relevant_documents <- function(documents) {
  markers <- c("privacy", "security", "gdpr", "data-protection", "personal-data", "privacy-policy", "terms", "legal", "compliance", "retention", "consent", "subprocessor")
  excluded <- c("dataset", "datasets", "test", "tests", "fixture", "fixtures", "sample", "samples", "output", "outputs", "example", "examples", "node_modules", "vendor", "dist", "build")
  if (!is.list(documents) || !length(documents)) return(list())
  values <- list()
  for (document in documents) {
    path <- scalar_text(document$path, "")
    lower <- tolower(path)
    name <- file_name(lower)
    segments <- strsplit(lower, "/", fixed = TRUE)[[1L]]
    root_readme <- !grepl("/", lower, fixed = TRUE) && (name == "readme" || startsWith(name, "readme."))
    marked <- any(vapply(markers, grepl, logical(1L), x = lower, fixed = TRUE))
    if (!root_readme && !marked) next
    if (!root_readme && any(segments %in% excluded)) next
    values[[length(values) + 1L]] <- document
  }
  values
}

# Recalcula C1-C7 e D1 somente sobre os documentos relevantes. Para cada critério,
# a primeira evidência válida é preservada junto do SHA, arquivo e trecho textual.
classify_documents <- function(documents, sha = "", repository = "") {
  evidence <- list()
  weak <- FALSE
  weak_pattern <- "\\bprivacy\\b|\\bgdpr\\b|personal data|personal information|data subject"
  for (document in documents) {
    if (scalar_int(document$status, 0L) != 200L) next
    content <- scalar_text(document$text, "")
    if (!nzchar(content)) next
    path <- scalar_text(document$path, "")
    dedicated <- is_dedicated_privacy_document(path)
    if (safe_grepl(weak_pattern, tolower(content))) weak <- TRUE
    values <- text_units(content)
    if (!dedicated) values <- values[!vapply(values, is_external_resource_unit, logical(1L))]
    values <- vapply(values, clean_for_matching, character(1L))
    values <- values[nzchar(values)]
    # Uma evidência por critério basta para a classificação, mas o trecho fica
    # armazenado para auditoria e revisão manual.
    for (code in names(RULES)) {
      if (!is.null(evidence[[code]])) next
      rule <- RULES[[code]]
      accepted <- function(snippet) {
        (code != "C1" || is_c1_evidence(snippet)) &&
          (code != "C4" || is_c4_evidence(snippet)) &&
          (code == "C7" || !is_anonymous_only(snippet)) &&
          (!(code %in% c("C2", "C3", "C4", "C5", "C6")) || has_privacy_criterion_context(snippet, path)) &&
          (code != "C5" || !is_false_deletion_context(snippet)) &&
          (code != "C6" || !is_false_sharing_context(snippet)) &&
          is_project_contextual(snippet, path, repository)
      }
      snippet <- find_evidence(values, rule$primary, rule$context, rule$required_groups, accepted)
      if (nzchar(snippet)) evidence[[code]] <- paste0("sha=", sha, "; file=", path, "; text=", snippet)
    }
  }

  # D1 pode ser sustentado por qualquer critério positivo; os blocos seguintes
  # também reconhecem uma política explícita ou mecanismos documentados de privacidade.
  d1_evidence <- if (length(evidence)) evidence[[1L]] else ""
  if (!nzchar(d1_evidence)) {
    # Primeiro procura uma política ou aviso de privacidade claramente identificado.
    policy_phrase <- "privacy\\s+(?:policy|notice)|data\\s+protection\\s+(?:policy|notice)"
    for (document in documents) {
      if (scalar_int(document$status, 0L) != 200L) next
      content <- scalar_text(document$text, "")
      if (!nzchar(content)) next
      path <- scalar_text(document$path, "")
      dedicated <- is_dedicated_privacy_document(path)
      values <- text_units(content)
      if (!dedicated) values <- values[!vapply(values, is_external_resource_unit, logical(1L))]
      values <- vapply(values, clean_for_matching, character(1L))
      values <- values[nzchar(values)]
      snippet <- find_evidence(values, c("privacy", "data protection", "personal data", "personal information", "data subject"), c("policy", "notice", "data", "information", "collect", "process", "user"), list())
      path_lower <- tolower(path)
      dedicated_path <- any(vapply(c("privacy", "gdpr", "data-protection", "personal-data", "consent", "retention"), grepl, logical(1L), x = path_lower, fixed = TRUE))
      if (nzchar(snippet) && (dedicated_path || safe_grepl(policy_phrase, clean_for_matching(content)))) {
        d1_evidence <- paste0("sha=", sha, "; file=", path, "; text=", snippet)
        break
      }
    }
  }
  if (!nzchar(d1_evidence)) {
    # Depois considera telemetria/analytics quando o texto a associa ao projeto.
    for (document in documents) {
      if (scalar_int(document$status, 0L) != 200L) next
      content <- clean_for_matching(scalar_text(document$text, ""))
      snippet <- find_evidence(text_units(content), c("anonymous user data", "anonymous usage", "usage analytics", "in-editor analytics"), c("collect", "collection", "analytics", "reporting", "data", "privacy"), list())
      path <- scalar_text(document$path, "")
      if (nzchar(snippet) && !is_bibliographic_context(snippet) && (is_project_contextual(snippet, path, repository) || safe_grepl("anonymous user data|anonymous usage|usage analytics|in-editor analytics", snippet))) {
        d1_evidence <- paste0("sha=", scalar_text(document$commit_sha, sha), "; file=", path, "; text=", snippet)
        break
      }
    }
  }
  if (!nzchar(d1_evidence)) {
    # Por fim, procura mecanismos técnicos que expressem uma propriedade de privacidade.
    pattern <- "privacy[- ]preserv|privacy of synthetic data|measur(?:e|es|ed|ing)[^.]{0,100}privacy|redact[^.]{0,100}(?:private|personal) data|(?:personal data|PII|privacy information)[^.]{0,100}leak|leak(?:ing|age)?[^.]{0,100}(?:personal data|PII|privacy information)"
    for (document in documents) {
      if (scalar_int(document$status, 0L) != 200L) next
      content <- clean_for_matching(scalar_text(document$text, ""))
      snippet <- direct_snippet(content, pattern)
      path <- scalar_text(document$path, "")
      if (nzchar(snippet) && !is_bibliographic_context(snippet) && is_project_contextual(snippet, path, repository)) {
        d1_evidence <- paste0("sha=", sha, "; file=", path, "; text=", snippet)
        break
      }
    }
  }
  values <- c(setNames(as.integer(names(RULES) %in% names(evidence)), names(RULES)))
  score <- sum(values)
  note <- if (!nzchar(d1_evidence) && weak) {
    "Menções isoladas a privacy/GDPR sem contexto semântico suficiente foram classificadas como 0."
  } else if (!nzchar(d1_evidence)) {
    "Nenhuma evidência semântica suficiente encontrada nos documentos candidatos."
  } else {
    paste0("Classificação ", RULE_VERSION, " por evidência textual contextualizada.")
  }
  list(
    C1 = values[["C1"]], C2 = values[["C2"]], C3 = values[["C3"]], C4 = values[["C4"]],
    C5 = values[["C5"]], C6 = values[["C6"]], C7 = values[["C7"]], D1 = as.integer(nzchar(d1_evidence)),
    score = score, D1_evidence = d1_evidence, note = note, evidence = evidence
  )
}

# Schema estável do CSV final, usado para manter as colunas na mesma ordem.
dataset_columns <- function() {
  c(
    "repository", "url", "name", "owner", "description", "language", "input_license_spdx_id", "input_license_name", "input_license_osi_approved", "stars", "issues", "created_at",
    "input_first_commit_at", "input_last_commit_at", "input_activity_months", "input_ai_ml_match_topics", "input_row_number", "sample_source_sha256",
    "accessibility_http_status", "accessible_at_run", "current_visibility", "current_private", "current_fork", "current_archived", "current_stars", "current_open_issues", "current_created_at", "current_updated_at", "current_license_spdx_id", "current_license_name", "current_metadata_exception",
    "pre_cutoff", "pre_commit_sha", "pre_commit_date", "pre_tree_sha", "pre_tree_truncated", "pre_document_paths", "pre_D1", "pre_C1", "pre_C2", "pre_C3", "pre_C4", "pre_C5", "pre_C6", "pre_C7", "pre_score", "pre_D1_evidence", "pre_C1_evidence", "pre_C2_evidence", "pre_C3_evidence", "pre_C4_evidence", "pre_C5_evidence", "pre_C6_evidence", "pre_C7_evidence", "pre_classification_note", "pre_version_error",
    "post_cutoff", "post_commit_sha", "post_commit_date", "post_tree_sha", "post_tree_truncated", "post_document_paths", "post_D1", "post_C1", "post_C2", "post_C3", "post_C4", "post_C5", "post_C6", "post_C7", "post_score", "post_D1_evidence", "post_C1_evidence", "post_C2_evidence", "post_C3_evidence", "post_C4_evidence", "post_C5_evidence", "post_C6_evidence", "post_C7_evidence", "post_classification_note", "post_version_error", "observation"
  )
}

# Combina os metadados da amostra com o checkpoint e recalcula a classificação
# para as duas versões históricas, preservando observações e evidências.
flatten_record <- function(row, record, row_number, source_hash) {
  output <- list()
  copy_field <- function(out, input) scalar_text(row[[input]], "")
  output$repository <- copy_field("repository", "repository")
  output$url <- copy_field("url", "url")
  output$name <- copy_field("name", "name")
  output$owner <- copy_field("owner", "owner")
  output$description <- copy_field("description", "description")
  output$language <- copy_field("language", "language")
  output$input_license_spdx_id <- copy_field("input_license_spdx_id", "license_spdx_id")
  output$input_license_name <- copy_field("input_license_name", "license_name")
  output$input_license_osi_approved <- copy_field("input_license_osi_approved", "license_osi_approved")
  output$stars <- copy_field("stars", "stars")
  output$issues <- copy_field("issues", "issues")
  output$created_at <- copy_field("created_at", "created_at")
  output$input_first_commit_at <- copy_field("input_first_commit_at", "first_commit_at")
  output$input_last_commit_at <- copy_field("input_last_commit_at", "last_commit_at")
  output$input_activity_months <- copy_field("input_activity_months", "activity_months")
  output$input_ai_ml_match_topics <- copy_field("input_ai_ml_match_topics", "ai_ml_match_topics")
  output$input_row_number <- row_number
  output$sample_source_sha256 <- source_hash

  accessibility <- record$accessibility %||% list()
  metadata <- accessibility$metadata %||% list()
  output$accessibility_http_status <- scalar_text(accessibility$http_status, "")
  output$accessible_at_run <- scalar_bool(accessibility$accessible, FALSE)
  output$current_visibility <- scalar_text(metadata$visibility, "")
  output$current_private <- if (is.null(metadata$private)) "" else scalar_bool(metadata$private, FALSE)
  output$current_fork <- if (is.null(metadata$fork)) "" else scalar_bool(metadata$fork, FALSE)
  output$current_archived <- if (is.null(metadata$archived)) "" else scalar_bool(metadata$archived, FALSE)
  output$current_stars <- scalar_text(metadata$stargazers_count, "")
  output$current_open_issues <- scalar_text(metadata$open_issues_count, "")
  output$current_created_at <- scalar_text(metadata$created_at, "")
  output$current_updated_at <- scalar_text(metadata$updated_at, "")
  output$current_license_spdx_id <- scalar_text(metadata$license_spdx_id, "")
  output$current_license_name <- scalar_text(metadata$license_name, "")
  output$current_metadata_exception <- scalar_text(accessibility$error, "")

  observations <- character()
  if (scalar_int(accessibility$http_status, 0L) != 200L) observations <- c(observations, "repositório não acessível na validação")
  # O mesmo procedimento é aplicado aos snapshots pré e pós para produzir um
  # dataset pareado e diretamente comparável.
  for (label in c("pre", "post")) {
    version <- record[[label]] %||% list()
    commit <- version$commit %||% list()
    documents <- relevant_documents(version$documents %||% list())
    classification <- classify_documents(documents, scalar_text(commit$sha, ""), output$repository)
    prefix <- paste0(label, "_")
    output[[paste0(prefix, "cutoff")]] <- if (label == "pre") PRE_UNTIL else POST_UNTIL
    output[[paste0(prefix, "commit_sha")]] <- scalar_text(commit$sha, "")
    output[[paste0(prefix, "commit_date")]] <- scalar_text(commit$date, "")
    output[[paste0(prefix, "tree_sha")]] <- scalar_text(commit$tree_sha, "")
    output[[paste0(prefix, "tree_truncated")]] <- scalar_bool(version$tree_truncated, FALSE)
    output[[paste0(prefix, "document_paths")]] <- paste(vapply(documents, function(document) scalar_text(document$path, ""), character(1L)), collapse = "; ")
    output[[paste0(prefix, "D1")]] <- classification$D1
    for (code in names(RULES)) {
      output[[paste0(prefix, code)]] <- classification[[code]]
      output[[paste0(prefix, code, "_evidence")]] <- scalar_text(classification$evidence[[code]], "")
    }
    output[[paste0(prefix, "score")]] <- classification$score
    output[[paste0(prefix, "D1_evidence")]] <- classification$D1_evidence
    output[[paste0(prefix, "classification_note")]] <- classification$note
    output[[paste0(prefix, "version_error")]] <- scalar_text(version$error, "")
    if (nzchar(output[[paste0(prefix, "version_error")]])) observations <- c(observations, paste0(label, ": ", output[[paste0(prefix, "version_error")]]))
    if (scalar_bool(version$tree_truncated, FALSE)) observations <- c(observations, paste0(label, ": árvore Git truncada pela API"))
    if (!length(version$documents %||% list())) observations <- c(observations, paste0(label, ": nenhum documento candidato recuperado"))
  }
  output$observation <- if (length(observations)) paste(observations, collapse = " | ") else "par completo processado"
  output[dataset_columns()]
}

# Só entra nos testes pareados o registro com os dois SHAs e as duas árvores
# Git recuperadas com HTTP 200; os demais permanecem identificados como incompletos.
is_complete_record <- function(record) {
  pre <- record$pre %||% list()
  post <- record$post %||% list()
  nzchar(scalar_text((pre$commit %||% list())$sha, "")) &&
    nzchar(scalar_text((post$commit %||% list())$sha, "")) &&
    scalar_int(pre$tree_request_status, 0L) == 200L && scalar_int(post$tree_request_status, 0L) == 200L
}

# Interpolação type=7, padrão de quantile() e comum em análises estatísticas.
percentile_value <- function(values, probability) {
  values <- sort(as.numeric(values))
  values <- values[is.finite(values)]
  if (!length(values)) return(NA_real_)
  as.numeric(stats::quantile(values, probability, type = 7, names = FALSE))
}

# Calcula o p-valor bicaudal enumerando a distribuição binomial condicional
# dos pares discordantes do teste de McNemar.
exact_mcnemar <- function(b, c) {
  n <- b + c
  if (!n) return(1)
  lower <- min(b, c)
  min(1, 2 * sum(vapply(0:lower, function(k) choose(n, k), numeric(1L))) / 2^n)
}

# Calcula a versão exata bicaudal do Wilcoxon pareado, removendo diferenças zero,
# usando postos médios e a distribuição de todas as combinações de sinais.
wilcoxon_exact <- function(differences) {
  values <- differences[differences != 0]
  n <- length(values)
  if (!n) return(list(n_nonzero = 0L, w_plus = 0, w_minus = 0, p_two_sided_exact = 1, rank_biserial = 0))
  ranks <- rank(abs(values), ties.method = "average")
  w_plus <- sum(ranks[values > 0])
  w_minus <- sum(ranks[values < 0])
  # Multiplicar por 2 representa postos médios como inteiros no DP da distribuição.
  scaled <- as.integer(round(ranks * 2))
  probabilities <- 1
  for (rank_value in scaled) {
    next_values <- numeric(length(probabilities) + rank_value)
    next_values[seq_along(probabilities)] <- next_values[seq_along(probabilities)] + probabilities * 0.5
    shifted <- seq_along(probabilities) + rank_value
    next_values[shifted] <- next_values[shifted] + probabilities * 0.5
    probabilities <- next_values
  }
  total <- sum(scaled)
  expected <- total / 2
  distance <- abs(w_plus * 2 - expected)
  sums <- 0:(length(probabilities) - 1L)
  p_value <- min(1, sum(probabilities[abs(sums - expected) >= distance - 1e-12]))
  list(
    n_nonzero = n, w_plus = w_plus, w_minus = w_minus,
    p_two_sided_exact = p_value,
    rank_biserial = if ((w_plus + w_minus) == 0) 0 else (w_plus - w_minus) / (w_plus + w_minus)
  )
}

# IC da mediana por bootstrap percentílico determinístico; a semente permite repetir
# exatamente o mesmo resultado em outra execução.
bootstrap_median_ci <- function(values, seed = 20260908L, repetitions = 10000L) {
  values <- as.numeric(values)
  if (!length(values)) return(c(NA_real_, NA_real_))
  set.seed(seed)
  medians <- vapply(seq_len(repetitions), function(...) median(sample(values, length(values), replace = TRUE)), numeric(1L))
  c(percentile_value(medians, 0.025), percentile_value(medians, 0.975))
}

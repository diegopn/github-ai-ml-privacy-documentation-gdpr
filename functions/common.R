if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("O pacote 'jsonlite' é necessário. Instale-o com install.packages('jsonlite').")
}
if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("O pacote 'yaml' é necessário. Instale-o com install.packages('yaml').")
}

find_project_root <- function() {
  file_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  current <- if (length(file_argument)) {
    candidate <- sub("^--file=", "", file_argument[[1L]])
    candidate <- if (file.exists(candidate)) candidate else file.path(getwd(), candidate)
    dirname(normalizePath(candidate, mustWork = FALSE))
  } else {
    getwd()
  }
  current <- normalizePath(current, mustWork = FALSE)
  repeat {
    if (file.exists(file.path(current, "settings.yml")) || dir.exists(file.path(current, ".git"))) return(current)
    parent <- dirname(current)
    if (identical(parent, current)) return(normalizePath(getwd(), mustWork = FALSE))
    current <- parent
  }
}

PROJECT_ROOT <- find_project_root()

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

SETTINGS <- yaml::read_yaml(file.path(PROJECT_ROOT, "settings.yml"))

setting <- function(section, key, default = NULL) {
  values <- SETTINGS[[section]] %||% list()
  value <- values[[key]]
  if (is.null(value) || length(value) == 0L) default else value
}

project_path <- function(...) file.path(PROJECT_ROOT, ...)

resolve_project_path <- function(path) {
  path <- as.character(path %||% "")[[1L]]
  if (!nzchar(path)) return(path)
  if (grepl("^~", path)) return(path.expand(path))
  if (grepl("^(/|[A-Za-z]:[\\\\/])", path)) return(path)
  project_path(path)
}

project_relative <- function(path) {
  absolute <- normalizePath(path, mustWork = FALSE)
  root <- normalizePath(PROJECT_ROOT, mustWork = FALSE)
  prefix <- paste0(root, .Platform$file.sep)
  if (startsWith(absolute, prefix)) substring(absolute, nchar(prefix) + 1L) else absolute
}

path_setting <- function(name, default) {
  value <- setting("paths", name, default)
  if (is.list(value)) value <- value[[1L]]
  resolve_project_path(as.character(value))
}

sample_path <- function() path_setting("sample", "inputs/final/selected_repositories.csv")

sample_hash_path <- function() path_setting("sample_hash", "inputs/final/published_sample.sha256.txt")

raw_checkpoint_path <- function() path_setting("raw_checkpoint", "inputs/raw/repository_results.jsonl")

reference_spdx_path <- function() path_setting("reference_spdx", "inputs/reference/osi_approved_spdx_ids.txt")

selection_audit_path <- function() path_setting("selection_audit", "outputs/metadata/selection_search_manifest.json")

selection_state_path <- function() path_setting("selection_state", "outputs/metadata/selection_run_state.json")

selection_progress_path <- function() path_setting("selection_progress", "outputs/metadata/selection_progress")

output_root_path <- function() path_setting("output_root", "outputs")

output_paths <- function(output = output_root_path()) {
  root <- resolve_project_path(output)
  list(
    root = root,
    tables = file.path(root, "tables"),
    figures = file.path(root, "figures"),
    reports = file.path(root, "reports"),
    metadata = file.path(root, "metadata")
  )
}

PRE_UNTIL <- as.character(setting("analysis", "pre_until", "2018-05-24T23:59:59Z"))
POST_UNTIL <- as.character(setting("analysis", "post_until", "2026-06-30T23:59:59Z"))
ALPHA <- as.numeric(setting("analysis", "alpha", 0.05))
COLLECTOR_PROTOCOL_VERSION <- "open-source-no-size-limit-expanded-topics-2026-09"
COLLECTOR_PROTOCOL_COMPATIBLE_VERSIONS <- c(
  COLLECTOR_PROTOCOL_VERSION,
  "open-source-no-size-limit-2026-09"
)
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

safe_grepl <- function(pattern, text) {
  if (is.null(text) || !length(text) || is.na(text)) return(FALSE)
  tryCatch(grepl(pattern, text, ignore.case = TRUE, perl = TRUE), error = function(...) FALSE)
}

matches_any <- function(patterns, text) {
  length(patterns) > 0L && any(vapply(patterns, safe_grepl, logical(1L), text = text))
}

sha256_file <- function(path) {
  result <- system2("sha256sum", c(path), stdout = TRUE, stderr = TRUE)
  if (!length(result)) stop(sprintf("Não foi possível calcular SHA-256 de %s.", path))
  sub("[[:space:]].*$", "", result[[1L]])
}

sha256_text <- function(text) {
  temporary <- tempfile(fileext = ".txt")
  on.exit(unlink(temporary), add = TRUE)
  writeBin(charToRaw(enc2utf8(text %||% "")), temporary)
  sha256_file(temporary)
}

read_sample <- function(path) {
  if (!file.exists(path)) stop(sprintf("Arquivo de entrada não encontrado: %s", path))
  result <- read.csv(
    path, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE,
    na.strings = "NA", fileEncoding = "UTF-8", quote = "\""
  )
  names(result)[[1L]] <- sub("^\\ufeff", "", names(result)[[1L]])
  result[is.na(result)] <- ""
  if (!nrow(result)) stop(sprintf("A amostra está vazia: %s", path))
  result
}

validate_open_source_sample <- function(sample) {
  required <- c("repository", "license_spdx_id", "license_osi_approved")
  missing <- setdiff(required, names(sample))
  if (length(missing)) stop(sprintf("Colunas ausentes na amostra: %s", paste(missing, collapse = ", ")))
  repositories <- trimws(as.character(sample$repository))
  if (any(!nzchar(repositories))) stop("A amostra contém repositório vazio.")
  if (anyDuplicated(repositories)) {
    duplicated_repositories <- unique(repositories[duplicated(repositories)])
    stop(sprintf("A amostra contém repositórios duplicados: %s", paste(head(duplicated_repositories, 5L), collapse = ", ")))
  }
  invalid <- is.na(sample$license_spdx_id) |
    trimws(as.character(sample$license_spdx_id)) == "" |
    tolower(as.character(sample$license_osi_approved)) != "true"
  if (any(invalid)) {
    repository <- as.character(sample$repository[which(invalid)[[1L]]])
    stop(sprintf("A amostra exige licença SPDX/OSI aprovada. Repositório inválido: %s", repository))
  }
  invisible(TRUE)
}

write_jsonl <- function(records, path, append = TRUE) {
  if (!length(records)) return(invisible(path))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- file(path, open = if (append) "a" else "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  for (record in records) {
    line <- jsonlite::toJSON(record, auto_unbox = TRUE, null = "null", dataframe = "rows", pretty = FALSE, digits = 16)
    writeLines(enc2utf8(line), con, useBytes = TRUE)
    flush(con)
  }
}

# Descarta uma última linha sem newline somente quando a escrita foi interrompida.
read_jsonl <- function(path, repair_truncated_tail = TRUE) {
  if (!file.exists(path)) return(list())
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  if (!length(lines)) return(list())
  parsed <- vector("list", length(lines))
  for (index in seq_along(lines)) {
    parsed[[index]] <- tryCatch(
      jsonlite::fromJSON(lines[[index]], simplifyVector = FALSE),
      error = function(error) {
        is_last <- index == length(lines)
        raw_file <- tryCatch(readBin(path, "raw", n = file.info(path)$size), error = function(...) raw())
        has_final_newline <- length(raw_file) > 0L && tail(raw_file, 1L) %in% as.raw(c(10, 13))
        if (isTRUE(repair_truncated_tail) && is_last && !has_final_newline) {
          warning(sprintf("A última linha incompleta do checkpoint foi removida: %s", path), call. = FALSE)
          temporary <- paste0(path, ".repair")
          valid_lines <- if (index > 1L) lines[seq_len(index - 1L)] else character()
          writeLines(valid_lines, temporary, useBytes = TRUE)
          if (!file.rename(temporary, path)) unlink(temporary, force = TRUE)
          return(NULL)
        }
        stop(sprintf("JSONL inválido na linha %d: %s", index, error$message), call. = FALSE)
      }
    )
  }
  parsed[!vapply(parsed, is.null, logical(1L))]
}

index_records <- function(records) {
  result <- list()
  for (record in records) {
    repository <- scalar_text(record$repository, "")
    if (nzchar(repository)) result[[repository]] <- record
  }
  result
}

read_dotenv_token <- function(path = project_path(".env")) {
  if (!file.exists(path)) return("")
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  match <- grep("^GITHUB_TOKEN=", lines, value = TRUE)
  if (!length(match)) return("")
  token <- sub("^GITHUB_TOKEN=", "", match[[1L]])
  sub("^[\"']|[\"']$", "", trimws(token))
}

atomic_write_json <- function(object, path, pretty = TRUE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- paste0(path, ".partial")
  jsonlite::write_json(object, temporary, auto_unbox = TRUE, pretty = pretty, na = "null", digits = 16)
  if (!file.rename(temporary, path)) {
    unlink(temporary, force = TRUE)
    stop(sprintf("Não foi possível finalizar o arquivo: %s", path), call. = FALSE)
  }
  invisible(path)
}

atomic_write_lines <- function(lines, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- paste0(path, ".partial")
  writeLines(enc2utf8(as.character(lines)), temporary, useBytes = TRUE)
  if (!file.rename(temporary, path)) {
    unlink(temporary, force = TRUE)
    stop(sprintf("Não foi possível finalizar o arquivo: %s", path), call. = FALSE)
  }
  invisible(path)
}

# Impede duas execuções do projeto de escreverem os mesmos artefatos.
process_is_alive <- function(pid) {
  pid <- suppressWarnings(as.integer(pid))
  if (is.na(pid) || pid <= 0L) return(FALSE)
  if (dir.exists("/proc")) return(file.exists(file.path("/proc", as.character(pid))))
  FALSE
}

read_lock_owner <- function(path) {
  owner_path <- file.path(path, "owner.json")
  if (!file.exists(owner_path)) return(NULL)
  tryCatch(jsonlite::fromJSON(owner_path, simplifyVector = FALSE), error = function(...) NULL)
}

acquire_file_lock <- function(path, timeout_seconds = 30, stale_seconds = 300, metadata = list()) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  started <- Sys.time()
  timeout_seconds <- max(0, as.numeric(timeout_seconds %||% 30))
  stale_seconds <- max(5, as.numeric(stale_seconds %||% 300))
  repeat {
    if (dir.create(path, showWarnings = FALSE, recursive = FALSE)) {
      owner <- c(
        list(
          pid = Sys.getpid(),
          acquired_at = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
        ),
        metadata
      )
      tryCatch(
        atomic_write_json(owner, file.path(path, "owner.json")),
        error = function(error) {
          unlink(path, recursive = TRUE, force = TRUE)
          stop(error)
        }
      )
      return(invisible(path))
    }

    info <- file.info(path)
    age <- if (nrow(info) && !is.na(info$mtime)) as.numeric(difftime(Sys.time(), info$mtime, units = "secs")) else 0
    owner <- read_lock_owner(path)
    owner_pid <- if (is.list(owner)) owner$pid %||% NA_integer_ else NA_integer_
    owner_known <- !is.null(owner)
    owner_alive <- owner_known && process_is_alive(owner_pid)
    if ((!owner_known && age > stale_seconds) || (owner_known && !owner_alive)) {
      unlink(path, recursive = TRUE, force = TRUE)
      next
    }
    elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    if (elapsed >= timeout_seconds) {
      owner_label <- if (owner_known) paste0("PID ", owner_pid) else "proprietário desconhecido"
      stop(sprintf("Lock ocupado: %s. Aguarde a execução atual terminar.", owner_label), call. = FALSE)
    }
    Sys.sleep(min(0.25, max(0.05, timeout_seconds - elapsed)))
  }
}

release_file_lock <- function(path) {
  if (dir.exists(path)) unlink(path, recursive = TRUE, force = TRUE)
  invisible(TRUE)
}

project_lock_path <- function() file.path(project_path("outputs", "metadata"), ".pipeline.lock")

with_file_lock <- function(path, code, timeout_seconds = 30, stale_seconds = 300, metadata = list()) {
  acquire_file_lock(path, timeout_seconds, stale_seconds, metadata)
  on.exit(release_file_lock(path), add = TRUE)
  force(code)
}

api_rate_state_path <- function() {
  resolve_project_path(as.character(setting("api", "rate_state_path", "outputs/metadata/github_api_rate_state.json")))
}

api_rate_state_default <- function() {
  list(
    last_request = list(search = 0, core = 0, raw = 0, graphql = 0),
    not_before = list(search = 0, core = 0, raw = 0, graphql = 0),
    updated_at = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
  )
}

read_api_rate_state <- function(path = api_rate_state_path()) {
  if (!file.exists(path)) return(api_rate_state_default())
  state <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(...) NULL)
  if (!is.list(state)) return(api_rate_state_default())
  defaults <- api_rate_state_default()
  state$last_request <- utils::modifyList(defaults$last_request, state$last_request %||% list())
  state$not_before <- utils::modifyList(defaults$not_before, state$not_before %||% list())
  state
}

api_state_number <- function(state, section, resource) {
  value <- (state[[section]] %||% list())[[resource]]
  value <- suppressWarnings(as.numeric(value %||% 0))
  if (is.na(value)) 0 else value
}

api_rate_interval <- function(resource) {
  key <- switch(
    resource,
    search = "search_interval_seconds",
    raw = "raw_interval_seconds",
    graphql = "graphql_interval_seconds",
    "core_interval_seconds"
  )
  value <- suppressWarnings(as.numeric(setting("api", key, if (resource == "search") 2.2 else 0)))
  if (is.na(value) || value < 0) 0 else value
}

api_wait_for_slot <- function(resource = "core") {
  resource <- if (resource %in% c("search", "raw", "graphql")) resource else "core"
  interval <- api_rate_interval(resource)
  path <- api_rate_state_path()
  lock_path <- paste0(path, ".lock")
  lock_timeout <- as.numeric(setting("api", "lock_timeout_seconds", 30))
  stale_seconds <- as.numeric(setting("api", "lock_stale_seconds", 300))
  wait <- with_file_lock(lock_path, {
    state <- read_api_rate_state(path)
    now <- as.numeric(Sys.time())
    interval_until <- api_state_number(state, "last_request", resource) + interval
    rate_limit_until <- api_state_number(state, "not_before", resource)
    scheduled <- max(
      interval_until,
      rate_limit_until
    )
    delay <- max(0, scheduled - now)
    state$last_request[[resource]] <- max(now, scheduled)
    state$updated_at <- format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
    atomic_write_json(state, path, pretty = FALSE)
    list(
      delay = delay,
      interval_delay = max(0, interval_until - now),
      rate_limit_delay = max(0, rate_limit_until - now)
    )
  }, timeout_seconds = lock_timeout, stale_seconds = stale_seconds)
  delay <- wait$delay
  max_wait <- as.numeric(setting("api", "max_rate_wait_seconds", 7200))
  if (is.finite(max_wait) && delay > max_wait) {
    stop("Limite da API do GitHub ainda ativo; aguarde o reset antes de executar novamente.", call. = FALSE)
  }
  remaining <- delay
  report_wait <- wait$rate_limit_delay > wait$interval_delay + 0.5
  if (remaining > 0 && report_wait) {
    cat(sprintf("GitHub API (%s): limite ativo; aguardando renovação.\n", resource))
  }
  while (remaining > 0) {
    chunk <- min(30, remaining)
    Sys.sleep(chunk)
    remaining <- remaining - chunk
  }
  invisible(TRUE)
}

api_record_rate_state <- function(resource, headers, fallback_delay = 0) {
  if (!is.list(headers)) return(invisible(FALSE))
  remaining <- suppressWarnings(as.numeric(headers$github_remaining %||% NA_real_))
  reset <- suppressWarnings(as.numeric(headers$github_reset %||% NA_real_))
  retry_after <- suppressWarnings(as.numeric(headers$retry_after %||% NA_real_))
  fallback_delay <- suppressWarnings(as.numeric(fallback_delay %||% 0))
  if (is.na(fallback_delay) || fallback_delay < 0) fallback_delay <- 0
  if ((is.na(remaining) || remaining > 0) && (is.na(retry_after) || retry_after <= 0) && fallback_delay <= 0) {
    return(invisible(FALSE))
  }
  path <- api_rate_state_path()
  lock_path <- paste0(path, ".lock")
  with_file_lock(lock_path, {
    state <- read_api_rate_state(path)
    now <- as.numeric(Sys.time())
    current <- api_state_number(state, "not_before", resource)
    retry_at <- if (!is.na(retry_after) && retry_after > 0) now + retry_after + 2 else 0
    reset_at <- if ((is.na(retry_after) || retry_after <= 0) &&
                    !is.na(remaining) && remaining <= 0 &&
                    !is.na(reset) && reset > now) reset + 2 else 0
    fallback_at <- if (fallback_delay > 0) now + fallback_delay else 0
    state$not_before[[resource]] <- max(current, reset_at, retry_at, fallback_at)
    state$updated_at <- format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
    atomic_write_json(state, path, pretty = FALSE)
    invisible(TRUE)
  }, timeout_seconds = as.numeric(setting("api", "lock_timeout_seconds", 30)),
  stale_seconds = as.numeric(setting("api", "lock_stale_seconds", 300)))
  invisible(TRUE)
}

api_rate_retry_at <- function(resources = c("search", "core", "graphql")) {
  state <- read_api_rate_state()
  values <- vapply(resources, function(resource) api_state_number(state, "not_before", resource), numeric(1L))
  retry_at <- max(c(values, 0), na.rm = TRUE)
  if (!is.finite(retry_at) || retry_at <= as.numeric(Sys.time())) return("")
  format(as.POSIXct(retry_at, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

parse_curl_headers <- function(lines) {
  lines <- as.character(lines %||% character())
  lines <- lines[nzchar(trimws(lines))]
  status_lines <- grep("^HTTP/", lines, ignore.case = TRUE)
  if (length(status_lines) > 1L) lines <- lines[status_lines[[length(status_lines)]]:length(lines)]
  result <- list()
  for (line in lines) {
    match <- regexec("^([^:]+):[[:space:]]*(.*)$", line, perl = TRUE)
    captured <- regmatches(line, match)[[1L]]
    if (length(captured) < 3L) next
    result[[tolower(trimws(captured[[2L]]))]] <- trimws(captured[[3L]])
  }
  result
}

api_header <- function(headers, name) as.character(headers[[tolower(name)]] %||% "")

rate_headers <- function(headers) {
  list(
    github_remaining = api_header(headers, "x-ratelimit-remaining"),
    github_reset = api_header(headers, "x-ratelimit-reset"),
    github_resource = api_header(headers, "x-ratelimit-resource"),
    retry_after = api_header(headers, "retry-after"),
    request_id = api_header(headers, "x-github-request-id")
  )
}

api_error_detail <- function(body_text, curl_error = "") {
  parsed <- tryCatch(jsonlite::fromJSON(body_text, simplifyVector = FALSE), error = function(...) NULL)
  message <- if (is.list(parsed)) scalar_text(parsed$message, "") else ""
  detail <- if (nzchar(message)) message else trimws(gsub("[[:space:]]+", " ", body_text, perl = TRUE))
  if (!nzchar(detail)) detail <- trimws(gsub("[[:space:]]+", " ", curl_error, perl = TRUE))
  substr(detail, 1L, 500L)
}

api_response_is_rate_limited <- function(status, body_text, rate) {
  if (is.na(status)) return(FALSE)
  if (status == 429L) return(TRUE)
  remaining <- suppressWarnings(as.numeric(rate$github_remaining %||% NA_real_))
  retry_after <- suppressWarnings(as.numeric(rate$retry_after %||% NA_real_))
  message <- tolower(body_text %||% "")
  has_rate_message <- grepl("rate limit|secondary rate|abuse detection", message, perl = TRUE)
  if (status == 200L && grepl('"errors"', message, fixed = TRUE)) {
    return((!is.na(remaining) && remaining <= 0) || has_rate_message)
  }
  status == 403L && (
    (!is.na(remaining) && remaining <= 0) ||
      (!is.na(retry_after) && retry_after > 0) || has_rate_message
  )
}

api_response_is_retryable <- function(status, body_text, rate) {
  if (is.na(status) || status == 0L) return(TRUE)
  if (status %in% c(408L, 425L, 429L) || status >= 500L) return(TRUE)
  api_response_is_rate_limited(status, body_text, rate)
}

api_response_ok <- function(response) {
  is.list(response) && !is.null(response$status) && !is.na(response$status) &&
    response$status >= 200L && response$status < 300L &&
    !nzchar(response$error %||% "")
}

api_error_is_rate_limited <- function(error) {
  message <- if (inherits(error, "condition")) conditionMessage(error) else as.character(error %||% "")
  grepl("rate limit|secondary rate|abuse detection|limite da api|limite de requisi", message, ignore.case = TRUE, perl = TRUE)
}

api_retry_delay <- function(status, rate, attempt, rate_limited = FALSE) {
  retry_after <- suppressWarnings(as.numeric(rate$retry_after %||% NA_real_))
  if (!is.na(retry_after) && retry_after > 0) return(retry_after)
  remaining <- suppressWarnings(as.numeric(rate$github_remaining %||% NA_real_))
  reset <- suppressWarnings(as.numeric(rate$github_reset %||% NA_real_))
  if ((!is.na(remaining) && remaining <= 0 || isTRUE(rate_limited)) &&
      !is.na(reset) && reset > as.numeric(Sys.time())) {
    return(reset - as.numeric(Sys.time()) + 2)
  }
  base <- suppressWarnings(as.numeric(setting("api", "retry_base_seconds", 2)))
  ceiling <- suppressWarnings(as.numeric(setting("api", "retry_max_seconds", 60)))
  if (is.na(base) || base <= 0) base <- 2
  if (is.na(ceiling) || ceiling < base) ceiling <- 60
  if (isTRUE(rate_limited)) return(max(60, min(ceiling, base * 2 ^ max(0, attempt - 1L))))
  min(ceiling, base * 2 ^ max(0, attempt - 1L))
}

http_request_files <- function() {
  list(
    body = tempfile(fileext = ".body"),
    status = tempfile(fileext = ".status"),
    error = tempfile(fileext = ".error"),
    headers = tempfile(fileext = ".headers")
  )
}

curl_request_arguments <- function(url, token, method, body_json, connect_timeout, request_timeout, files) {
  headers <- c(
    "Accept: application/vnd.github+json",
    "X-GitHub-Api-Version: 2022-11-28",
    paste0("User-Agent: ", setting("api", "user_agent", "github-ai-ml-privacy-research"))
  )
  if (nzchar(token)) headers <- c(headers, paste0("Authorization: Bearer ", token))
  args <- c(
    "--silent", "--show-error", "--location", "--connect-timeout", as.character(connect_timeout),
    "--max-time", as.character(request_timeout)
  )
  method <- toupper(as.character(method %||% "GET"))
  if (!method %in% c("GET", "POST")) stop(sprintf("Método HTTP não suportado: %s", method), call. = FALSE)
  if (identical(method, "POST")) {
    args <- c(args, "--request", "POST", "--header", shQuote("Content-Type: application/json"))
    if (!is.null(body_json)) args <- c(args, "--data-raw", shQuote(as.character(body_json)))
  }
  # system2() monta a linha de comando do shell; valores com espaços
  # precisam ser protegidos para que cada cabeçalho permaneça um argumento.
  for (header in headers) args <- c(args, "--header", shQuote(header))
  c(
    args,
    "--dump-header", shQuote(files$headers), "--output", shQuote(files$body),
    "--write-out", shQuote("%{http_code}"), shQuote(url)
  )
}

perform_http_request_attempt <- function(url, token, resource, method, body_json,
                                         connect_timeout, request_timeout, files) {
  api_wait_for_slot(resource)
  args <- curl_request_arguments(url, token, method, body_json, connect_timeout, request_timeout, files)
  unlink(c(files$status, files$error, files$headers))
  exit_status <- tryCatch(
    system2("curl", args, stdout = files$status, stderr = files$error),
    error = function(...) 1L
  )
  status_text <- if (file.exists(files$status)) paste(readLines(files$status, warn = FALSE), collapse = "") else ""
  status <- suppressWarnings(as.integer(trimws(status_text)))
  error_text <- if (file.exists(files$error)) paste(readLines(files$error, warn = FALSE), collapse = " ") else ""
  bytes <- if (file.exists(files$body)) readBin(files$body, "raw", n = file.info(files$body)$size) else raw(0)
  body_text <- if (length(bytes)) iconv(rawToChar(bytes), from = "UTF-8", to = "UTF-8", sub = "") else ""
  raw_headers <- if (file.exists(files$headers)) readLines(files$headers, warn = FALSE) else character()
  rate <- rate_headers(parse_curl_headers(raw_headers))
  list(
    status = status,
    body_text = body_text,
    error_text = error_text,
    raw_headers = raw_headers,
    rate = rate,
    exit_status = exit_status
  )
}

http_response <- function(status, body_text, error_text, rate, parse_json, raw_headers, return_headers) {
  if (is.na(status)) status <- 0L
  parsed_body <- tryCatch(
    if (nzchar(body_text)) jsonlite::fromJSON(body_text, simplifyVector = FALSE) else list(),
    error = function(error) list()
  )
  body <- if (parse_json) parsed_body else body_text
  error <- if (status >= 200L && status < 300L) {
    ""
  } else if (status > 0L) {
    detail <- api_error_detail(body_text, error_text)
    paste0("HTTP ", status, if (nzchar(detail)) paste0(": ", detail) else "")
  } else {
    paste0("Falha de transporte", if (nzchar(error_text)) paste0(": ", api_error_detail("", error_text)) else "")
  }
  result <- list(status = status, body = body, error = error, rate = rate)
  if (return_headers) result$headers <- raw_headers
  result
}

sleep_before_http_retry <- function(delay) {
  remaining <- delay
  while (remaining > 0) {
    chunk <- min(60, remaining)
    Sys.sleep(chunk)
    remaining <- remaining - chunk
  }
}

# Coordena tentativas, limite compartilhado e resposta HTTP final.
http_request <- function(url, token = "", parse_json = TRUE, max_attempts = NULL,
                         return_headers = FALSE, resource = "core", method = "GET",
                         body_json = NULL) {
  max_attempts <- suppressWarnings(as.integer(max_attempts %||% setting("api", "max_attempts", 4L)))
  if (is.na(max_attempts) || max_attempts < 1L) max_attempts <- 1L
  retry_budget <- suppressWarnings(as.numeric(setting("api", "retry_budget_seconds", 180)))
  if (is.na(retry_budget) || retry_budget < 0) retry_budget <- 180
  connect_timeout <- suppressWarnings(as.numeric(setting("api", "connect_timeout_seconds", 30)))
  request_timeout <- suppressWarnings(as.numeric(setting("api", "timeout_seconds", 45)))
  if (is.na(connect_timeout) || connect_timeout <= 0) connect_timeout <- 30
  if (is.na(request_timeout) || request_timeout <= 0) request_timeout <- 45
  max_rate_wait <- suppressWarnings(as.numeric(setting("api", "max_rate_wait_seconds", 7200)))
  if (is.na(max_rate_wait) || max_rate_wait < 0) max_rate_wait <- 7200

  started <- Sys.time()
  files <- http_request_files()
  on.exit(unlink(unlist(files, use.names = FALSE)), add = TRUE)
  attempt <- 1L
  repeat {
    response <- perform_http_request_attempt(
      url, token, resource, method, body_json, connect_timeout, request_timeout, files
    )
    status <- response$status
    rate <- response$rate
    rate_limited <- api_response_is_rate_limited(status, response$body_text, rate)
    retryable <- api_response_is_retryable(status, response$body_text, rate) || response$exit_status != 0L
    delay <- if (retryable) api_retry_delay(status, rate, attempt, rate_limited) else 0
    api_record_rate_state(resource, rate, fallback_delay = if (rate_limited) delay else 0)
    elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    if (rate_limited && elapsed + delay <= max_rate_wait) next
    if (retryable && attempt < max_attempts && elapsed + delay <= retry_budget) {
      cat(sprintf(
        "GitHub API: resposta transitória HTTP %s; nova tentativa %d/%d.\n",
        ifelse(is.na(status), "transporte", status), attempt, max_attempts - 1L
      ))
      sleep_before_http_retry(delay)
      attempt <- attempt + 1L
      next
    }
    return(http_response(
      status, response$body_text, response$error_text, rate,
      parse_json, response$raw_headers, return_headers
    ))
  }
}

encode_path <- function(path) {
  parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
  paste(vapply(parts, utils::URLencode, character(1L), reserved = TRUE), collapse = "/")
}

github_api <- function(path, params = list(), token = "", return_headers = FALSE) {
  query <- if (length(params)) {
    paste(
      vapply(names(params), function(name) paste0(utils::URLencode(name, reserved = TRUE), "=", utils::URLencode(as.character(params[[name]]), reserved = TRUE)), character(1L)),
      collapse = "&"
    )
  } else ""
  base <- sub("/$", "", as.character(setting("api", "github_base", "https://api.github.com")))
  url <- paste0(base, path, if (nzchar(query)) paste0("?", query) else "")
  resource <- if (grepl("/search/", path, fixed = TRUE)) "search" else "core"
  http_request(url, token = token, parse_json = TRUE, return_headers = return_headers, resource = resource)
}

# Usa o limite GraphQL separado para contar issues reais em lotes.
github_graphql <- function(query, token = "", variables = list()) {
  base <- sub("/$", "", as.character(setting("api", "github_graphql_base", "https://api.github.com/graphql")))
  payload <- jsonlite::toJSON(list(query = query, variables = variables), auto_unbox = TRUE, null = "null", digits = 16)
  response <- http_request(
    base, token = token, parse_json = TRUE, resource = "graphql", method = "POST",
    body_json = payload
  )
  errors <- response$body$errors %||% list()
  if (length(errors)) {
    messages <- vapply(errors, function(error) scalar_text(error$message, "erro GraphQL"), character(1L))
    detail <- paste(unique(messages), collapse = "; ")
    remaining <- suppressWarnings(as.numeric(response$rate$github_remaining %||% NA_real_))
    response$error <- if ((!is.na(remaining) && remaining <= 0) || nzchar(api_rate_retry_at())) {
      paste0("GraphQL rate limit: ", detail)
    } else detail
  }
  response
}

# Usa o SHA do commit para manter os snapshots históricos imutáveis.
github_raw <- function(repository, sha, path, token = "") {
  base <- sub("/$", "", as.character(setting("api", "raw_base", "https://raw.githubusercontent.com")))
  url <- paste0(base, "/", repository, "/", sha, "/", encode_path(path))
  response <- http_request(url, token = token, parse_json = FALSE, resource = "raw")
  list(
    status = response$status,
    text = if (response$status == 200L) response$body else "",
    note = if (response$status == 200L) "" else response$error,
    rate = response$rate %||% list()
  )
}

file_name <- function(path) {
  parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
  parts[[length(parts)]]
}

is_text_document <- function(path) {
  lower <- tolower(path)
  name <- file_name(lower)
  name %in% c("readme", "license", "copying") || any(vapply(TEXT_EXTENSIONS, function(extension) endsWith(lower, extension), logical(1L)))
}

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

text_units <- function(text) {
  if (is.null(text) || !nzchar(text)) return(character())
  value <- gsub("\\r", " ", text, fixed = TRUE)
  units <- unlist(strsplit(value, "\\n+|(?<=[.!?])\\s+", perl = TRUE), use.names = FALSE)
  units <- trimws(gsub("\\s+", " ", units, perl = TRUE))
  units[nzchar(units)]
}

clean_for_matching <- function(text) {
  value <- gsub("https?://[^\\s\"')>]+", " ", text, perl = TRUE)
  value <- gsub("!\\[[^]]*\\]\\([^)]*\\)", " ", value, perl = TRUE)
  value <- gsub("<[^>]+>", " ", value, perl = TRUE)
  value <- gsub("[*`]+", " ", value, perl = TRUE)
  trimws(gsub("\\s+", " ", value, perl = TRUE))
}

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

is_c1_evidence <- function(snippet) {
  concrete <- safe_grepl("\\b(?:email|e-mail|ip address|mailing address|cookies?|authentication credentials|license plate(?: data)?|health data|location data)\\b", snippet)
  named <- safe_grepl("\\b(?:personal|user|customer|usage|account) data\\b|\\bpersonal information\\b|\\bPII\\b|data subject", snippet)
  processing <- safe_grepl("\\b(?:collect(?:ed|ing)?|store(?:d|s)?|process(?:ed|ing)?|use(?:d|s)?|access(?:ed|ing)?|retain(?:ed|ing)?|shar(?:e|ed|ing)|delet(?:e|ed|ion))\\b", snippet)
  concrete || (named && processing)
}

is_c4_evidence <- function(snippet) {
  safe_grepl("right[s]?\\s+(?:of|to)\\s+(?:data )?subjects?|right to (?:access|rectification|erasure|deletion|portability|object|withdraw)|data subject rights|your right|withdraw (?:your )?consent|(?:modify|access|retrieve|correct|delete)[^.;\\n]{0,100}personal data|users? can delete[^.;\\n]{0,100}(?:data|accounts?)|data deleted from", snippet)
}

direct_snippet <- function(text, pattern) {
  match <- tryCatch(regexpr(pattern, text, ignore.case = TRUE, perl = TRUE), error = function(...) -1L)
  if (length(match) && match[[1L]] > 0L) {
    start <- max(1L, as.integer(match[[1L]]) - 220L)
    end <- min(nchar(text), as.integer(match[[1L]] + attr(match, "match.length") + 360L - 1L))
    return(trimws(gsub("\\s+", " ", substr(text, start, end), perl = TRUE)))
  }
  ""
}

is_dedicated_privacy_document <- function(path) {
  safe_grepl("privacy|gdpr|data[-_ ]?protection|personal[-_ ]?data|cookie[-_ ]?policy", file_name(tolower(path)))
}

has_privacy_criterion_context <- function(snippet, path) {
  is_dedicated_privacy_document(path) ||
    safe_grepl("privacy|gdpr|personal data|personal information|personally identifiable|\\bPII\\b|data subject|user data|customer data|private data|anonymous user data|license plate data", snippet) ||
    safe_grepl("security[-_ ]and[-_ ]privacy|privacy[-_ ]and[-_ ]security|(?:^|/)privacy(?:[-_/]|$)", path)
}

is_anonymous_only <- function(snippet) {
  if (!safe_grepl("\\b(?:anonymous|anonymized|anonymised) (?:user |usage )?data\\b", snippet)) return(FALSE)
  !safe_grepl("personal|personally identifiable|\\bPII\\b|data subject|email address|ip address|cookies?|license plate|authentication credentials", snippet)
}

is_false_deletion_context <- function(snippet) {
  (safe_grepl("jwt|auth[_ ]manager|revoke[_ ]token|token expiration|cookie deletion", snippet) &&
     !safe_grepl("(?:personal|user|customer|account) data|data deleted|delete accounts?|retention", snippet))
}

is_false_sharing_context <- function(snippet) {
  software_only <- safe_grepl("shared librar|third[- ]party dependenc|share\\.sh|build_release.*share", snippet)
  data_disclosure <- safe_grepl("(?:share|transfer|disclos)[^.]{0,50}(?:personal|user|customer|usage|training|private)? ?data|(?:personal|user|customer|usage|training|private) data[^.]{0,50}(?:share|transfer|disclos)", snippet)
  software_only && !data_disclosure
}

is_bibliographic_context <- function(snippet) {
  safe_grepl("proceedings|conference|journal|association for computing machinery|\\bdoi\\b|\\bet al\\.|\\bvolume\\s+\\d|\\bpages?\\s+\\d", snippet)
}

is_external_resource_unit <- function(text) {
  has_link <- safe_grepl("https?://|!\\[|\\]\\(", text)
  if (!has_link) return(FALSE)
  if (safe_grepl("doi\\.org|sciencedirect|arxiv\\.org|proceedings\\.|conference|journal", text)) return(TRUE)
  project_language <- "\\b(?:we|our|this (?:application|project|service|site)|your app|collect(?:ed|ing)?|store|share|protect|encrypt|delete|privacy policy|security measures|data privacy|personal data collected)\\b"
  !safe_grepl(project_language, text)
}

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

prepare_classification_documents <- function(documents, default_sha) {
  weak_pattern <- "\\bprivacy\\b|\\bgdpr\\b|personal data|personal information|data subject"
  prepared <- list()
  for (document in documents) {
    if (scalar_int(document$status, 0L) != 200L) next
    content <- scalar_text(document$text, "")
    if (!nzchar(content)) next
    path <- scalar_text(document$path, "")
    dedicated <- is_dedicated_privacy_document(path)
    matching_units <- text_units(content)
    if (!dedicated) matching_units <- matching_units[!vapply(matching_units, is_external_resource_unit, logical(1L))]
    matching_units <- vapply(matching_units, clean_for_matching, character(1L))
    matching_units <- matching_units[nzchar(matching_units)]
    cleaned_content <- clean_for_matching(content)
    prepared[[length(prepared) + 1L]] <- list(
      path = path,
      path_lower = tolower(path),
      dedicated = dedicated,
      matching_units = matching_units,
      fallback_units = text_units(cleaned_content),
      cleaned_content = cleaned_content,
      commit_sha = scalar_text(document$commit_sha, default_sha),
      weak = safe_grepl(weak_pattern, tolower(content))
    )
  }
  prepared
}

rule_evidence_is_accepted <- function(code, snippet, path, repository) {
  if (code == "C1" && !is_c1_evidence(snippet)) return(FALSE)
  if (code == "C4" && !is_c4_evidence(snippet)) return(FALSE)
  if (code != "C7" && is_anonymous_only(snippet)) return(FALSE)
  if (code %in% c("C2", "C3", "C4", "C5", "C6") && !has_privacy_criterion_context(snippet, path)) return(FALSE)
  if (code == "C5" && is_false_deletion_context(snippet)) return(FALSE)
  if (code == "C6" && is_false_sharing_context(snippet)) return(FALSE)
  is_project_contextual(snippet, path, repository)
}

collect_rule_evidence <- function(documents, sha, repository) {
  evidence <- list()
  for (document in documents) {
    for (code in names(RULES)) {
      if (!is.null(evidence[[code]])) next
      rule <- RULES[[code]]
      accepted <- function(snippet) rule_evidence_is_accepted(code, snippet, document$path, repository)
      snippet <- find_evidence(document$matching_units, rule$primary, rule$context, rule$required_groups, accepted)
      if (nzchar(snippet)) evidence[[code]] <- paste0("sha=", sha, "; file=", document$path, "; text=", snippet)
    }
  }
  evidence
}

find_policy_document_evidence <- function(documents, sha) {
  policy_phrase <- "privacy\\s+(?:policy|notice)|data\\s+protection\\s+(?:policy|notice)"
  path_markers <- c("privacy", "gdpr", "data-protection", "personal-data", "consent", "retention")
  for (document in documents) {
    snippet <- find_evidence(
      document$matching_units,
      c("privacy", "data protection", "personal data", "personal information", "data subject"),
      c("policy", "notice", "data", "information", "collect", "process", "user"),
      list()
    )
    dedicated_path <- any(vapply(path_markers, grepl, logical(1L), x = document$path_lower, fixed = TRUE))
    if (nzchar(snippet) && (dedicated_path || safe_grepl(policy_phrase, document$cleaned_content))) {
      return(paste0("sha=", sha, "; file=", document$path, "; text=", snippet))
    }
  }
  ""
}

find_anonymous_usage_evidence <- function(documents, repository) {
  primary <- c("anonymous user data", "anonymous usage", "usage analytics", "in-editor analytics")
  context <- c("collect", "collection", "analytics", "reporting", "data", "privacy")
  for (document in documents) {
    snippet <- find_evidence(document$fallback_units, primary, context, list())
    if (!nzchar(snippet) || is_bibliographic_context(snippet)) next
    project_context <- is_project_contextual(snippet, document$path, repository)
    known_analytics_phrase <- safe_grepl(paste(primary, collapse = "|"), snippet)
    if (project_context || known_analytics_phrase) {
      return(paste0("sha=", document$commit_sha, "; file=", document$path, "; text=", snippet))
    }
  }
  ""
}

find_privacy_technology_evidence <- function(documents, sha, repository) {
  pattern <- "privacy[- ]preserv|privacy of synthetic data|measur(?:e|es|ed|ing)[^.]{0,100}privacy|redact[^.]{0,100}(?:private|personal) data|(?:personal data|PII|privacy information)[^.]{0,100}leak|leak(?:ing|age)?[^.]{0,100}(?:personal data|PII|privacy information)"
  for (document in documents) {
    snippet <- direct_snippet(document$cleaned_content, pattern)
    if (!nzchar(snippet) || is_bibliographic_context(snippet)) next
    if (is_project_contextual(snippet, document$path, repository)) {
      return(paste0("sha=", sha, "; file=", document$path, "; text=", snippet))
    }
  }
  ""
}

find_fallback_d1_evidence <- function(documents, sha, repository) {
  evidence <- find_policy_document_evidence(documents, sha)
  if (nzchar(evidence)) return(evidence)
  evidence <- find_anonymous_usage_evidence(documents, repository)
  if (nzchar(evidence)) return(evidence)
  find_privacy_technology_evidence(documents, sha, repository)
}

classify_documents <- function(documents, sha = "", repository = "") {
  prepared_documents <- prepare_classification_documents(documents, sha)
  evidence <- collect_rule_evidence(prepared_documents, sha, repository)
  weak <- any(vapply(prepared_documents, `[[`, logical(1L), "weak"))
  d1_evidence <- if (length(evidence)) evidence[[1L]] else ""
  if (!nzchar(d1_evidence)) d1_evidence <- find_fallback_d1_evidence(prepared_documents, sha, repository)

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

dataset_columns <- function() {
  c(
    "repository", "url", "name", "owner", "description", "language", "input_license_spdx_id", "input_license_name", "input_license_osi_approved", "stars", "issues", "created_at",
    "input_first_commit_at", "input_last_commit_at", "input_activity_months", "input_ai_ml_match_topics", "input_row_number", "sample_source_sha256",
    "accessibility_http_status", "accessible_at_run", "current_visibility", "current_private", "current_fork", "current_archived", "current_stars", "current_open_issues", "current_created_at", "current_updated_at", "current_license_spdx_id", "current_license_name", "current_metadata_exception",
    "pre_cutoff", "pre_commit_sha", "pre_commit_date", "pre_tree_sha", "pre_tree_truncated", "pre_document_paths", "pre_D1", "pre_C1", "pre_C2", "pre_C3", "pre_C4", "pre_C5", "pre_C6", "pre_C7", "pre_score", "pre_D1_evidence", "pre_C1_evidence", "pre_C2_evidence", "pre_C3_evidence", "pre_C4_evidence", "pre_C5_evidence", "pre_C6_evidence", "pre_C7_evidence", "pre_classification_note", "pre_version_error",
    "post_cutoff", "post_commit_sha", "post_commit_date", "post_tree_sha", "post_tree_truncated", "post_document_paths", "post_D1", "post_C1", "post_C2", "post_C3", "post_C4", "post_C5", "post_C6", "post_C7", "post_score", "post_D1_evidence", "post_C1_evidence", "post_C2_evidence", "post_C3_evidence", "post_C4_evidence", "post_C5_evidence", "post_C6_evidence", "post_C7_evidence", "post_classification_note", "post_version_error", "observation"
  )
}

flatten_record <- function(row, record, row_number, source_hash) {
  input_fields <- c(
    repository = "repository", url = "url", name = "name", owner = "owner",
    description = "description", language = "language",
    input_license_spdx_id = "license_spdx_id", input_license_name = "license_name",
    input_license_osi_approved = "license_osi_approved", stars = "stars", issues = "issues",
    created_at = "created_at", input_first_commit_at = "first_commit_at",
    input_last_commit_at = "last_commit_at", input_activity_months = "activity_months",
    input_ai_ml_match_topics = "ai_ml_match_topics"
  )
  output <- lapply(input_fields, function(field) scalar_text(row[[field]], ""))
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
  for (label in c("pre", "post")) {
    version <- record[[label]] %||% list()
    commit <- version$commit %||% list()
    documents <- relevant_documents(version$documents %||% list())
    cached_classification <- version$classification %||% NULL
    classification_version <- scalar_text(version$classification_rule_version, "")
    # Cache sem versão explícita é compatível; versões diferentes são recalculadas.
    classification <- if (is.list(cached_classification) &&
                          (identical(classification_version, RULE_VERSION) || !nzchar(classification_version))) {
      cached_classification
    } else {
      classify_documents(documents, scalar_text(commit$sha, ""), output$repository)
    }
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

is_complete_record <- function(record) {
  pre <- record$pre %||% list()
  post <- record$post %||% list()
  nzchar(scalar_text((pre$commit %||% list())$sha, "")) &&
    nzchar(scalar_text((post$commit %||% list())$sha, "")) &&
    scalar_int(pre$tree_request_status, 0L) == 200L && scalar_int(post$tree_request_status, 0L) == 200L
}

percentile_value <- function(values, probability) {
  values <- sort(as.numeric(values))
  values <- values[is.finite(values)]
  if (!length(values)) return(NA_real_)
  as.numeric(stats::quantile(values, probability, type = 7, names = FALSE))
}

exact_mcnemar <- function(b, c) {
  n <- b + c
  if (!n) return(1)
  lower <- min(b, c)
  min(1, 2 * sum(vapply(0:lower, function(k) choose(n, k), numeric(1L))) / 2^n)
}

wilcoxon_exact <- function(differences) {
  values <- differences[differences != 0]
  n <- length(values)
  if (!n) return(list(n_nonzero = 0L, w_plus = 0, w_minus = 0, p_two_sided_exact = 1, rank_biserial = 0))
  ranks <- rank(abs(values), ties.method = "average")
  w_plus <- sum(ranks[values > 0])
  w_minus <- sum(ranks[values < 0])
  # A escala inteira permite representar postos médios na distribuição.
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

bootstrap_median_ci <- function(values, seed = 20260908L, repetitions = 10000L) {
  values <- as.numeric(values)
  if (!length(values)) return(c(NA_real_, NA_real_))
  set.seed(seed)
  medians <- vapply(seq_len(repetitions), function(...) median(sample(values, length(values), replace = TRUE)), numeric(1L))
  c(percentile_value(medians, 0.025), percentile_value(medians, 0.975))
}

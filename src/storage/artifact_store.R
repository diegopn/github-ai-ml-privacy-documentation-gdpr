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

with_file_lock <- function(path, code, timeout_seconds = 30, stale_seconds = 300, metadata = list()) {
  acquire_file_lock(path, timeout_seconds, stale_seconds, metadata)
  on.exit(release_file_lock(path), add = TRUE)
  force(code)
}


ArtifactStore <- R6::R6Class(
  "ArtifactStore",
  public = list(
    config = NULL,
    initialize = function(config = PROJECT_CONFIG) self$config <- config,
    sample_path = function() self$config$path("sample", "inputs/final/selected_repositories.csv"),
    sample_hash_path = function() self$config$path("sample_hash", "inputs/final/published_sample.sha256.txt"),
    raw_path = function() self$config$path("raw_checkpoint", "inputs/raw/repository_results.jsonl"),
    audit_path = function() self$config$path("selection_audit", "outputs/metadata/selection_search_manifest.json"),
    read_sample = function(path = self$sample_path()) read_sample(path),
    read_jsonl = function(path = self$raw_path()) read_jsonl(path),
    write_jsonl = function(records, path = self$raw_path(), append = TRUE) write_jsonl(records, path, append),
    write_json = function(object, path, pretty = TRUE) atomic_write_json(object, path, pretty),
    write_lines = function(lines, path) atomic_write_lines(lines, path),
    hash_file = function(path) sha256_file(path),
    validate_sample = function(sample) validate_open_source_sample(sample),
    assert_published_sample = function(path = self$sample_path()) {
      sample <- self$read_sample(path)
      self$validate_sample(sample)
      digest <- self$hash_file(path)
      hash_file <- self$sample_hash_path()
      audit_file <- self$audit_path()
      if (!file.exists(hash_file) || !file.exists(audit_file)) {
        stop("A amostra não tem manifesto persistente de seleção. Execute Rscript main.R --select.", call. = FALSE)
      }
      published_digest <- sub("^sha256[[:space:]]+", "", readLines(hash_file, warn = FALSE, encoding = "UTF-8")[[1L]])
      audit <- tryCatch(jsonlite::fromJSON(audit_file, simplifyVector = FALSE), error = function(...) NULL)
      if (!identical(digest, published_digest) || !identical(digest, scalar_text(audit$sample_sha256, ""))) {
        stop("O hash da amostra não corresponde aos manifestos persistentes. Execute novamente --select.", call. = FALSE)
      }
      if (!nzchar(scalar_text(audit$protocol_version, ""))) stop("O manifesto de seleção não informa a versão do protocolo.", call. = FALSE)
      sample
    },
    assert_checkpoint_complete = function(sample, checkpoint, source_hash = self$hash_file(self$sample_path())) {
      assert_checkpoint_complete(sample, checkpoint, source_hash)
    }
  )
)

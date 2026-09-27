ArtifactStore <- R6::R6Class(
  "ArtifactStore",
  public = list(
    initialize = function(config, values, protocol) {
      if (!inherits(config, "ProjectConfig"))
          stop("ArtifactStore exige ProjectConfig.", call. = FALSE)
      if (!inherits(values, "ValueTools"))
          stop("ArtifactStore exige ValueTools.", call. = FALSE)
      if (!inherits(protocol, "CollectionProtocol"))
          stop("ArtifactStore exige CollectionProtocol.", call. = FALSE)
      private$config <- config
      private$values <- values
      private$protocol <- protocol
      invisible(self)
    },

    sample_path = function() private$config$path("sample", "inputs/final/selected_repositories.csv"),

    sample_hash_path = function() private$config$path("sample_hash", "inputs/final/published_sample.sha256.txt"),

    raw_path = function() private$config$path("raw_checkpoint", "inputs/raw/repository_results.jsonl"),

    audit_path = function() private$config$path("selection_audit", "outputs/metadata/selection_search_manifest.json"),

    read_sample = function(path = self$sample_path()) {
      if (!file.exists(path))
          stop(sprintf("Arquivo de entrada não encontrado: %s", path), call. = FALSE)
      result <- utils::read.csv(path, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE,
          na.strings = "NA", fileEncoding = "UTF-8", quote = "\"")
      names(result)[[1L]] <- sub("^﻿", "", names(result)[[1L]])
      result[is.na(result)] <- ""
      if (!nrow(result))
          stop(sprintf("A amostra está vazia: %s", path), call. = FALSE)
      result
    },
    read_jsonl = function(path = self$raw_path()) {
      if (!file.exists(path)) return(list())
      lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
      lines <- lines[nzchar(trimws(lines))]
      lapply(seq_along(lines), function(index) {
        if (!startsWith(trimws(lines[[index]]), "{")) {
          stop(sprintf("JSONL precisa conter um objeto por linha: linha %d (%s).", index, path), call. = FALSE)
        }
        tryCatch(jsonlite::fromJSON(lines[[index]], simplifyVector = FALSE), error = function(error) {
          stop(sprintf("JSONL inválido na linha %d (%s): %s", index, path, conditionMessage(error)), call. = FALSE)
        })
      })
    },
    write_jsonl = function(records, path = self$raw_path(), append = TRUE) {
      if (!length(records) && isTRUE(append)) return(invisible(path))
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      connection <- file(path, open = if (append)
          "a"
      else "w", encoding = "UTF-8")
      on.exit(close(connection), add = TRUE)
      for (record in records) {
          line <- jsonlite::toJSON(record, auto_unbox = TRUE, null = "null", dataframe = "rows",
              pretty = FALSE, digits = 16)
          writeLines(enc2utf8(line), connection, useBytes = TRUE)
          flush(connection)
      }
      invisible(path)
    },
    write_json = function(object, path, pretty = TRUE) private$atomic_write(
      object, path, jsonlite::write_json, auto_unbox = TRUE, pretty = pretty, na = "null", digits = 16),

    write_lines = function(lines, path) private$atomic_write(
      enc2utf8(as.character(lines)), path, writeLines, useBytes = TRUE),

    write_csv = function(data, path, row.names = FALSE, fileEncoding = "UTF-8", na = "") private$atomic_write(
      data, path, utils::write.csv, row.names = row.names, fileEncoding = fileEncoding, na = na),

    hash_file = function(path) {
      if (!is.character(path) || length(path) != 1L || is.na(path) || !file.exists(path)) {
        stop("Arquivo não encontrado para hash.", call. = FALSE)
      }
      result <- unname(tools::sha256sum(path))
      if (is.na(result)) stop(sprintf("Não foi possível calcular SHA-256 de %s.", path), call. = FALSE)
      result
    },
    hash_text = function(text) {
      text <- private$values$or_else(text, "")
      if (!is.character(text) || length(text) != 1L || is.na(text)) {
        stop("O texto para SHA-256 precisa ser uma string não ausente.", call. = FALSE)
      }
      tools::sha256sum(bytes = charToRaw(enc2utf8(text)))
    },
    validate_sample = function(sample) {
      if (!is.data.frame(sample) || !nrow(sample)) stop("A amostra precisa ser uma tabela não vazia.", call. = FALSE)
      required <- c("repository", "license_spdx_id", "license_osi_approved")
      missing <- setdiff(required, names(sample))
      if (length(missing))
          stop(sprintf("Colunas ausentes na amostra: %s", paste(missing, collapse = ", ")),
              call. = FALSE)
      repositories <- as.character(sample$repository)
      normalized_repositories <- trimws(repositories)
      if (anyNA(repositories) || any(!nzchar(normalized_repositories))) {
          stop("A amostra contém repositório vazio.", call. = FALSE)
      }
      if (any(repositories != normalized_repositories)) {
          stop("A amostra contém identificadores de repositório com espaços externos.",
              call. = FALSE)
      }
      valid_repository <- vapply(strsplit(repositories, "/", fixed = TRUE), function(parts) {
          length(parts) == 2L && all(nzchar(parts)) && !any(parts %in% c(".", "..")) && all(grepl("^[[:alnum:]_.-]+$",
              parts))
      }, logical(1L))
      if (any(!valid_repository)) {
          stop("A amostra contém um identificador de repositório GitHub inválido.", call. = FALSE)
      }
      if (anyDuplicated(normalized_repositories)) {
          duplicated_repositories <- unique(normalized_repositories[duplicated(normalized_repositories)])
          stop(sprintf("A amostra contém repositórios duplicados: %s", paste(head(duplicated_repositories,
              5L), collapse = ", ")), call. = FALSE)
      }
      license_ids <- trimws(as.character(sample$license_spdx_id))
      osi_approved <- tolower(trimws(as.character(sample$license_osi_approved)))
      invalid <- is.na(license_ids) | !nzchar(license_ids) | is.na(osi_approved) | osi_approved !=
          "true"
      if (any(invalid)) {
          repository <- as.character(sample$repository[which(invalid)[[1L]]])
          stop(sprintf("A amostra exige licença SPDX/OSI aprovada. Repositório inválido: %s",
              repository), call. = FALSE)
      }
      invisible(TRUE)
    },
    publish_sample = function(sample, audit, path = self$sample_path()) {
      self$validate_sample(sample)
      path <- private$config$resolve(path)
      targets <- c(path, self$sample_hash_path(), self$audit_path())
      prepared <- private$prepare_sample_publication(sample, audit, path, targets)
      on.exit(unlink(c(prepared$temporary, prepared$backups), force = TRUE), add = TRUE)
      private$commit_sample_publication(targets, prepared)
      list(path = path, sample_sha256 = prepared$digest, rows = nrow(sample), audit = prepared$audit)
    },

    assert_published_sample = function(path = self$sample_path()) {
      sample <- self$read_sample(path)
      self$validate_sample(sample)
      digest <- self$hash_file(path)
      hash_path <- self$sample_hash_path()
      audit_path <- self$audit_path()
      if (!file.exists(hash_path) || !file.exists(audit_path)) {
          stop("A amostra não tem manifesto persistente de seleção. Execute Rscript main.R --select.",
              call. = FALSE)
      }
      hash_lines <- readLines(hash_path, warn = FALSE, encoding = "UTF-8")
      published_digest <- if (length(hash_lines))
          sub("^sha256[[:space:]]+", "", hash_lines[[1L]])
      else ""
      audit <- tryCatch(jsonlite::fromJSON(audit_path, simplifyVector = FALSE), error = function(...) NULL)
      if (!identical(digest, published_digest) || !identical(digest, private$values$scalar_text(audit$sample_sha256,
          ""))) {
          stop("O hash da amostra não corresponde aos manifestos persistentes. Execute novamente --select.",
              call. = FALSE)
      }
      if (!nzchar(private$values$scalar_text(audit$protocol_version, "")))
          stop("O manifesto de seleção não informa a versão do protocolo.", call. = FALSE)
      private$assert_selection_configuration(audit)
      sample
    },

    read_complete_checkpoint = function(sample, checkpoint = self$raw_path(), source_hash = self$hash_file(self$sample_path())) {
      self$validate_sample(sample)
      records <- self$read_jsonl(checkpoint)
      identities <- vapply(records, function(record) {
        repository <- record$repository
        if (is.character(repository) && length(repository) == 1L && !is.na(repository)) repository else ""
      }, character(1L))
      repositories <- as.character(sample$repository)
      if (any(!nzchar(identities)) || anyDuplicated(identities) ||
        length(identities) != length(repositories) || !setequal(identities, repositories)) {
        stop("O checkpoint precisa conter exatamente um registro por repositório da amostra.", call. = FALSE)
      }
      records <- records[match(repositories, identities)]
      cutoffs <- private$config$analysis_cutoffs()
      valid <- vapply(records, function(record) {
        private$protocol$record_reusable(record, source_hash) && private$protocol$record_complete(record) &&
          all(vapply(names(cutoffs), function(period) identical(record[[period]]$until, cutoffs[[period]]), logical(1L)))
      }, logical(1L))
      if (any(!valid)) {
        stop(sprintf("Coleta incompleta ou incompatível com os cortes configurados: %s. Execute novamente --select.",
          paste(utils::head(repositories[!valid], 8L), collapse = ", ")), call. = FALSE)
      }
      records
    },

    publish_checkpoint = function(temporary, path = self$raw_path()) {
      if (!file.exists(temporary))
          stop(sprintf("Checkpoint temporário não encontrado: %s", temporary), call. = FALSE)
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      if (!file.rename(temporary, path))
          stop("Não foi possível publicar o checkpoint completo da coleta.", call. = FALSE)
      invisible(path)
    }
  ),
  private = list(
    config = NULL,

    assert_selection_configuration = function(audit) {
      if (is.null(audit$configuration_fingerprint)) return(invisible(TRUE))
      configuration <- jsonlite::toJSON(private$config$selection_configuration(),
        auto_unbox = TRUE, null = "null", digits = 16)
      if (!identical(audit$configuration_fingerprint, self$hash_text(configuration))) {
        stop("A configuração de seleção mudou desde a publicação da amostra. Execute novamente --select.", call. = FALSE)
      }
      invisible(TRUE)
    },

    prepare_sample_publication = function(sample, audit, path, targets) {
      for (target in targets) dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
      temporary <- vapply(targets, function(target) {
          tempfile(paste0(".", basename(target), "-"), tmpdir = dirname(target), fileext = ".partial")
      }, character(1L))
      backups <- vapply(targets, function(target) {
          tempfile(paste0(".", basename(target), "-backup-"), tmpdir = dirname(target))
      }, character(1L))
      had_target <- file.exists(targets)
      prepared <- FALSE
      on.exit({
          if (!prepared) unlink(c(temporary, backups), force = TRUE)
      }, add = TRUE)
      for (index in which(had_target)) {
          if (!file.copy(targets[[index]], backups[[index]], overwrite = TRUE)) {
              stop(sprintf("Não foi possível preservar o artefato anterior: %s", targets[[index]]),
                  call. = FALSE)
          }
      }
      utils::write.csv(sample, temporary[[1L]], row.names = FALSE, fileEncoding = "UTF-8",
          na = "")
      digest <- self$hash_file(temporary[[1L]])
      audit$sample_sha256 <- digest
      audit$sample_rows <- nrow(sample)
      writeLines(c(paste0("sha256  ", digest), paste0("source  ", basename(path)), paste0("rows  ",
          nrow(sample))), temporary[[2L]], useBytes = TRUE)
      jsonlite::write_json(audit, temporary[[3L]], auto_unbox = TRUE, pretty = TRUE, na = "null",
          digits = 16)
      prepared <- TRUE
      list(temporary = temporary, backups = backups, had_target = had_target, digest = digest,
          audit = audit)
    },

    commit_sample_publication = function(targets, prepared) {
      committed <- character()
      success <- FALSE
      on.exit({
          if (!success) {
              for (target in committed) {
                  index <- match(target, targets)
                  if (prepared$had_target[[index]] && file.exists(prepared$backups[[index]])) {
                    file.copy(prepared$backups[[index]], target, overwrite = TRUE)
                  } else {
                    unlink(target, force = TRUE)
                  }
              }
          }
      }, add = TRUE)
      for (index in seq_along(targets)) {
          if (!file.rename(prepared$temporary[[index]], targets[[index]])) {
              stop(sprintf("Não foi possível publicar o artefato: %s", targets[[index]]),
                  call. = FALSE)
          }
          committed <- c(committed, targets[[index]])
      }
      success <- TRUE
      invisible(TRUE)
    },

    atomic_write = function(content, path, writer, ...) {
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      temporary <- tempfile(paste0(".", basename(path), "-"), tmpdir = dirname(path), fileext = ".partial")
      on.exit(unlink(temporary, force = TRUE), add = TRUE)
      writer(content, temporary, ...)
      if (!file.rename(temporary, path)) {
        stop(sprintf("Não foi possível finalizar o arquivo: %s", path), call. = FALSE)
      }
      invisible(path)
    },

    values = NULL,

    protocol = NULL
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

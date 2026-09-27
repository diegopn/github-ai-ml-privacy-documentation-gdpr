StorageContract <- R6::R6Class(
  "StorageContract",
  public = list(
    run = function(context) {
      project <- context$new_test_project()
      sample <- data.frame(repository = "owner/project", license_spdx_id = "MIT",
        license_osi_approved = "true", stringsAsFactors = FALSE)
      result <- project$store$publish_sample(sample, list(protocol_version = "offline-test"))
      missing_approval <- sample
      missing_approval$license_osi_approved <- NA_character_
      missing_repository <- sample
      missing_repository$repository <- NA_character_
      unsafe_repository <- sample
      unsafe_repository$repository <- "owner/project?redirect=example"
      context$check("ArtifactStore rejeita licença desconhecida e identificadores de repositório inválidos",
        all(context$errors(project$store$validate_sample(missing_approval)),
          context$errors(project$store$validate_sample(missing_repository)),
          context$errors(project$store$validate_sample(unsafe_repository))))
      context$check("ArtifactStore publica amostra, hash e manifesto juntos",
        all(result$rows == 1L, nrow(project$store$assert_published_sample()) == 1L,
          file.exists(project$store$sample_hash_path()), file.exists(project$store$audit_path())))
      jsonl_path <- file.path(project$root, "inputs", "raw", "records.jsonl")
      project$store$write_jsonl(list(list(repository = "owner/project", n = 1L)), jsonl_path, append = FALSE)
      parsed <- project$store$read_jsonl(jsonl_path)
      context$check("ArtifactStore grava e lê JSONL", all(length(parsed) == 1L, parsed[[1L]]$repository == "owner/project"))
      cat("{broken", file = jsonl_path, append = TRUE)
      original_hash <- project$store$hash_file(jsonl_path)
      context$check("leitura JSONL rejeita a última linha interrompida sem alterar o arquivo",
        all(context$errors(project$store$read_jsonl(jsonl_path)),
          identical(original_hash, project$store$hash_file(jsonl_path))))
      cat("\n", file = jsonl_path, append = TRUE)
      original_hash <- project$store$hash_file(jsonl_path)
      context$check("leitura JSONL rejeita uma linha inválida mesmo com quebra de linha final",
        all(context$errors(project$store$read_jsonl(jsonl_path)),
          identical(original_hash, project$store$hash_file(jsonl_path))))
      target <- file.path(project$root, "atomic.json")
      project$store$write_json(list(original = TRUE), target)
      before <- readBin(target, "raw", n = file.info(target)$size)
      failed <- context$errors(project$store$write_json(new.env(parent = emptyenv()), target))
      context$check("falha de serialização atômica preserva o arquivo anterior e remove o temporário",
        all(failed, identical(before, readBin(target, "raw", n = file.info(target)$size)),
          !length(list.files(project$root, pattern = "[.]partial$", all.files = TRUE))))
      lock_path <- context$track(tempfile("r6-artifact-lock-"))
      inside_lock <- context$services()$lock_manager$with_lock(lock_path, {
        dir.exists(lock_path) && file.exists(file.path(lock_path, "owner.json"))
      }, timeout_seconds = 0)
      context$check("ArtifactLock registra o proprietário e libera o lock após a operação",
        all(isTRUE(inside_lock), !dir.exists(lock_path)))
      private$hashes(context)
      private$checkpoint_integrity(context)
      private$selection_configuration(context)
      invisible(TRUE)
    }
  ),
  private = list(
    hashes = function(context) {
      store <- context$store()
      path <- context$track(tempfile("r6 hash % espaço-"))
      writeBin(charToRaw("abc"), path)
      context$check("SHA-256 nativo preserva hashes conhecidos e nomes de arquivo especiais",
        all(store$hash_text("abc") == "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad",
          identical(store$hash_file(path), store$hash_text("abc")),
          store$hash_text("") == "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
          context$errors(store$hash_text(c("a", "b")))))
    },
    checkpoint_integrity = function(context) {
      project <- context$new_test_project()
      sample <- data.frame(repository = "owner/project", license_spdx_id = "MIT", license_osi_approved = "true")
      record <- list(repository = "owner/project", source_sha256 = "test-hash",
        collector_protocol_version = context$protocol()$version(),
        pre = context$complete_period("pre"), post = context$complete_period("post", "post"))
      project$store$write_jsonl(list(record), append = FALSE)
      valid <- project$store$read_complete_checkpoint(sample, source_hash = "test-hash")
      context$check("leitura do checkpoint validado devolve os registros sem nova desserialização",
        all(length(valid) == 1L, valid[[1L]]$repository == "owner/project"))
      project$store$write_jsonl(list(record, record), append = FALSE)
      context$check("checkpoint duplicado é rejeitado em vez de sobrescrever registros em memória",
        context$errors(project$store$read_complete_checkpoint(sample, source_hash = "test-hash")))
      record$pre$until <- "2017-01-01T00:00:00Z"
      project$store$write_jsonl(list(record), append = FALSE)
      context$check("checkpoint de outro corte temporal não pode ser analisado como o corte atual",
        context$errors(project$store$read_complete_checkpoint(sample, source_hash = "test-hash")))
      record$pre <- context$complete_period("pre")
      project$store$write_jsonl(list(record), append = FALSE)
      cat("{broken", file = project$store$raw_path(), append = TRUE)
      hash <- project$store$hash_file(project$store$raw_path())
      context$check("conferência de completude rejeita JSONL truncado sem reparar seus bytes",
        all(context$errors(project$store$read_complete_checkpoint(sample, source_hash = "test-hash")),
          identical(hash, project$store$hash_file(project$store$raw_path()))))
      project$store$write_jsonl(list(), append = FALSE)
      context$check("substituir JSONL por lista vazia remove registros anteriores",
        all(file.info(project$store$raw_path())$size == 0,
          context$errors(project$store$read_complete_checkpoint(sample, source_hash = "test-hash"))))
      project$store$write_lines("42", project$store$raw_path())
      context$check("checkpoint rejeita JSON válido que não seja um objeto de registro",
        context$errors(project$store$read_jsonl()))
      context$check("publicação rejeita amostra vazia antes de alterar os artefatos",
        context$errors(project$store$publish_sample(sample[FALSE, ], list())))
    },
    selection_configuration = function(context) {
      project <- context$new_test_project()
      sample <- data.frame(repository = "owner/project", license_spdx_id = "MIT", license_osi_approved = "true")
      configuration <- jsonlite::toJSON(project$config$selection_configuration(), auto_unbox = TRUE, null = "null", digits = 16)
      project$store$publish_sample(sample, list(protocol_version = project$config$selection_settings()$protocol_version,
        configuration_fingerprint = project$store$hash_text(configuration)))
      settings_path <- file.path(project$root, "config", "settings.yml")
      settings <- yaml::read_yaml(settings_path)
      settings$selection$min_stars <- 600L
      yaml::write_yaml(settings, settings_path)
      config <- context$classes()$ProjectConfig$new(project$root, context$values())
      store <- context$classes()$ArtifactStore$new(config, context$values(), context$protocol())
      context$check("mudança dos critérios de seleção invalida a reutilização da amostra publicada",
        context$errors(store$assert_published_sample()))
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

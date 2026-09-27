ArtifactLockContract <- R6::R6Class(
  "ArtifactLockContract",
  public = list(
    run = function(context) {
      private$ownership(context)
      private$cleanup(context)
      private$stale_owners(context)
      invisible(TRUE)
    }
  ),
  private = list(
    ownership = function(context) {
      path <- context$track(tempfile("r6-lock-ownership-"))
      first <- private$new_lock(context)
      second <- private$new_lock(context)
      first$acquire(path, timeout_seconds = 0)
      owner <- jsonlite::fromJSON(file.path(path, "owner.json"))
      context$check("outra instância no mesmo processo não libera nem adquire um lock ativo",
        all(identical(second$release(path), FALSE), dir.exists(path),
          context$errors(second$acquire(path, timeout_seconds = 0))))
      context$check("aquisição repetida pelo proprietário falha sem alterar o lock",
        all(context$errors(first$acquire(path, timeout_seconds = 0)),
          identical(jsonlite::fromJSON(file.path(path, "owner.json"))$token, owner$token)))
      unlink(path, recursive = TRUE)
      second$acquire(path, timeout_seconds = 0, metadata = list(pid = -1L, token = "forged", mode = "test"))
      replacement <- jsonlite::fromJSON(file.path(path, "owner.json"))
      context$check("uma aquisição anterior não libera um lock substituído no mesmo caminho",
        all(identical(first$release(path), FALSE), dir.exists(path), !identical(owner$token, replacement$token)))
      context$check("metadados adicionais não substituem a identidade do proprietário",
        all(identical(replacement$pid, Sys.getpid()), replacement$token != "forged", replacement$mode == "test"))
      alias <- file.path(dirname(path), ".", basename(path))
      context$check("proprietário libera o lock por caminho equivalente e a liberação é idempotente",
        all(isTRUE(second$release(alias)), !dir.exists(path), isTRUE(second$release(path))))
    },
    cleanup = function(context) {
      path <- context$track(tempfile("r6-lock-cleanup-"))
      lock <- private$new_lock(context)
      context$check("with_lock libera a aquisição quando o código protegido falha",
        all(context$errors(lock$with_lock(path, stop("fixture failure"), timeout_seconds = 0)), !dir.exists(path)))
      other <- context$track(tempfile("r6-lock-independent-"))
      lock$acquire(path, timeout_seconds = 0)
      lock$acquire(other, timeout_seconds = 0)
      context$check("uma instância mantém a propriedade de aquisições independentes",
        all(isTRUE(lock$release(path)), dir.exists(other), isTRUE(lock$release(other))))
    },
    stale_owners = function(context) {
      path <- context$track(tempfile("r6-lock-stale-"))
      lock <- private$new_lock(context)
      dir.create(path)
      jsonlite::write_json(list(pid = "invalid"), file.path(path, "owner.json"), auto_unbox = TRUE)
      context$check("proprietário malformado não permite remover um lock recém-criado",
        all(context$errors(lock$acquire(path, timeout_seconds = 0, stale_seconds = 5)), dir.exists(path)))
      Sys.setFileTime(path, Sys.time() - 10)
      lock$acquire(path, timeout_seconds = 0, stale_seconds = 5)
      context$check("lock abandonado com proprietário inválido é recuperado após o prazo",
        all(isTRUE(lock$release(path)), !dir.exists(path)))
      if (dir.exists("/proc")) {
        dir.create(path)
        jsonlite::write_json(list(pid = .Machine$integer.max), file.path(path, "owner.json"), auto_unbox = TRUE)
        lock$acquire(path, timeout_seconds = 0)
        context$check("lock de processo inexistente é recuperado",
          all(isTRUE(lock$release(path)), !dir.exists(path)))
      }
    },
    new_lock = function(context) {
      context$classes()$ArtifactLock$new(
        context$classes()$SystemRuntime$new(), context$values())
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

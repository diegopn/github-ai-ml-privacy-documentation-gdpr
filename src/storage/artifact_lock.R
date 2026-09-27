ArtifactLock <- R6::R6Class(
  "ArtifactLock",
  public = list(
    initialize = function(runtime, values) {
      if (!inherits(runtime, "R6") || !is.function(runtime$now) || !is.function(runtime$sleep)) {
        stop("ArtifactLock exige um relógio R6 com now() e sleep().", call. = FALSE)
      }
      if (!inherits(values, "ValueTools")) stop("ArtifactLock exige ValueTools.", call. = FALSE)
      private$runtime <- runtime
      private$values <- values
      invisible(self)
    },
    acquire = function(path, timeout_seconds = 30, stale_seconds = 300, metadata = list()) {
      private$acquire_directory_lock(path, timeout_seconds, stale_seconds, metadata)
    },
    release = function(path) {
      key <- private$lock_key(path)
      token <- private$owned_locks[[key]]
      if (is.null(token)) return(invisible(!dir.exists(path)))
      if (!dir.exists(path)) {
        private$owned_locks[[key]] <- NULL
        return(invisible(TRUE))
      }
      owner <- private$read_lock_owner(path)
      if (!is.list(owner) || !all(c(identical(owner$token, token), identical(owner$pid, Sys.getpid())))) {
        private$owned_locks[[key]] <- NULL
        return(invisible(FALSE))
      }
      released <- unlink(path, recursive = TRUE, force = TRUE) == 0L
      if (released) private$owned_locks[[key]] <- NULL
      invisible(released)
    },
    with_lock = function(path, code, timeout_seconds = 30, stale_seconds = 300, metadata = list()) {
      self$acquire(path, timeout_seconds, stale_seconds, metadata)
      on.exit(self$release(path), add = TRUE)
      force(code)
    }
  ),
  private = list(
    runtime = NULL,
    values = NULL,
    owned_locks = list(),
    lock_key = function(path) {
      file.path(normalizePath(dirname(path), mustWork = FALSE), basename(path))
    },
    process_is_alive = function(pid) {
      if (!is.numeric(pid) || length(pid) != 1L ||
        !isTRUE(all(c(is.finite(pid), pid > 0, pid == floor(pid))))) return(NA)
      if (pid == Sys.getpid()) return(TRUE)
      # A missing /proc is not evidence that the owner process has exited.
      if (dir.exists("/proc")) file.exists(file.path("/proc", as.character(pid))) else NA
    },
    read_lock_owner = function(path) {
      owner_path <- file.path(path, "owner.json")
      if (!file.exists(owner_path)) return(NULL)
      tryCatch(jsonlite::fromJSON(owner_path, simplifyVector = FALSE), error = function(...) NULL)
    },
    write_lock_owner = function(path, owner) {
      owner_path <- file.path(path, "owner.json")
      temporary <- tempfile(".owner-", tmpdir = path, fileext = ".partial")
      on.exit(unlink(temporary, force = TRUE), add = TRUE)
      jsonlite::write_json(owner, temporary, auto_unbox = TRUE, pretty = FALSE, na = "null", digits = 16)
      if (!file.rename(temporary, owner_path)) stop("Não foi possível registrar o proprietário do lock.", call. = FALSE)
    },
    try_create_lock = function(path, metadata) {
      if (!dir.create(path, showWarnings = FALSE, recursive = FALSE)) return(FALSE)
      token <- basename(tempfile("lock-owner-"))
      owner <- c(list(
        pid = Sys.getpid(),
        token = token,
        acquired_at = format(as.POSIXct(private$runtime$now(), origin = "1970-01-01", tz = "UTC"),
          "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      ), metadata[setdiff(names(metadata), c("pid", "token", "acquired_at"))])
      tryCatch(private$write_lock_owner(path, owner), error = function(error) {
        unlink(path, recursive = TRUE, force = TRUE)
        stop(error)
      })
      private$owned_locks[[private$lock_key(path)]] <- token
      TRUE
    },
    remove_stale_lock = function(path, stale_seconds) {
      info <- file.info(path)
      now <- as.POSIXct(private$runtime$now(), origin = "1970-01-01", tz = "UTC")
      age <- if (nrow(info) && !is.na(info$mtime)) as.numeric(difftime(now, info$mtime, units = "secs")) else 0
      owner <- private$read_lock_owner(path)
      owner_pid <- if (is.list(owner)) owner$pid else NULL
      owner_alive <- private$process_is_alive(owner_pid)
      owner_known <- is.numeric(owner_pid) && length(owner_pid) == 1L &&
        isTRUE(all(c(is.finite(owner_pid), owner_pid > 0, owner_pid == floor(owner_pid))))
      stale <- any(c(isFALSE(owner_alive), !owner_known && age > stale_seconds))
      stale && unlink(path, recursive = TRUE, force = TRUE) == 0L
    },
    acquire_directory_lock = function(path, timeout_seconds, stale_seconds, metadata) {
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      started <- private$runtime$now()
      timeout_seconds <- suppressWarnings(as.numeric(private$values$or_else(timeout_seconds, 30)))
      stale_seconds <- suppressWarnings(as.numeric(private$values$or_else(stale_seconds, 300)))
      if (!is.finite(timeout_seconds) || timeout_seconds < 0) timeout_seconds <- 30
      if (!is.finite(stale_seconds) || stale_seconds < 5) stale_seconds <- 300

      repeat {
        if (private$try_create_lock(path, metadata)) return(invisible(path))
        if (private$remove_stale_lock(path, stale_seconds)) next
        elapsed <- private$runtime$now() - started
        if (elapsed >= timeout_seconds) {
          owner <- private$read_lock_owner(path)
          owner_label <- if (is.list(owner)) paste0("PID ", private$values$scalar_text(owner$pid, "?")) else {
            "proprietário desconhecido"
          }
          stop(sprintf("Lock ocupado: %s. Aguarde a execução atual terminar.", owner_label), call. = FALSE)
        }
        private$runtime$sleep(min(0.25, max(0.05, timeout_seconds - elapsed)))
      }
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

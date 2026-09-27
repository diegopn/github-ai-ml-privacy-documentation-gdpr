ValueTools <- R6::R6Class(
  "ValueTools",
  public = list(
    or_else = function(value, fallback) {
      if (is.null(value) || length(value) == 0L) fallback else value
    },
    scalar = function(value, default = "") {
      if (is.null(value) || length(value) == 0L) return(default)
      if (is.list(value)) value <- value[[1L]]
      if (!is.atomic(value) || is.null(value) || length(value) == 0L) default else value[[1L]]
    },
    scalar_text = function(value, default = "") {
      value <- self$scalar(value, default)
      if (is.null(value) || !length(value) || is.na(value)) return(default)
      as.character(value)
    },
    scalar_int = function(value, default = 0L) {
      value <- suppressWarnings(as.integer(self$scalar(value, default)))
      if (length(value) != 1L || is.na(value)) default else value
    },
    scalar_number = function(value, default = NA_real_) {
      value <- suppressWarnings(as.numeric(self$scalar(value, default)))
      if (length(value) != 1L || !is.finite(value)) default else value
    },
    scalar_bool = function(value, default = FALSE) {
      value <- self$scalar(value, default)
      if (is.null(value) || !length(value) || is.na(value)) return(default)
      if (is.logical(value)) return(isTRUE(value))
      if (is.numeric(value)) {
        if (!is.finite(value)) return(default)
        return(value != 0)
      }
      normalized <- tolower(trimws(as.character(value)))
      if (normalized %in% c("true", "1", "yes")) TRUE
      else if (normalized %in% c("false", "0", "no", "")) FALSE
      else default
    },
    safe_grepl = function(pattern, text) {
      text <- self$scalar_text(text, "")
      if (!nzchar(text)) return(FALSE)
      tryCatch(grepl(pattern, text, ignore.case = TRUE, perl = TRUE), error = function(...) FALSE)
    },
    matches_any = function(patterns, text) {
      length(patterns) > 0L && any(vapply(patterns, self$safe_grepl, logical(1L), text = text))
    },
    format_utc = function(value, empty = "") {
      value <- self$scalar(value, NULL)
      if (is.null(value) || !length(value) || is.na(value)) return(empty)
      format(value, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

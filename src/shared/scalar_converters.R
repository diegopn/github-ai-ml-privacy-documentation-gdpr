`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

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


format_utc <- function(value) {
  if (is.null(value) || !length(value) || is.na(value)) return("")
  format(value, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
}

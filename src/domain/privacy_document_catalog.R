PrivacyDocumentCatalog <- R6::R6Class(
  "PrivacyDocumentCatalog",
  public = list(
    initialize = function(rule_set, values) {
      if (!inherits(rule_set, "PrivacyRuleSet"))
          stop("PrivacyDocumentCatalog exige PrivacyRuleSet.", call. = FALSE)
      if (!inherits(values, "ValueTools"))
          stop("PrivacyDocumentCatalog exige ValueTools.", call. = FALSE)
      private$rule_set <- rule_set
      private$values <- values
      invisible(self)
    },

    discover_candidates = function(tree) {
      entries <- private$values$or_else(tree$tree, list())
      if (!is.list(entries) || !length(entries)) return(list())
      values <- list()
      markers <- private$rule_set$privacy_markers()
      for (entry in entries) {
        if (private$values$scalar_text(entry$type, "") != "blob") next
        path <- private$values$scalar_text(entry$path, "")
        if (!nzchar(path) || private$is_excluded_path(path) || !private$is_text_document(path)) next
        lower <- tolower(path)
        name <- basename(lower)
        root <- !grepl("/", lower, fixed = TRUE)
        priority <- c(0, 1, 2)[match(TRUE, c(
          root && (name == "readme" || startsWith(name, "readme.")),
          any(vapply(markers, grepl, logical(1L), x = name, fixed = TRUE)),
          (startsWith(lower, "docs/") || startsWith(lower, "doc/") ||
            grepl("/docs/|/doc/", lower)) &&
            any(vapply(markers, grepl, logical(1L), x = lower, fixed = TRUE))
        ), nomatch = 0L)]
        if (!length(priority)) next
        values[[length(values) + 1L]] <- list(
          path = path,
          size = private$values$scalar_int(entry$size, -1L),
          blob_sha = private$values$scalar_text(entry$sha, ""),
          priority = priority
        )
      }
      order_index <- order(vapply(values, function(x) x$priority, numeric(1L)), vapply(values,
        function(x) x$path, character(1L)))
      values[order_index]
    },
    relevant_documents = function(documents) {
      markers <- c("privacy", "security", "gdpr", "data-protection", "personal-data", "privacy-policy",
          "terms", "legal", "compliance", "retention", "consent", "subprocessor")
      if (!is.list(documents) || !length(documents))
          return(list())
      values <- list()
      for (document in documents) {
          path <- private$values$scalar_text(document$path, "")
          lower <- tolower(path)
          name <- basename(lower)
          root_readme <- !grepl("/", lower, fixed = TRUE) && (name == "readme" || startsWith(name,
              "readme."))
          marked <- any(vapply(markers, grepl, logical(1L), x = lower, fixed = TRUE))
          if (!root_readme && !marked)
              next
          if (private$is_excluded_path(lower))
              next
          values[[length(values) + 1L]] <- document
      }
      values
    },
    is_dedicated_privacy_document = function(path) {
      private$values$safe_grepl(
        "privacy|gdpr|data[-_ ]?protection|personal[-_ ]?data|cookie[-_ ]?policy",
        basename(tolower(private$values$scalar_text(path, "")))
      )
    }
  ),
  private = list(
    rule_set = NULL,

    values = NULL,

    excluded_path_segments = c(
      "dataset", "datasets", "test", "tests", "fixture", "fixtures", "sample", "samples",
      "output", "outputs", "example", "examples", "node_modules", "vendor", "dist", "build"
    ),

    is_excluded_path = function(path) {
      segments <- strsplit(tolower(path), "/", fixed = TRUE)[[1L]]
      any(segments %in% private$excluded_path_segments)
    },

    is_text_document = function(path) {
      lower <- tolower(path)
      name <- basename(lower)
      name %in% c("readme", "license", "copying") ||
        any(endsWith(lower, private$rule_set$text_extensions()))
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

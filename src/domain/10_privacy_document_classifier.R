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

collect_rule_evidence <- function(documents, sha, repository, rules = RULES) {
  evidence <- list()
  for (document in documents) {
    for (code in names(rules)) {
      if (!is.null(evidence[[code]])) next
      rule <- rules[[code]]
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

classify_documents <- function(documents, sha = "", repository = "", rules = RULES, rule_version = RULE_VERSION) {
  prepared_documents <- prepare_classification_documents(documents, sha)
  evidence <- collect_rule_evidence(prepared_documents, sha, repository, rules)
  weak <- any(vapply(prepared_documents, `[[`, logical(1L), "weak"))
  d1_evidence <- if (length(evidence)) evidence[[1L]] else ""
  if (!nzchar(d1_evidence)) d1_evidence <- find_fallback_d1_evidence(prepared_documents, sha, repository)

  values <- c(setNames(as.integer(names(rules) %in% names(evidence)), names(rules)))
  score <- sum(values)
  note <- if (!nzchar(d1_evidence) && weak) {
    "Menções isoladas a privacy/GDPR sem contexto semântico suficiente foram classificadas como 0."
  } else if (!nzchar(d1_evidence)) {
    "Nenhuma evidência semântica suficiente encontrada nos documentos candidatos."
  } else {
    paste0("Classificação ", rule_version, " por evidência textual contextualizada.")
  }
  list(
    C1 = values[["C1"]], C2 = values[["C2"]], C3 = values[["C3"]], C4 = values[["C4"]],
    C5 = values[["C5"]], C6 = values[["C6"]], C7 = values[["C7"]], D1 = as.integer(nzchar(d1_evidence)),
    score = score, D1_evidence = d1_evidence, note = note, evidence = evidence
  )
}



PrivacyDocumentClassifier <- R6::R6Class(
  "PrivacyDocumentClassifier",
  public = list(
    rules = NULL,
    version = NULL,
    initialize = function(rules = RULES, version = RULE_VERSION) {
      self$rules <- rules
      self$version <- version
    },
    classify = function(documents, sha = "", repository = "") {
      classify_documents(documents, sha, repository, self$rules, self$version)
    }
  )
)

PrivacyDocumentClassifier <- R6::R6Class(
  "PrivacyDocumentClassifier",
  public = list(
    initialize = function(rule_set, values, document_catalog, rules = NULL, version = "semantic-conservative-2026-09-c1-c4-r2") {
      if (!inherits(rule_set, "PrivacyRuleSet"))
          stop("PrivacyDocumentClassifier exige PrivacyRuleSet.", call. = FALSE)
      if (!inherits(values, "ValueTools"))
          stop("PrivacyDocumentClassifier exige ValueTools.", call. = FALSE)
      if (!inherits(document_catalog, "PrivacyDocumentCatalog")) {
          stop("PrivacyDocumentClassifier exige PrivacyDocumentCatalog.", call. = FALSE)
      }
      if (is.null(rules))
          rules <- rule_set$rules()
      criterion_codes <- paste0("C", 1:7)
      if (!is.list(rules) || is.null(names(rules)) || anyDuplicated(names(rules)) || !setequal(names(rules),
          criterion_codes)) {
          stop("As regras do classificador precisam definir exatamente C1 a C7.", call. = FALSE)
      }
      rules <- rules[criterion_codes]
      version <- as.character(version)
      if (length(version) != 1L || is.na(version) || !nzchar(trimws(version))) {
          stop("A versão das regras precisa ser um texto não vazio.", call. = FALSE)
      }
      private$values <- values
      private$document_catalog <- document_catalog
      private$validate_patterns(rules)
      private$rules <- rules
      private$version <- as.character(version)
    },

    classify = function(documents, sha = "", repository = "") {
      relevant_documents <- private$document_catalog$relevant_documents(documents)
      prepared_documents <- private$prepare_classification_documents(relevant_documents, sha)
      evidence <- private$collect_rule_evidence(prepared_documents, sha, repository)
      weak <- any(vapply(prepared_documents, `[[`, logical(1L), "weak"))
      d1_evidence <- if (length(evidence))
          evidence[[1L]]
      else ""
      if (!nzchar(d1_evidence))
          d1_evidence <- private$find_fallback_d1_evidence(prepared_documents, sha, repository)
      values <- setNames(as.integer(names(private$rules) %in% names(evidence)), names(private$rules))
      score <- sum(values)
      note <- if (!nzchar(d1_evidence) && weak) {
          "Menções isoladas a privacy/GDPR sem contexto semântico suficiente foram classificadas como 0."
      }
      else if (!nzchar(d1_evidence)) {
          "Nenhuma evidência semântica suficiente encontrada nos documentos candidatos."
      }
      else {
          paste0("Classificação ", private$version, " por evidência textual contextualizada.")
      }
      c(as.list(values), list(D1 = as.integer(nzchar(d1_evidence)), score = score,
        D1_evidence = d1_evidence, note = note, evidence = evidence))
    },

    rule_version = function() private$version,

    criterion_codes = function() names(private$rules)
  ),
  private = list(
    rules = NULL,

    version = NULL,

    validate_patterns = function(rules) {
      patterns <- unlist(private$fixed_patterns, use.names = FALSE)
      for (rule in rules) {
          patterns <- c(patterns, private$values$or_else(rule$primary, character()), private$values$or_else(rule$context,
              character()))
          for (group in private$values$or_else(rule$required_groups, list())) patterns <- c(patterns,
              group)
      }
      for (pattern in unique(patterns)) {
          tryCatch(suppressWarnings(grepl(pattern, "", ignore.case = TRUE, perl = TRUE)), error = function(error) stop(sprintf("Expressão regular inválida no classificador: %s",
              error$message), call. = FALSE))
      }
      invisible(TRUE)
    },

    text_units = function(text) {
      if (is.null(text) || !nzchar(text))
          return(character())
      value <- gsub("\\r", " ", text, fixed = TRUE)
      units <- unlist(strsplit(value, "\\n+|(?<=[.!?])\\s+", perl = TRUE), use.names = FALSE)
      units <- trimws(gsub("\\s+", " ", units, perl = TRUE))
      units[nzchar(units)]
    },

    clean_for_matching = function(text) {
      value <- gsub("https?://[^\\s\"')>]+", " ", text, perl = TRUE)
      value <- gsub("!\\[[^]]*\\]\\([^)]*\\)", " ", value, perl = TRUE)
      value <- gsub("<[^>]+>", " ", value, perl = TRUE)
      value <- gsub("[*`]+", " ", value, perl = TRUE)
      trimws(gsub("\\s+", " ", value, perl = TRUE))
    },

    centered_snippet = function(value, primary) {
      starts <- vapply(primary, function(pattern) {
        as.integer(regexpr(pattern, value, ignore.case = TRUE, perl = TRUE)[[1L]])
      }, integer(1L))
      starts <- starts[starts > 0L]
      if (!length(starts)) return(substr(value, 1L, 650L))
      start <- max(1L, min(starts) - 300L)
      trimws(substr(value, start, start + 649L))
    },
    find_evidence = function(text_values, primary, context = character(), required_groups = list(), code = NULL,
      path = "", repository = "") {
      if (!length(text_values))
          return("")
      for (index in seq_along(text_values)) {
          unit <- text_values[[index]]
          if (!private$values$matches_any(primary, unit))
              next
          first <- max(1L, index - 2L)
          last <- min(length(text_values), index + 2L)
          neighborhood <- paste(text_values[first:last], collapse = " ")
          if (length(context) && !private$values$matches_any(context, neighborhood))
              next
          groups_match <- all(vapply(required_groups, function(group) private$values$matches_any(group,
              neighborhood), logical(1L)))
          if (!groups_match)
              next
          if (!is.null(code) && !private$rule_evidence_is_accepted(code, neighborhood, path,
              repository))
              next
          return(private$centered_snippet(neighborhood, primary))
      }
      ""
    },

    is_c1_evidence = function(snippet) {
      concrete <- private$values$safe_grepl(private$fixed_patterns$c1_concrete, snippet)
      named <- private$values$safe_grepl(private$fixed_patterns$c1_named, snippet)
      processing <- private$values$safe_grepl(private$fixed_patterns$c1_processing, snippet)
      concrete || (named && processing)
    },

    is_c4_evidence = function(snippet) {
      private$values$safe_grepl(private$fixed_patterns$c4_evidence, snippet)
    },

    direct_snippet = function(text, pattern) {
      match <- regexpr(pattern, text, ignore.case = TRUE, perl = TRUE)
      if (length(match) && match[[1L]] > 0L) {
          start <- max(1L, as.integer(match[[1L]]) - 220L)
          end <- min(nchar(text), as.integer(match[[1L]] + attr(match, "match.length") + 360L -
              1L))
          return(trimws(gsub("\\s+", " ", substr(text, start, end), perl = TRUE)))
      }
      ""
    },

    has_privacy_criterion_context = function(snippet, path) {
      private$document_catalog$is_dedicated_privacy_document(path) || private$values$safe_grepl(private$fixed_patterns$privacy_criterion_context,
          snippet) || private$values$safe_grepl(private$fixed_patterns$privacy_path_context,
          path)
    },

    is_anonymous_only = function(snippet) {
      private$values$safe_grepl(private$fixed_patterns$anonymous_data, snippet) &&
        !private$values$safe_grepl(private$fixed_patterns$anonymous_exclusions, snippet)
    },
    is_false_deletion_context = function(snippet) {
      private$values$safe_grepl(private$fixed_patterns$false_deletion_security, snippet) &&
          !private$values$safe_grepl(private$fixed_patterns$false_deletion_personal_data, snippet)
    },

    is_false_sharing_context = function(snippet) {
      private$values$safe_grepl(private$fixed_patterns$false_sharing_software, snippet) &&
        !private$values$safe_grepl(private$fixed_patterns$false_sharing_data, snippet)
    },
    is_bibliographic_context = function(snippet) {
      private$values$safe_grepl(private$fixed_patterns$bibliographic_context, snippet)
    },

    is_external_resource_unit = function(text) {
      private$values$safe_grepl(private$fixed_patterns$external_resource_link, text) &&
        (private$values$safe_grepl(private$fixed_patterns$external_bibliographic_link, text) ||
          !private$values$safe_grepl(private$fixed_patterns$external_project_language, text))
    },
    is_project_contextual = function(snippet, path, repository) {
      if (private$document_catalog$is_dedicated_privacy_document(path))
          return(TRUE)
      if (private$values$safe_grepl(private$fixed_patterns$privacy_path_context, path))
          return(TRUE)
      project_name <- sub("^[^/]*/", "", repository)
      parts <- strsplit(project_name, "[-_\\s]+", perl = TRUE)[[1L]]
      parts <- parts[nzchar(parts)]
      if (length(parts)) {
          project_regex <- paste(paste0("\\Q", parts, "\\E"), collapse = "[\\s_-]*")
          if (private$values$safe_grepl(project_regex, snippet))
              return(TRUE)
      }
      if (private$values$safe_grepl(private$fixed_patterns$project_pronouns, snippet))
          return(TRUE)
      private$values$safe_grepl(private$fixed_patterns$project_security_context, snippet)
    },

    prepare_classification_documents = function(documents, default_sha) {
      weak_pattern <- private$fixed_patterns$weak_privacy_mention
      prepared <- list()
      for (document in documents) {
          if (private$values$scalar_int(document$status, 0L) != 200L)
              next
          content <- private$values$scalar_text(document$text, "")
          if (!nzchar(content))
              next
          path <- private$values$scalar_text(document$path, "")
          dedicated <- private$document_catalog$is_dedicated_privacy_document(path)
          matching_units <- private$text_units(content)
          if (!dedicated)
              matching_units <- matching_units[!vapply(matching_units, private$is_external_resource_unit,
                  logical(1L))]
          matching_units <- private$clean_for_matching(matching_units)
          matching_units <- matching_units[nzchar(matching_units)]
          cleaned_content <- private$clean_for_matching(content)
          prepared[[length(prepared) + 1L]] <- list(path = path, path_lower = tolower(path),
              dedicated = dedicated, matching_units = matching_units, fallback_units = private$text_units(cleaned_content),
              cleaned_content = cleaned_content, commit_sha = private$values$scalar_text(document$commit_sha,
                  default_sha), weak = private$values$safe_grepl(weak_pattern, tolower(content)))
      }
      prepared
    },

    rule_evidence_is_accepted = function(code, snippet, path, repository) {
      if (code == "C1" && !private$is_c1_evidence(snippet))
          return(FALSE)
      if (code == "C4" && !private$is_c4_evidence(snippet))
          return(FALSE)
      if (code != "C7" && private$is_anonymous_only(snippet))
          return(FALSE)
      if (code %in% c("C2", "C3", "C4", "C5", "C6") && !private$has_privacy_criterion_context(snippet,
          path))
          return(FALSE)
      if (code == "C5" && private$is_false_deletion_context(snippet))
          return(FALSE)
      if (code == "C6" && private$is_false_sharing_context(snippet))
          return(FALSE)
      private$is_project_contextual(snippet, path, repository)
    },

    collect_rule_evidence = function(documents, sha, repository) {
      evidence <- list()
      for (document in documents) {
          for (code in names(private$rules)) {
              if (!is.null(evidence[[code]]))
                  next
              rule <- private$rules[[code]]
              snippet <- private$find_evidence(document$matching_units, rule$primary, rule$context,
                  rule$required_groups, code, document$path, repository)
              if (nzchar(snippet))
                  evidence[[code]] <- paste0("sha=", sha, "; file=", document$path, "; text=",
                    snippet)
          }
      }
      evidence
    },

    find_policy_document_evidence = function(documents, sha) {
      policy_phrase <- private$fixed_patterns$privacy_policy_phrase
      path_markers <- c("privacy", "gdpr", "data-protection", "personal-data", "consent", "retention")
      for (document in documents) {
          snippet <- private$find_evidence(document$matching_units, c("privacy", "data protection",
              "personal data", "personal information", "data subject"), c("policy", "notice",
              "data", "information", "collect", "process", "user"), list())
          dedicated_path <- any(vapply(path_markers, grepl, logical(1L), x = document$path_lower,
              fixed = TRUE))
          if (nzchar(snippet) && (dedicated_path || private$values$safe_grepl(policy_phrase,
              document$cleaned_content))) {
              return(paste0("sha=", sha, "; file=", document$path, "; text=", snippet))
          }
      }
      ""
    },

    find_anonymous_usage_evidence = function(documents) {
      primary <- c("anonymous user data", "anonymous usage", "usage analytics", "in-editor analytics")
      context <- c("collect", "collection", "analytics", "reporting", "data", "privacy")
      for (document in documents) {
          snippet <- private$find_evidence(document$fallback_units, primary, context, list())
          if (!nzchar(snippet) || private$is_bibliographic_context(snippet))
              next
          return(paste0("sha=", document$commit_sha, "; file=", document$path, "; text=", snippet))
      }
      ""
    },

    find_privacy_technology_evidence = function(documents, sha, repository) {
      pattern <- private$fixed_patterns$privacy_technology_evidence
      for (document in documents) {
          snippet <- private$direct_snippet(document$cleaned_content, pattern)
          if (!nzchar(snippet) || private$is_bibliographic_context(snippet))
              next
          if (private$is_project_contextual(snippet, document$path, repository)) {
              return(paste0("sha=", sha, "; file=", document$path, "; text=", snippet))
          }
      }
      ""
    },

    find_fallback_d1_evidence = function(documents, sha, repository) {
      evidence <- private$find_policy_document_evidence(documents, sha)
      if (nzchar(evidence))
          return(evidence)
      evidence <- private$find_anonymous_usage_evidence(documents)
      if (nzchar(evidence))
          return(evidence)
      private$find_privacy_technology_evidence(documents, sha, repository)
    },


    values = NULL,

    fixed_patterns = list(
      c1_concrete = "\\b(?:email|e-mail|ip address|mailing address|cookies?|authentication credentials|license plate(?: data)?|health data|location data)\\b",

      c1_named = "\\b(?:personal|user|customer|usage|account) data\\b|\\bpersonal information\\b|\\bPII\\b|data subject",

      c1_processing = "\\b(?:collect(?:ed|ing)?|store(?:d|s)?|process(?:ed|ing)?|use(?:d|s)?|access(?:ed|ing)?|retain(?:ed|ing)?|shar(?:e|ed|ing)|delet(?:e|ed|ion))\\b",

      c4_evidence = "right[s]?\\s+(?:of|to)\\s+(?:data )?subjects?|right to (?:access|rectification|erasure|deletion|portability|object|withdraw)|data subject rights|your right|withdraw (?:your )?consent|(?:modify|access|retrieve|correct|delete)[^.;\\n]{0,100}personal data|users? can delete[^.;\\n]{0,100}(?:data|accounts?)|data deleted from",

      external_resource_link = "https?://|!\\[|\\]\\(",

      external_bibliographic_link = "doi\\.org|sciencedirect|arxiv\\.org|proceedings\\.|conference|journal",

      external_project_language = "\\b(?:we|our|this (?:application|project|service|site)|your app|collect(?:ed|ing)?|store|share|protect|encrypt|delete|privacy policy|security measures|data privacy|personal data collected)\\b",

      project_security_context = "privacy[- ]preserv|homomorphic encrypt|secure multi[- ]party|encrypted data|data never leaves|on[- ]device|training data[^.]{0,120}this project|redact[^.]{0,100}(?:private|personal) data|(?:protect|secure|sandbox)[^.]{0,100}(?:model|framework|application)|(?:model|framework|application)[^.]{0,100}(?:protect|secure|sandbox)",

      bibliographic_context = "proceedings|conference|journal|association for computing machinery|\\bdoi\\b|\\bet al\\.|\\bvolume\\s+\\d|\\bpages?\\s+\\d",

      privacy_policy_phrase = "privacy\\s+(?:policy|notice)|data\\s+protection\\s+(?:policy|notice)",

      weak_privacy_mention = "\\bprivacy\\b|\\bgdpr\\b|personal data|personal information|data subject",

      privacy_technology_evidence = paste0("privacy[- ]preserv|privacy of synthetic data|", "measur(?:e|es|ed|ing)[^.]{0,100}privacy|",
      "redact[^.]{0,100}(?:private|personal) data|", "(?:personal data|PII|privacy information)[^.]{0,100}leak|",
      "leak(?:ing|age)?[^.]{0,100}(?:personal data|PII|privacy information)"),

      privacy_criterion_context = paste0("privacy|gdpr|personal data|personal information|", "personally identifiable|\\bPII\\b|data subject|",
      "user data|customer data|private data|", "anonymous user data|license plate data"),

      privacy_path_context = paste0("security[-_ ]and[-_ ]privacy|privacy[-_ ]and[-_ ]security|", "(?:^|/)privacy(?:[-_/]|$)"),

      anonymous_data = "\\b(?:anonymous|anonymized|anonymised) (?:user |usage )?data\\b",

      anonymous_exclusions = paste0("personal|personally identifiable|\\bPII\\b|data subject|", "email address|ip address|cookies?|license plate|authentication credentials"),

      false_deletion_security = paste0("jwt|auth[_ ]manager|revoke[_ ]token|token expiration|cookie deletion"),

      false_deletion_personal_data = paste0("(?:personal|user|customer|account) data|data deleted|", "delete accounts?|retention"),

      false_sharing_software = paste0("shared librar|third[- ]party dependenc|", "share\\.sh|build_release.*share"),

      false_sharing_data = paste0("(?:share|transfer|disclos)[^.]{0,50}(?:personal|user|customer|usage|training|private)? ?data|",
      "(?:personal|user|customer|usage|training|private) data[^.]{0,50}(?:share|transfer|disclos)"),

      project_pronouns = paste0("\\b(?:we|our|ours|you|your|users?|visitors?|", "this (?:project|application|app|service|site|website|framework|library|tool)|",
      "the (?:project|application|app|service|site|website))\\b")
    ),

    document_catalog = NULL
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

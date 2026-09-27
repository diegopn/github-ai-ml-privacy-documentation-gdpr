ClassifierContract <- R6::R6Class(
  "ClassifierContract",
  public = list(
    run = function(context) {
      project <- context$project()
      classes <- context$classes()
      values <- context$values()
      classifier <- project$services$classifier
      documents <- list(list(
        path = "PRIVACY.md",
        status = 200L,
        commit_sha = "commit-a",
        text = "Our privacy policy explains that we collect personal data, including email addresses, to provide the service."
      ))
      positive <- classifier$classify(documents, "commit-a", "owner/project")
      weak <- classifier$classify(list(list(
        path = "README.md", status = 200L, text = "Privacy is a human right."
      )), "commit-b", "owner/project")
      context$check("classificador preserva evidência contextualizada para critérios C1–C7",
        identical(classifier$criterion_codes(), paste0("C", 1:7)) && positive$D1 == 1L &&
          positive$C1 == 1L && positive$score >= 1L && nzchar(positive$evidence$C1))
      context$check("menção isolada a privacidade não produz D1",
        weak$D1 == 0L && weak$score == 0L)
      excluded_paths <- c("tests/privacy-policy.md", "examples/privacy-policy.md", "datasets/privacy-policy.md")
      excluded_results <- lapply(excluded_paths, function(path) classifier$classify(list(list(
        path = path, status = 200L,
        text = "Our privacy policy explains that we collect personal data, including email addresses, to provide the service."
      )), "commit-excluded", "owner/project"))
      context$check("classificador exclui testes, exemplos e datasets mesmo quando chamado pelo coletor",
        all(vapply(excluded_results, function(result) result$D1 == 0L && result$score == 0L, logical(1L))) &&
          positive$D1 == 1L)
      catalog <- project$services$document_catalog
      discovered <- catalog$discover_candidates(list(tree = list(
        list(type = "blob", path = "docs/privacy-policy.md", sha = "a", size = 12L),
        list(type = "blob", path = "tests/privacy-policy.md", sha = "excluded", size = 20L),
        list(type = "blob", path = "README.md", sha = "b", size = 10L),
        list(type = "blob", sha = "missing-path"),
        list(type = "tree", path = "privacy", sha = "c")
      )))
      context$check("descoberta prioriza README raiz, encontra política e exclui caminhos de teste",
        identical(vapply(discovered, `[[`, character(1L), "path"), c("README.md", "docs/privacy-policy.md")) &&
          length(catalog$discover_candidates(list(tree = list(list(type = "blob", sha = "missing"))))) == 0L)
      bad_regex <- project$services$rule_set$rules()
      bad_regex$C1$primary <- "("
      bad_codes <- project$services$rule_set$rules()
      bad_codes$extra <- list(primary = "data", context = "privacy")
      context$check("classificador valida regex e rejeita códigos fora de C1–C7",
        context$errors(classes$PrivacyDocumentClassifier$new(project$services$rule_set, values, catalog, rules = bad_regex)) &&
          context$errors(classes$PrivacyDocumentClassifier$new(project$services$rule_set, values, catalog, rules = bad_codes)))
      services <- project$services
      context$check("bootstrap injeta os serviços R6 sem chamar a API",
        inherits(services$client, "GitHubClient") && inherits(services$http_transport, "GitHubHttpTransport") &&
          inherits(services$document_catalog, "PrivacyDocumentCatalog") &&
          inherits(services$selector, "SampleSelector") && inherits(services$collector, "RepositoryCollector") &&
          inherits(services$analyzer, "ExperimentAnalyzer") && inherits(services$site_content, "SiteContentWriter") && is.function(services$site_content$render))
      invisible(TRUE)
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

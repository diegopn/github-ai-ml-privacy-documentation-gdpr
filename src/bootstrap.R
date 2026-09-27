ProjectBootstrap <- R6::R6Class(
  "ProjectBootstrap",
  public = list(
    initialize = function(root) {
      if (!requireNamespace("R6", quietly = TRUE)) stop("Instale o pacote 'R6' para executar o projeto.", call. = FALSE)
      if (!is.character(root) || length(root) != 1L || !dir.exists(root)) stop("A raiz do projeto precisa ser uma pasta existente.", call. = FALSE)
      if (getRversion() < "4.6.0") stop("O projeto exige R 4.6 ou superior.", call. = FALSE)
      private$root <- normalizePath(root, mustWork = TRUE)
      invisible(self)
    },
    build = function() {
      classes <- private$load_classes()
      values <- classes$ValueTools$new()
      config <- classes$ProjectConfig$new(private$root, values)
      protocol <- classes$CollectionProtocol$new()
      runtime <- classes$SystemRuntime$new()
      lock_manager <- classes$ArtifactLock$new(runtime, values)
      store <- classes$ArtifactStore$new(config, values, protocol)
      limiter <- classes$GitHubRateLimiter$new(config, store, lock_manager, runtime, values)
      http_transport <- classes$GitHubHttpTransport$new(config, limiter, runtime, values)
      client <- classes$GitHubClient$new(config, values, http_transport)
      rule_set <- classes$PrivacyRuleSet$new()
      document_catalog <- classes$PrivacyDocumentCatalog$new(rule_set, values)
      classifier <- classes$PrivacyDocumentClassifier$new(rule_set, values, document_catalog)
      builder <- classes$DatasetBuilder$new(config, classifier, document_catalog, values)
      writer <- classes$ReportWriter$new(config, store, classifier)
      searcher <- classes$RepositorySearch$new(client, values)
      selector <- classes$SampleSelector$new(config, client, store, values, searcher)
      collector <- classes$RepositoryCollector$new(config, client, store, classifier, document_catalog, values, protocol)
      statistics <- classes$PairedStatistics$new(config, values, protocol)
      analyzer <- classes$ExperimentAnalyzer$new(config, store, builder, writer, statistics, classifier)
      cli <- classes$ExperimentCli$new()
      site_content <- classes$SiteContentWriter$new(config, store)
      runner <- classes$ExperimentRunner$new(config, store, lock_manager, selector, collector, analyzer, cli, values, site_content)
      services <- list(values = values, protocol = protocol, store = store, runtime = runtime,
        lock_manager = lock_manager, limiter = limiter,
        client = client, http_transport = http_transport, rule_set = rule_set, document_catalog = document_catalog,
        classifier = classifier, builder = builder, writer = writer,
        searcher = searcher, selector = selector, collector = collector, statistics = statistics,
        analyzer = analyzer, site_content = site_content)
      list(root = config$root(), config = config, classes = classes, services = services, cli = cli, runner = runner)
    }
  ),
  private = list(
    root = NULL,
    load_classes = function() {
      namespace <- new.env(parent = globalenv())
      files <- list.files(file.path(private$root, "src"), pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
      for (file in files[basename(files) != "bootstrap.R"]) sys.source(file, envir = namespace)
      namespace
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

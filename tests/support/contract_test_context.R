ContractTestContext <- R6::R6Class(
  "ContractTestContext",
  public = list(
    initialize = function(project_root) {
      if (!dir.exists(project_root)) stop("Raiz de teste inválida.", call. = FALSE)
      private$root_path <- normalizePath(project_root, mustWork = TRUE)
      private$old_timezone <- Sys.getenv("TZ", unset = NA_character_)
      Sys.setenv(TZ = "UTC")
      namespace <- new.env(parent = globalenv())
      sys.source(file.path(private$root_path, "src", "bootstrap.R"), envir = namespace)
      private$project_instance <- namespace$ProjectBootstrap$new(private$root_path)$build()
      private$temporary_paths <- character()
      private$passed <- 0L
      invisible(self)
    },
    project_root = function() private$root_path,
    project = function() private$project_instance,
    classes = function() private$project_instance$classes,
    services = function() private$project_instance$services,
    config = function() private$project_instance$config,
    store = function() private$project_instance$services$store,
    values = function() private$project_instance$services$values,
    protocol = function() private$project_instance$services$protocol,
    cli = function() private$project_instance$cli,
    check = function(label, condition) {
      if (!isTRUE(condition)) stop(sprintf("Falha no teste: %s", label), call. = FALSE)
      private$passed <- private$passed + 1L
      cat(sprintf("[OK] %s\n", label))
      invisible(TRUE)
    },
    errors = function(expression) inherits(tryCatch(force(expression), error = identity), "error"),
    track = function(path) {
      private$temporary_paths <- c(private$temporary_paths, path)
      path
    },
    new_test_project = function(api_settings = list()) {
      root <- self$track(tempfile("r6-test-project-"))
      dir.create(file.path(root, "config"), recursive = TRUE)
      dir.create(file.path(root, "inputs", "reference"), recursive = TRUE)
      file.copy(file.path(private$root_path, "config", "settings.yml"), file.path(root, "config", "settings.yml"))
      file.copy(file.path(private$root_path, "inputs", "reference", "osi_approved_spdx_ids.txt"),
        file.path(root, "inputs", "reference", "osi_approved_spdx_ids.txt"))
      settings_path <- file.path(root, "config", "settings.yml")
      settings <- yaml::read_yaml(settings_path)
      settings$api$max_attempts <- 2L
      settings$api$retry_base_seconds <- 1
      settings$api$retry_max_seconds <- 1
      settings$api$retry_budget_seconds <- 10
      settings$api$max_rate_wait_seconds <- 0
      settings$api$core_interval_seconds <- 0
      settings$api$search_interval_seconds <- 0
      settings$api$raw_interval_seconds <- 0
      settings$api$graphql_interval_seconds <- 0
      settings$api <- utils::modifyList(settings$api, api_settings)
      yaml::write_yaml(settings, settings_path)
      config <- private$project_instance$classes$ProjectConfig$new(root, self$values())
      store <- private$project_instance$classes$ArtifactStore$new(
        config, self$values(), self$protocol()
      )
      list(root = root, config = config, store = store)
    },
    complete_period = function(sha, period = "pre", documents = list()) {
      list(until = self$config()$analysis_cutoffs()[[period]], commit_request_status = 200L,
        commit = list(sha = sha, tree_sha = paste0(sha, "-tree")), tree_request_status = 200L,
        tree_truncated = FALSE, document_candidates = lapply(documents, function(document) list(path = document$path)),
        documents = documents)
    },
    make_repository = function(name, fork = FALSE, archived = FALSE,
                               created = "2018-05-24T23:59:59Z", stars = 500L,
                               license = "MIT") {
      list(
        full_name = name,
        html_url = paste0("https://github.com/", name),
        name = sub("^[^/]*/", "", name),
        owner = list(login = sub("/.*$", "", name)),
        description = "offline fixture",
        language = "R",
        visibility = "public",
        private = FALSE,
        fork = fork,
        archived = archived,
        created_at = created,
        updated_at = "2024-01-01T00:00:00Z",
        stargazers_count = stars,
        license = list(spdx_id = license, name = license)
      )
    },
    new_http_client = function(config, store, runtime, wire_transport = NULL) {
      classes <- private$project_instance$classes
      limiter <- classes$GitHubRateLimiter$new(config, store, self$services()$lock_manager, runtime, self$values())
      http_transport <- classes$GitHubHttpTransport$new(config, limiter, runtime, self$values(), wire_transport)
      client <- classes$GitHubClient$new(config, self$values(), http_transport)
      list(client = client, http_transport = http_transport, limiter = limiter)
    },
    summary = function() {
      cat(sprintf("\n%d verificações locais passaram; todos os testes usam APIs simuladas e fixtures geradas offline.\n",
        private$passed))
      invisible(private$passed)
    },
    cleanup = function() {
      if (length(private$temporary_paths)) unlink(private$temporary_paths, recursive = TRUE, force = TRUE)
      private$temporary_paths <- character()
      if (is.na(private$old_timezone)) Sys.unsetenv("TZ") else Sys.setenv(TZ = private$old_timezone)
      invisible(TRUE)
    }
  ),
  private = list(root_path = NULL, project_instance = NULL, passed = 0L, temporary_paths = character(), old_timezone = NA_character_),
  lock_class = TRUE,
  cloneable = FALSE
)

ConfigurationCliContract <- R6::R6Class(
  "ConfigurationCliContract",
  public = list(
    run = function(context) {
      config <- context$config()
      store <- context$store()
      cli <- context$cli()
      global_names <- c("PROJECT_CONFIG", "GITHUB_CLIENT", "PRE_UNTIL", "POST_UNTIL", "ALPHA")
      context$check("bootstrap não publica configuração nem cliente no ambiente global",
        !any(c(global_names, "SampleSelector", "RepositoryCollector") %in% ls(globalenv(), all.names = TRUE)))
      for (name in global_names) {
        makeActiveBinding(name, function(value) stop("estado global acessado", call. = FALSE), .GlobalEnv)
      }
      on.exit(rm(list = global_names, envir = .GlobalEnv), add = TRUE)
      context$check("instâncias usam dependências injetadas mesmo com nomes globais interceptados",
        all(identical(config$get("analysis", "alpha"), 0.05), store$sample_path() == config$path("sample")))
      context$check("configuração e store não expõem suas dependências como campos públicos",
        all(is.null(config$config), is.null(store$config)))
      context$check("configuração carrega os tópicos e parâmetros declarados no YAML",
        length(unlist(config$get("selection", "topics"), use.names = FALSE)) == 24L)
      context$check("CLI usa --run por padrão e aceita --help",
        all(identical(cli$parse_main_options(character())$mode, "run"), isTRUE(cli$parse_main_options("--help")$help)))
      context$check("CLI expõe testes e preparação do site sem alterar o modo padrão",
        all(identical(cli$parse_main_options("--test")$mode, "test"),
          identical(cli$parse_main_options("--prepare-site")$mode, "prepare-site")))
      context$check("CLI preserva a ordem das etapas de cada modo",
        all(identical(cli$pipeline_stages("run"), c("selection", "collection", "analysis", "site")),
          identical(cli$pipeline_stages("select"), c("selection", "collection")),
          identical(cli$pipeline_stages("analyze"), c("collection_check", "analysis", "site"))))
      context$check("CLI rejeita opções desconhecidas e modos incompatíveis",
        all(context$errors(cli$parse_main_options("--analyize")),
          context$errors(cli$parse_main_options(c("--run", "--select"))),
          context$errors(cli$parse_main_options("--resume"))))
      private$configuration_validation(context)
      invisible(TRUE)
    }
  ),
  private = list(
    configuration_validation = function(context) {
      project <- context$new_test_project()
      settings <- yaml::read_yaml(file.path(project$root, "config", "settings.yml"))
      invalid_numbers <- list(c(500, 600), list(values = c(500, 600)), NA_real_, Inf, TRUE, "invalid", -1, 0, NULL)
      context$check("configuração rejeita limites não escalares, listas, NA, infinito, booleanos e valores inválidos",
        all(vapply(invalid_numbers, function(value) {
          private$rejects(context, project$root, settings, "selection", "min_stars", value)
        }, logical(1L))))
      context$check("configuração rejeita contagens fracionárias ou maiores que o limite inteiro de R",
        all(private$rejects(context, project$root, settings, "api", "max_attempts", 1.5),
          private$rejects(context, project$root, settings, "selection", "min_stars", 2^31)))
      context$check("configuração valida alpha e intervalos da API individualmente",
        all(private$rejects(context, project$root, settings, "analysis", "alpha", c(0.01, 0.05)),
          private$rejects(context, project$root, settings, "api", "core_interval_seconds", c(0, 1)),
          private$rejects(context, project$root, settings, "api", "max_rate_wait_seconds", -1)))
      invalid_dates <- list("2018-05-24T99:99:99Z", "2018-02-30T23:59:59Z", "2019-02-29",
        "2018-05-24T23:59:59Zextra", "2018-05-24T24:00:00Z", c("2018-05-24", "2018-05-25"), NA_character_, NULL)
      context$check("configuração rejeita horários, calendários, sufixos e formatos de data inválidos",
        all(vapply(invalid_dates, function(value) {
          private$rejects(context, project$root, settings, "analysis", "pre_until", value)
        }, logical(1L))))
      context$check("configuração rejeita caminhos não escalares e tópicos ausentes ou aninhados",
        all(private$rejects(context, project$root, settings, "paths", "sample", c("one", "two")),
          private$rejects(context, project$root, settings, "selection", "topics", c("ai", NA_character_)),
          private$rejects(context, project$root, settings, "selection", "topics", list(list("ai", "ml")))))
      context$check("configuração rejeita janelas invertidas e limites incompatíveis entre si",
        all(private$rejects(context, project$root, settings, "analysis", "pre_until", "2027-01-01"),
          private$rejects(context, project$root, settings, "selection", "search_start_date", "2020-01-01"),
          private$rejects(context, project$root, settings, "selection", "max_results_per_query", 1001),
          private$rejects(context, project$root, settings, "api", "retry_max_seconds", 0.5),
          private$rejects(context, project$root, settings, "api", "lock_stale_seconds", 1)))
      settings$analysis$pre_until <- "2024-02-29T23:59:59Z"
      settings$selection$min_stars <- "500"
      yaml::write_yaml(settings, file.path(project$root, "config", "settings.yml"))
      config <- context$classes()$ProjectConfig$new(project$root, context$values())
      context$check("configuração preserva datas válidas, limites numéricos em texto e intervalos zero",
        all(config$get("analysis", "pre_until") == "2024-02-29T23:59:59Z",
          config$get("selection", "min_stars") == "500", config$get("api", "core_interval_seconds") == 0))
      settings$api$timeout_seconds <- NULL
      settings$analysis$post_until <- NULL
      yaml::write_yaml(settings, file.path(project$root, "config", "settings.yml"))
      config <- context$classes()$ProjectConfig$new(project$root, context$values())
      context$check("configuração conserva os defaults de opções omitidas",
        all(config$get("api", "timeout_seconds", 45) == 45,
          config$get("analysis", "post_until", "2026-06-30T23:59:59Z") == "2026-06-30T23:59:59Z"))
    },
    rejects = function(context, root, settings, section, key, value) {
      settings[[section]][key] <- list(value)
      yaml::write_yaml(settings, file.path(root, "config", "settings.yml"), precision = 17)
      context$errors(context$classes()$ProjectConfig$new(root, context$values()))
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

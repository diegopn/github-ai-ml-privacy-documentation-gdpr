ArchitectureContract <- R6::R6Class(
  "ArchitectureContract",
  public = list(
    run = function(context) {
      source_files <- list.files(file.path(context$project_root(), "src"), pattern = "\\.R$",
        recursive = TRUE, full.names = TRUE)
      test_files <- list.files(file.path(context$project_root(), "tests"), pattern = "\\.R$",
        recursive = TRUE, full.names = TRUE)
      source_ok <- all(vapply(source_files, private$is_single_r6_class, logical(1L)))
      test_ok <- all(vapply(test_files, private$is_single_r6_class, logical(1L)))
      context$check("cada arquivo R de src e tests contém somente uma declaração de classe R6",
        source_ok && test_ok)
      page <- readLines(file.path(context$project_root(), "index.qmd"), warn = FALSE)
      context$check("a página Quarto usa artefatos sem executar código R procedural",
        !any(grepl("```{r", page, fixed = TRUE)) && !any(grepl("`r ", page, fixed = TRUE)))
      text <- unlist(lapply(source_files, readLines, warn = FALSE), use.names = FALSE)
      forbidden <- c("%||%", "bootstrap_project <- function", "CLI_MODES <-", "PIPELINE_STAGES <-",
        "write_file = function", " transport = function", " site_renderer = function")
      context$check("produção não mantém funções globais, callbacks de serviço nem constantes de pipeline",
        !any(vapply(forbidden, grepl, logical(1L), x = paste(text, collapse = "\n"), fixed = TRUE)))
      context$check("serviços e doubles usados pela suíte são objetos R6",
        inherits(context$services()$client, "GitHubClient") && inherits(context$services()$http_transport, "GitHubHttpTransport") &&
          inherits(context$services()$document_catalog, "PrivacyDocumentCatalog") &&
          inherits(MockSelectionClient$new(character(), list(), list()), "MockSelectionClient") &&
          inherits(MockCollectionClient$new(), "MockCollectionClient") && inherits(MockHttpTransport$new("retry"), "MockHttpTransport"))
      invisible(TRUE)
    }
  ),
  private = list(
    is_r6_assignment = function(expression) {
      is.call(expression) && identical(expression[[1L]], as.name("<-")) && is.symbol(expression[[2L]]) &&
        is.call(expression[[3L]]) && identical(paste(deparse(expression[[3L]][[1L]]), collapse = ""), "R6::R6Class")
    },
    is_single_r6_class = function(path) {
      expressions <- as.list(parse(path))
      length(expressions) == 1L && private$is_r6_assignment(expressions[[1L]])
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

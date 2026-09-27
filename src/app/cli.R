ExperimentCli <- R6::R6Class(
  "ExperimentCli",
  public = list(
    usage_text = function() {
      paste(
        "Uso: Rscript main.R [opção]", "",
        "Sem opção, executa o pipeline completo (--run).", "", "Opções:",
        "  --run      seleção, coleta, análise e geração do site",
        "  --select   nova amostra e todas as coletas históricas",
        "  --analyze  análise offline e geração do site após --select",
        "  --test     testes contratuais offline",
        "  --prepare-site  atualiza o conteúdo local antes do Quarto",
        "  --help     mostra esta mensagem",
        sep = "\n"
      )
    },
    parse_main_options = function(args) {
      options <- list(mode = "run", help = FALSE)
      if (identical(args, "--help")) {
        options$help <- TRUE
        return(options)
      }
      if (!length(args)) return(options)
      valid_modes <- names(private$modes)
      if (length(args) > 1L && all(args %in% valid_modes)) {
        stop(sprintf("Escolha apenas um modo: %s", paste(args, collapse = ", ")), call. = FALSE)
      }
      if (length(args) != 1L || !args[[1L]] %in% valid_modes) {
        stop(sprintf("Opção desconhecida ou combinação inválida: %s\n\n%s",
          paste(args, collapse = " "), self$usage_text()), call. = FALSE)
      }
      options$mode <- unname(private$modes[[args[[1L]]]])
      options
    },
    pipeline_stages = function(mode) {
      stages <- private$stages[[mode]]
      if (is.null(stages)) stop(sprintf("Modo não suportado: %s", mode), call. = FALSE)
      unname(stages)
    },
    stage_label = function(stage) {
      label <- private$labels[[stage]]
      if (is.null(label)) stop(sprintf("Etapa desconhecida: %s", stage), call. = FALSE)
      label
    }
  ),
  private = list(
    modes = c("--run" = "run", "--select" = "select", "--analyze" = "analyze",
      "--test" = "test", "--prepare-site" = "prepare-site"),
    stages = list(
      run = c("selection", "collection", "analysis", "site"),
      select = c("selection", "collection"),
      analyze = c("collection_check", "analysis", "site")
    ),
    labels = c(
      selection = "seleção da amostra",
      collection = "coleta histórica nova",
      collection_check = "validação da coleta",
      analysis = "análise offline",
      site = "publicação Quarto"
    )
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

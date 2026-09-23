CLI_MODES <- c(
  "--run" = "run",
  "--select" = "select",
  "--analyze" = "analyze"
)

PIPELINE_STAGES <- list(
  run = c("selection", "collection", "analysis", "site"),
  select = c("selection", "collection"),
  analyze = c("collection_check", "analysis", "site")
)

STAGE_LABELS <- c(
  selection = "seleção da amostra",
  collection = "coleta histórica nova",
  collection_check = "validação da coleta",
  analysis = "análise offline",
  site = "publicação Quarto"
)

usage_text <- function() {
  paste(
    "Uso: Rscript main.R [opção]",
    "",
    "Sem opção, executa o pipeline completo (--run).",
    "",
    "Opções:",
    "  --run      seleção, coleta, análise e geração do site",
    "  --select   nova amostra e todas as coletas históricas",
    "  --analyze  análise offline e geração do site após --select",
    "  --help     mostra esta mensagem",
    sep = "\n"
  )
}

parse_main_options <- function(args) {
  values <- list(mode = "run", help = FALSE)
  if (identical(args, "--help")) {
    values$help <- TRUE
    return(values)
  }
  if (!length(args)) return(values)
  if (length(args) > 1L && all(args %in% names(CLI_MODES))) {
    stop(sprintf("Escolha apenas um modo: %s", paste(args, collapse = ", ")), call. = FALSE)
  }
  if (length(args) != 1L || !args[[1L]] %in% names(CLI_MODES)) {
    stop(sprintf("Opção desconhecida ou combinação inválida: %s\n\n%s", paste(args, collapse = " "), usage_text()), call. = FALSE)
  }
  values$mode <- unname(CLI_MODES[[args[[1L]]]])
  values
}

pipeline_stages <- function(mode) {
  stages <- PIPELINE_STAGES[[mode]]
  if (is.null(stages)) stop(sprintf("Modo não suportado: %s", mode), call. = FALSE)
  unname(stages)
}

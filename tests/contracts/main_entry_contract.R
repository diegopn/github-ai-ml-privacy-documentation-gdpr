MainEntryContract <- R6::R6Class(
  "MainEntryContract",
  public = list(
    run = function(context) {
      output <- system2("Rscript", c("main.R", "--help"), stdout = TRUE, stderr = TRUE)
      context$check("main.R compõe os serviços e mostra ajuda sem iniciar consultas",
        all(any(grepl("--run", output, fixed = TRUE)), any(grepl("--analyze", output, fixed = TRUE))))
      namespace <- new.env(parent = globalenv())
      original_directory <- getwd()
      messages <- capture.output(sys.source(file.path(context$project_root(), "tests", "test_contracts.R"), envir = namespace))
      context$check("carregar a classe de testes não executa testes nem muda o diretório",
        all(!length(messages), identical(getwd(), original_directory), inherits(namespace$ContractTestRunner, "R6ClassGenerator")))
      invisible(TRUE)
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

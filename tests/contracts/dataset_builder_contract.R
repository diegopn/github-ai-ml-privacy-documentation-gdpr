DatasetBuilderContract <- R6::R6Class(
  "DatasetBuilderContract",
  public = list(
    run = function(context) {
      classifier <- context$services()$classifier
      documents <- list(list(
        path = "PRIVACY.md",
        status = 200L,
        commit_sha = "pre",
        text = "Our privacy policy says we collect personal data, including email addresses, to provide the service."
      ))
      cached <- list(C1 = 0L, C2 = 0L, C3 = 0L, C4 = 0L, C5 = 0L, C6 = 0L, C7 = 0L,
        D1 = 0L, score = 0L, D1_evidence = "", note = "cached", evidence = list())
      sample <- data.frame(repository = "owner/project", stringsAsFactors = FALSE)
      record <- list(repository = "owner/project",
        pre = list(commit = list(sha = "pre"), documents = documents,
          classification_rule_version = classifier$rule_version(), classification = cached),
        post = list(commit = list(sha = "post"), documents = documents,
          classification_rule_version = classifier$rule_version(), classification = cached))
      builder <- context$classes()$DatasetBuilder$new(context$config(), classifier,
        context$services()$document_catalog, context$values())
      cached_data <- builder$build(sample, list(record), "fixture-hash")$data
      recalculated <- builder$reclassify(sample, list(record), "fixture-hash")$data
      context$check("reclassificação ignora o cache e preserva os registros recebidos",
        all(recalculated$pre_C1 == "1", identical(record$pre$classification, cached)))
      malformed_cache <- cached
      malformed_cache$C1 <- 1L
      malformed_cache$score <- 1L
      malformed_record <- record
      malformed_record$pre$classification <- malformed_cache
      malformed_data <- builder$build(sample, list(malformed_record), "fixture-hash")$data
      invalid_binary_record <- record
      invalid_binary_record$pre$classification$C1 <- c(0L, 0L)
      invalid_binary_record$pre$classification["C2"] <- list(NULL)
      invalid_binary <- builder$build(sample, list(invalid_binary_record), "fixture-hash")$data
      context$check("cache exige um valor escalar por critério e reclassifica estruturas inválidas",
        all(invalid_binary$pre_C1 == "1", nzchar(invalid_binary$pre_C1_evidence)))
      legacy_record <- record
      legacy_record$pre$classification_rule_version <- "semantic-conservative-2026-09-c1-c4"
      legacy_data <- builder$build(sample, list(legacy_record), "fixture-hash")$data
      unknown_tree_data <- builder$build(sample, list(record), "fixture-hash")$data
      context$check("dataset valida cache por versão/evidência e trata árvore sem estado como truncada",
        all(nrow(cached_data) == 1L, ncol(cached_data) == 82L, cached_data$pre_D1 == "0",
          recalculated$pre_D1 == "1", recalculated$pre_C1 == "1",
          malformed_data$pre_C1 == "1", nzchar(malformed_data$pre_C1_evidence),
          legacy_data$pre_D1 == "1", legacy_data$pre_C1 == "1",
          unknown_tree_data$pre_tree_truncated == "TRUE", unknown_tree_data$post_tree_truncated == "TRUE"))
      inconsistent <- record
      inconsistent$pre$classification$C1 <- 1L
      inconsistent$pre$classification$score <- 1L
      inconsistent$pre$classification$evidence$C1 <- "cached positive"
      rebuilt <- builder$build(sample, list(inconsistent), "fixture-hash")$data
      context$check("cache com critério positivo e D1 zero é recalculado", rebuilt$pre_D1 == "1")
      malformed <- record
      malformed$pre$classification$note <- c("one", "two")
      malformed$post$classification$C1 <- list(0L)
      rebuilt <- builder$build(sample, list(malformed), "fixture-hash")$data
      context$check("cache rejeita textos vetoriais e números encapsulados em listas",
        all(rebuilt$pre_D1 == "1", rebuilt$post_D1 == "1"))
      invisible(TRUE)
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

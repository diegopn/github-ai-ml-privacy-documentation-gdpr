CollectionProtocolContract <- R6::R6Class(
  "CollectionProtocolContract",
  public = list(
    run = function(context) {
      record <- list(pre = context$complete_period("pre"), post = context$complete_period("post", "post"))
      protocol <- context$protocol()
      context$check("coleta com árvore válida e nenhum candidato continua analisável", protocol$record_analyzable(record))
      invalid <- record
      invalid$pre$error <- "árvore malformada"
      context$check("erro de coleta invalida o par mesmo com HTTP 200", !protocol$record_complete(invalid))
      invalid <- record
      invalid$pre$documents <- list(list(path = "PRIVACY.md", status = 503L, text = ""))
      invalid$pre$document_candidates <- list(list(path = "PRIVACY.md"))
      context$check("falha no download não é confundida com ausência de evidência", !protocol$record_analyzable(invalid))
      invalid$pre$documents <- list()
      context$check("todos os candidatos precisam ter um documento correspondente", !protocol$record_complete(invalid))
      invalid$pre$documents <- list(list(path = "OTHER.md", status = 200L, text = ""))
      context$check("documento de outro caminho não completa o candidato esperado", !protocol$record_complete(invalid))
      invalid <- record
      invalid$pre$tree_truncated <- "false"
      context$check("estado desconhecido da árvore não é convertido em árvore completa", !protocol$record_analyzable(invalid))
      invalid$pre <- "invalid"
      context$check("estruturas inválidas de coleta são rejeitadas sem erro de coerção",
        !protocol$record_complete(invalid) && !protocol$record_complete(NULL) && !protocol$record_complete("invalid"))
      invisible(TRUE)
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

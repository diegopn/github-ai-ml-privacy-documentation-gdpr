SiteContentContract <- R6::R6Class(
  "SiteContentContract",
  public = list(
    run = function(context) {
      project <- context$new_test_project()
      settings_path <- file.path(project$root, "config", "settings.yml")
      settings <- yaml::read_yaml(settings_path)
      settings$paths$output_root <- "resultados locais"
      yaml::write_yaml(settings, settings_path)
      project$config <- context$classes()$ProjectConfig$new(project$root, context$values())
      project$store <- context$classes()$ArtifactStore$new(project$config, context$values(), context$protocol())
      dir.create(file.path(project$root, "site"))
      file.copy(file.path(context$project_root(), "site", "index-template.md"), file.path(project$root, "site", "index-template.md"))
      fixture <- OfflineExperimentFixture$new(context)$build()
      fixture$records[[1L]]$post$tree_truncated <- TRUE
      fixture$stats <- context$services()$statistics$calculate(fixture$records, fixture$dataset,
        fixture$sample, fixture$input_path, context$services()$classifier$criterion_codes(),
        context$services()$classifier$rule_version())
      project$store$publish_sample(fixture$sample, list(protocol_version = "offline-test"))
      fixture$source_hash <- project$store$hash_file(project$store$sample_path())
      fixture$stats$source_sha256 <- fixture$source_hash
      fixture$stats$significance_level <- 0.01
      paths <- project$config$output_paths()
      writer <- context$classes()$ReportWriter$new(project$config, project$store, context$services()$classifier)
      writer$write_all(paths, fixture$stats, fixture$sample, fixture$source_hash,
        fixture$dataset, fixture$input_path, fixture$raw_path)
      site <- context$classes()$SiteContentWriter$new(project$config, project$store)
      page <- paste(readLines(site$write(), warn = FALSE), collapse = "\n")
      transition <- utils::read.csv(file.path(paths$tables, "d1_transition_table.csv"), row.names = 1L)
      names(transition) <- c("Pós = 0", "Pós = 1")
      rownames(transition) <- c("Pré = 0", "Pré = 1")
      expected <- paste(knitr::kable(transition, format = "html", caption = "Matriz de transição de D1"), collapse = "\n")
      context$check("site usa a tabela publicada e a mesma população pareada das estatísticas",
        grepl(expected, page, fixed = TRUE) && grepl('"complete_pairs":4', page, fixed = TRUE) &&
          grepl('"sample_rows":5', page, fixed = TRUE))
      context$check("site substitui os dados sem emitir código R ou marcadores pendentes",
        !grepl("`r ", page, fixed = TRUE) && !grepl("```{r", page, fixed = TRUE) &&
          !grepl("\\{\\{[a-z_]+\\}\\}", page) && grepl("0,750", page, fixed = TRUE))
      context$check("site usa caminhos configurados e parâmetros estatísticos dos artefatos",
        grepl("resultados%20locais/figures/d1_pre_post.png", page, fixed = TRUE) &&
          grepl("alpha=0{,}01", page, fixed = TRUE))
      output <- site$write()
      Sys.setFileTime(output, Sys.time() - 3600)
      original_mtime <- file.info(output)$mtime
      site$write()
      context$check("preparar o mesmo site preserva o arquivo e evita disparar novamente o preview",
        identical(file.info(output)$mtime, original_mtime))
      unlink(project$store$audit_path())
      page_without_audit <- paste(readLines(site$write(), warn = FALSE), collapse = "\n")
      context$check("site mantém o texto alternativo quando não existe manifesto de seleção",
        grepl("O manifesto das consultas de seleção será criado", page_without_audit, fixed = TRUE))
      output_hash <- project$store$hash_file(output)
      template_path <- file.path(project$root, "site", "index-template.md")
      cat("\n{{unknown_marker}}\n", file = template_path, append = TRUE)
      context$check("marcadores sem valor impedem a publicação e preservam a página anterior",
        context$errors(site$write()) && identical(output_hash, project$store$hash_file(output)))
      project$store$write_csv(fixture$sample[-1L, ], project$store$sample_path())
      context$check("site rejeita estatísticas de outra amostra sem substituir a página anterior",
        context$errors(site$write()) && identical(output_hash, project$store$hash_file(output)))
      invisible(TRUE)
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

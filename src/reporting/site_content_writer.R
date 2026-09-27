SiteContentWriter <- R6::R6Class(
  "SiteContentWriter",
  public = list(
    initialize = function(config, store) {
      if (!inherits(config, "ProjectConfig")) stop("SiteContentWriter exige ProjectConfig.", call. = FALSE)
      if (!inherits(store, "ArtifactStore")) stop("SiteContentWriter exige ArtifactStore.", call. = FALSE)
      private$config <- config
      private$store <- store
      invisible(self)
    },
    render = function() {
      quarto <- Sys.which("quarto")
      if (!nzchar(quarto)) stop("Quarto não encontrado no PATH; ele é necessário para gerar o site.", call. = FALSE)
      project_root <- private$config$root()
      previous_directory <- getwd()
      on.exit(setwd(previous_directory), add = TRUE)
      setwd(project_root)
      output <- suppressWarnings(system2(quarto, "render", stdout = TRUE, stderr = TRUE))
      exit_status <- attr(output, "status")
      if (!is.null(exit_status) && exit_status != 0L) {
        stop(paste(c("Não foi possível gerar o site com o Quarto.", utils::tail(output, 20L)), collapse = "\n"), call. = FALSE)
      }
      cat("Site gerado.\n")
      invisible(project_root)
    },
    write = function() {
      paths <- private$config$output_paths()
      stats <- jsonlite::read_json(file.path(paths$metadata, "statistics.json"), simplifyVector = FALSE)
      sample <- private$store$read_sample()
      topics <- as.character(unlist(stats$selection_topics, use.names = FALSE))
      audit_available <- file.exists(private$store$audit_path())
      private$assert_current_results(stats, sample)
      if (audit_available) private$store$assert_published_sample()
      values <- c(private$metric_values(stats, sample, topics, audit_available),
        private$table_values(paths, sample), private$manifest_values(audit_available), private$configuration_values(stats))
      template <- readLines(file.path(private$config$root(), "site", "index-template.md"), warn = FALSE)
      for (name in names(values)) {
        template <- gsub(paste0("{{", name, "}}"), as.character(values[[name]]), template, fixed = TRUE)
      }
      if (any(grepl("\\{\\{[a-z_]+\\}\\}", template))) {
        stop("O template do site contém marcadores sem valor.", call. = FALSE)
      }
      output <- file.path(private$config$root(), "index.qmd")
      if (file.exists(output)) {
        current <- paste(readLines(output, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
        if (identical(current, paste(template, collapse = "\n"))) return(invisible(output))
      }
      private$store$write_lines(template, output)
      invisible(output)
    }
  ),
  private = list(
    config = NULL,
    store = NULL,
    assert_current_results = function(stats, sample) {
      if (!identical(stats$source_sha256, private$store$hash_file(private$store$sample_path())) ||
        !identical(as.integer(stats$total_input_rows), nrow(sample))) {
        stop("As estatísticas não pertencem à amostra atual. Execute --analyze antes de gerar o site.", call. = FALSE)
      }
    },
    configuration_values = function(stats) {
      selection <- private$config$selection_settings()
      list(
        output_root = private$artifact_url(private$config$path("output_root")),
        sample_path = private$artifact_url(private$store$sample_path()),
        raw_path = private$artifact_url(private$store$raw_path()),
        pre_until = stats$pre_until, post_until = stats$post_until,
        gdpr_date = selection$gdpr_date, min_stars = stats$selection_min_stars,
        min_issues = stats$selection_min_issues, min_activity_months = stats$selection_min_activity_months,
        search_limit = selection$max_results_per_query,
        alpha = gsub(".", "{,}", format(stats$significance_level, scientific = FALSE, trim = TRUE), fixed = TRUE)
      )
    },
    artifact_url = function(path) {
      utils::URLencode(private$config$relative(path), repeated = TRUE)
    },
    format_p = function(value) {
      value <- as.numeric(value)
      if (!is.finite(value)) return("NA")
      if (value > 0 && value < 0.001) {
        return(formatC(value, format = "e", digits = 2L, decimal.mark = ","))
      }
      formatC(value, format = "f", digits = 6L, decimal.mark = ",", big.mark = ".")
    },
    format_score = function(value) formatC(as.numeric(value), format = "f", digits = 3L, decimal.mark = ","),
    metric_values = function(stats, sample, topics, audit_available) {
      r1 <- stats$rq1_mcnemar
      r2 <- stats$rq2_wilcoxon
      w <- r2$wilcoxon_signed_rank_exact
      site_data <- list(
        topic_count = length(topics), sample_rows = nrow(sample), complete_pairs = as.integer(stats$complete_pairs),
        pre_d1 = as.integer(r1$pre_ones), post_d1 = as.integer(r1$post_ones),
        pre_score = as.numeric(r2$pre_mean), post_score = as.numeric(r2$post_mean),
        mcnemar_p = as.numeric(r1$exact_mcnemar_p_two_sided), wilcoxon_p = as.numeric(w$p_two_sided_exact),
        selection_audit_available = audit_available
      )
      c(list(
        site_data = paste0("<script>window.privacySiteData=",
          jsonlite::toJSON(site_data, auto_unbox = TRUE, digits = 16), ";</script>"),
        total = as.integer(stats$total_input_rows), complete_pairs = stats$complete_pairs,
        pre_d1 = r1$pre_ones, post_d1 = r1$post_ones, sample_rows = nrow(sample), topic_count = length(topics),
        pre_proportion = sprintf("%.1f%%", as.numeric(r1$pre_proportion) * 100),
        post_proportion = sprintf("%.1f%%", as.numeric(r1$post_proportion) * 100),
        losses = r1$pre1_post0_b, gains = r1$pre0_post1_c,
        mcnemar_p = private$format_p(r1$exact_mcnemar_p_two_sided), wilcoxon_p = private$format_p(w$p_two_sided_exact)
      ), private$score_values(r2))
    },
    score_values = function(result) {
      w <- result$wilcoxon_signed_rank_exact
      list(
        pre_score = private$format_score(result$pre_mean), post_score = private$format_score(result$post_mean),
        pre_mean = sprintf("%.3f", as.numeric(result$pre_mean)), post_mean = sprintf("%.3f", as.numeric(result$post_mean)),
        pre_median = sprintf("%.3f", as.numeric(result$pre_median)), post_median = sprintf("%.3f", as.numeric(result$post_median)),
        pre_iqr = paste(result$pre_iqr, collapse = "–"), post_iqr = paste(result$post_iqr, collapse = "–"),
        increased = result$increased, decreased = result$decreased, unchanged = result$unchanged,
        w_plus = sprintf("%.1f", as.numeric(w$w_plus)), w_minus = sprintf("%.1f", as.numeric(w$w_minus)),
        rank_biserial = sprintf("%.3f", as.numeric(w$rank_biserial))
      )
    },
    table_values = function(paths, sample) {
      topic_values <- unlist(strsplit(as.character(sample$ai_ml_match_topics), "\\s*;\\s*"), use.names = FALSE)
      counts <- sort(table(topic_values[nzchar(topic_values)]), decreasing = TRUE)
      topic_table <- data.frame(Tópico = names(counts), Repositórios = as.integer(counts), check.names = FALSE)
      criteria <- utils::read.csv(file.path(paths$tables, "criterion_frequencies.csv"), check.names = FALSE)
      names(criteria) <- c("Critério", "Pré", "Pós")
      transition <- utils::read.csv(file.path(paths$tables, "d1_transition_table.csv"), row.names = 1L)
      names(transition) <- c("Pós = 0", "Pós = 1")
      rownames(transition) <- c("Pré = 0", "Pré = 1")
      list(
        topic_table = private$html_table(topic_table, "Tópicos presentes na amostra congelada"),
        criteria_table = private$html_table(criteria, "Frequência de C1 a C7"),
        transition_table = private$html_table(transition, "Matriz de transição de D1")
      )
    },
    html_table = function(data, caption) {
      paste(knitr::kable(data, format = "html", caption = caption), collapse = "\n")
    },
    manifest_values = function(available) {
      if (available) {
        return(list(
          manifest_link = sprintf("[Manifesto das consultas de seleção](%s)", private$artifact_url(private$store$audit_path())),
          manifest_status = "O manifesto desta execução está disponível nos artefatos."
        ))
      }
      list(
        manifest_link = "O manifesto das consultas de seleção será criado na próxima seleção online.",
        manifest_status = "O manifesto será produzido quando a seleção da amostra for executada com um token do GitHub."
      )
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

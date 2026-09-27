StatisticsContract <- R6::R6Class(
  "StatisticsContract",
  public = list(
    run = function(context) {
      private$fixture_statistics(context)
      private$wilcoxon_compatibility(context)
      private$single_pair_and_population(context)
      private$invalid_classifications(context)
      private$random_state(context)
      private$large_mcnemar(context)
      private$source_provenance(context)
      invisible(TRUE)
    }
  ),
  private = list(
    source_provenance = function(context) {
      fixture <- OfflineExperimentFixture$new(context)$build()
      for (index in seq_along(fixture$records)) fixture$records[[index]]$post$tree_truncated <- TRUE
      stats <- context$services()$statistics$calculate(fixture$records, fixture$dataset, fixture$sample,
        fixture$input_path, context$services()$classifier$criterion_codes(), context$services()$classifier$rule_version())
      context$check("hash de origem é preservado mesmo quando nenhum par pode entrar nos testes",
        stats$complete_pairs == 0L && identical(stats$source_sha256, "fixture-hash"))
      fixture$dataset$sample_source_sha256[[1L]] <- "other-hash"
      context$check("estatística rejeita dataset que mistura hashes de amostras diferentes",
        context$errors(context$services()$statistics$calculate(fixture$records, fixture$dataset, fixture$sample,
          fixture$input_path, context$services()$classifier$criterion_codes(), context$services()$classifier$rule_version())))
    },
    wilcoxon_compatibility = function(context) {
      fixture <- OfflineExperimentFixture$new(context)$build()
      cases <- as.matrix(expand.grid(rep(list(-2:2), 5L)))
      # Wilcoxon is invariant to pair order: evaluate each multiset through calculate().
      ordered <- t(apply(cases, 1L, sort))
      keys <- apply(ordered, 1L, paste, collapse = ",")
      distinct <- which(!duplicated(keys))
      calculated <- vapply(distinct, function(index) {
        private$difference_statistics(context, fixture, ordered[index, ])$p_two_sided_exact
      }, numeric(1L))
      names(calculated) <- keys[distinct]
      expected <- apply(cases, 1L, private$enumerated_probability)
      context$check("API pública do Wilcoxon coincide com enumeração exata em 3.125 casos, incluindo zeros e empates",
        isTRUE(all.equal(unname(calculated[keys]), unname(expected))))
      untied <- c(-7, -3, 1, 2, 5)
      context$check("Wilcoxon sem empates coincide com stats::wilcox.test",
        isTRUE(all.equal(private$difference_statistics(context, fixture, untied)$p_two_sided_exact,
          stats::wilcox.test(untied, exact = TRUE)$p.value)))
    },
    difference_statistics = function(context, fixture, differences) {
      criteria <- context$services()$classifier$criterion_codes()
      for (period in c("pre", "post")) {
        scores <- if (period == "pre") pmax(-differences, 0) else pmax(differences, 0)
        fixture$dataset[[paste0(period, "_D1")]] <- as.character(as.integer(scores > 0))
        fixture$dataset[[paste0(period, "_score")]] <- as.character(scores)
        for (index in seq_along(criteria)) {
          fixture$dataset[[paste0(period, "_", criteria[[index]])]] <- as.character(as.integer(scores >= index))
        }
      }
      context$services()$statistics$calculate(fixture$records, fixture$dataset, fixture$sample,
        fixture$input_path, criteria, context$services()$classifier$rule_version())$rq2_wilcoxon$wilcoxon_signed_rank_exact
    },
    enumerated_probability = function(differences) {
      values <- differences[differences != 0]
      if (!length(values)) return(1)
      ranks <- rank(abs(values), ties.method = "average")
      w_plus <- sum(ranks[values > 0])
      signs <- as.matrix(expand.grid(rep(list(c(0, 1)), length(values))))
      sums <- as.numeric(signs %*% ranks)
      mean(abs(sums - sum(ranks) / 2) >= abs(w_plus - sum(ranks) / 2))
    },
    invalid_classifications = function(context) {
      fixture <- OfflineExperimentFixture$new(context)$build()
      fixture$dataset$pre_C1[[1L]] <- NA_character_
      fixture$dataset$post_score[[2L]] <- "0.5"
      fixture$dataset$post_C2[[3L]] <- "not-a-number"
      fixture$dataset$post_D1[[4L]] <- "2"
      stats <- context$services()$statistics$calculate(fixture$records, fixture$dataset,
        fixture$sample, fixture$input_path, context$services()$classifier$criterion_codes(),
        context$services()$classifier$rule_version())
      context$check("validação vetorial exclui NA, score inconsistente, texto e D1 fora do domínio",
        stats$complete_pairs == 1L && stats$incomplete_pairs == 4L &&
          identical(stats$incomplete_repositories, fixture$sample$repository[1:4]))
      empty <- context$services()$statistics$calculate(list(), fixture$dataset[FALSE, ],
        fixture$sample[FALSE, ], fixture$input_path, context$services()$classifier$criterion_codes(),
        context$services()$classifier$rule_version())
      context$check("análise com população vazia conserva esquema e retornos definidos",
        empty$complete_pairs == 0L && empty$rq1_mcnemar$exact_mcnemar_p_two_sided == 1 &&
          all(is.na(empty$rq2_wilcoxon$difference_ci95_bootstrap_median)))
    },
    fixture_statistics = function(context) {
      fixture <- OfflineExperimentFixture$new(context)$build()
      stats <- fixture$stats
      context$check("estatísticas usam pares completos e coincidem com o McNemar binomial exato",
        stats$total_input_rows == 5L && stats$complete_pairs == 5L && stats$incomplete_pairs == 0L &&
          stats$rq1_mcnemar$pre1_post0_b == 1L && stats$rq1_mcnemar$pre0_post1_c == 3L &&
          isTRUE(all.equal(stats$rq1_mcnemar$exact_mcnemar_p_two_sided, stats::binom.test(1, 4)$p.value)))
      wilcoxon <- stats$rq2_wilcoxon$wilcoxon_signed_rank_exact
      context$check("Wilcoxon exato preserva postos médios, sinais, zeros e diferenças",
        wilcoxon$n_nonzero == 3L && wilcoxon$w_plus == 4.5 && wilcoxon$w_minus == 1.5 &&
          wilcoxon$rank_biserial == 0.5 && wilcoxon$p_two_sided_exact == 0.75 &&
          wilcoxon$zero_differences_excluded == 2L)
    },
    single_pair_and_population = function(context) {
      analysis <- context$classes()$PairedStatistics$new(
        context$config(), context$values(), context$protocol()
      )
      classifier <- context$services()$classifier
      one_sample <- data.frame(repository = "owner/one", stringsAsFactors = FALSE)
      one_record <- list(repository = "owner/one",
        pre = context$complete_period("pre"), post = context$complete_period("post", "post"))
      one_dataset <- data.frame(repository = "owner/one", pre_D1 = "0", post_D1 = "1", pre_score = "0", post_score = "3",
        sample_source_sha256 = "one-hash", stringsAsFactors = FALSE)
      for (code in classifier$criterion_codes()) {
        one_dataset[[paste0("pre_", code)]] <- "0"
        one_dataset[[paste0("post_", code)]] <- "0"
      }
      one_dataset$post_C1 <- "1"
      one_dataset$post_C2 <- "1"
      one_dataset$post_C3 <- "1"
      one_result <- analysis$calculate(list(one_record), one_dataset, one_sample, context$store()$sample_path(),
        classifier$criterion_codes(), classifier$rule_version())
      context$check("bootstrap com um par não reinterpreta um escalar como intervalo 1:n",
        identical(as.numeric(one_result$rq2_wilcoxon$difference_ci95_bootstrap_median), c(3, 3)))
      misaligned_dataset <- one_dataset
      misaligned_dataset$repository <- "owner/other"
      context$check("estatística rejeita dataset fora da ordem ou identidade da amostra",
        context$errors(analysis$calculate(list(one_record), misaligned_dataset, one_sample,
          context$store()$sample_path(), classifier$criterion_codes(), classifier$rule_version())))
      truncated_record <- one_record
      truncated_record$post$tree_truncated <- TRUE
      truncated_result <- analysis$calculate(list(truncated_record), one_dataset, one_sample, context$store()$sample_path(),
        classifier$criterion_codes(), classifier$rule_version())
      context$check("par com árvore Git truncada é excluído da análise de evidência",
        truncated_result$complete_pairs == 0L && truncated_result$incomplete_pairs == 1L &&
          identical(truncated_result$incomplete_repositories, "owner/one"))
      unknown_tree_record <- one_record
      unknown_tree_record$post$tree_truncated <- NULL
      unknown_tree_result <- analysis$calculate(list(unknown_tree_record), one_dataset, one_sample,
        context$store()$sample_path(), classifier$criterion_codes(), classifier$rule_version())
      context$check("par sem informação sobre truncamento é excluído de forma conservadora",
        unknown_tree_result$complete_pairs == 0L && unknown_tree_result$incomplete_pairs == 1L)
    },
    random_state = function(context) {
      fixture <- OfflineExperimentFixture$new(context)$build()
      analysis <- context$services()$statistics
      classifier <- context$services()$classifier
      had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv) else NULL
      on.exit({
        if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
        else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
      }, add = TRUE)
      set.seed(7301L)
      expected_seed <- .Random.seed
      analysis$calculate(fixture$records, fixture$dataset, fixture$sample, fixture$input_path,
        classifier$criterion_codes(), classifier$rule_version())
      context$check("bootstrap restaura o estado aleatório do chamador", identical(.Random.seed, expected_seed))
    },
    large_mcnemar = function(context) {
      analysis <- context$services()$statistics
      classifier <- context$services()$classifier
      large_n <- 1100L
      large_sample <- data.frame(repository = paste0("owner/large-", seq_len(large_n)), stringsAsFactors = FALSE)
      large_records <- lapply(large_sample$repository, function(repository) list(repository = repository,
        pre = context$complete_period("pre"), post = context$complete_period("post", "post")))
      large_dataset <- data.frame(
        repository = large_sample$repository,
        pre_D1 = c(rep("1", large_n %/% 2L), rep("0", large_n %/% 2L)),
        post_D1 = c(rep("0", large_n %/% 2L), rep("1", large_n %/% 2L)),
        pre_score = rep("0", large_n), post_score = rep("0", large_n),
        sample_source_sha256 = rep("large-hash", large_n), stringsAsFactors = FALSE
      )
      for (code in classifier$criterion_codes()) {
        large_dataset[[paste0("pre_", code)]] <- "0"
        large_dataset[[paste0("post_", code)]] <- "0"
      }
      large_result <- analysis$calculate(large_records, large_dataset, large_sample, context$store()$sample_path(),
        classifier$criterion_codes(), classifier$rule_version())
      context$check("McNemar permanece finito quando coeficientes binomiais excedem double",
        is.finite(large_result$rq1_mcnemar$exact_mcnemar_p_two_sided) &&
          large_result$rq1_mcnemar$exact_mcnemar_p_two_sided == 1)
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)

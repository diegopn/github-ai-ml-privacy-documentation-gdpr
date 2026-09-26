percentile_value <- function(values, probability) {
  values <- sort(as.numeric(values))
  values <- values[is.finite(values)]
  if (!length(values)) return(NA_real_)
  as.numeric(stats::quantile(values, probability, type = 7, names = FALSE))
}

exact_mcnemar <- function(b, c) {
  n <- b + c
  if (!n) return(1)
  lower <- min(b, c)
  min(1, 2 * sum(vapply(0:lower, function(k) choose(n, k), numeric(1L))) / 2^n)
}

wilcoxon_exact <- function(differences) {
  values <- differences[differences != 0]
  n <- length(values)
  if (!n) return(list(n_nonzero = 0L, w_plus = 0, w_minus = 0, p_two_sided_exact = 1, rank_biserial = 0))
  ranks <- rank(abs(values), ties.method = "average")
  w_plus <- sum(ranks[values > 0])
  w_minus <- sum(ranks[values < 0])
  # A escala inteira permite representar postos médios na distribuição.
  scaled <- as.integer(round(ranks * 2))
  probabilities <- 1
  for (rank_value in scaled) {
    next_values <- numeric(length(probabilities) + rank_value)
    next_values[seq_along(probabilities)] <- next_values[seq_along(probabilities)] + probabilities * 0.5
    shifted <- seq_along(probabilities) + rank_value
    next_values[shifted] <- next_values[shifted] + probabilities * 0.5
    probabilities <- next_values
  }
  total <- sum(scaled)
  expected <- total / 2
  distance <- abs(w_plus * 2 - expected)
  sums <- 0:(length(probabilities) - 1L)
  p_value <- min(1, sum(probabilities[abs(sums - expected) >= distance - 1e-12]))
  list(
    n_nonzero = n, w_plus = w_plus, w_minus = w_minus,
    p_two_sided_exact = p_value,
    rank_biserial = if ((w_plus + w_minus) == 0) 0 else (w_plus - w_minus) / (w_plus + w_minus)
  )
}

bootstrap_median_ci <- function(values, seed = 20260908L, repetitions = 10000L) {
  values <- as.numeric(values)
  if (!length(values)) return(c(NA_real_, NA_real_))
  set.seed(seed)
  medians <- vapply(seq_len(repetitions), function(...) median(sample(values, length(values), replace = TRUE)), numeric(1L))
  c(percentile_value(medians, 0.025), percentile_value(medians, 0.975))
}

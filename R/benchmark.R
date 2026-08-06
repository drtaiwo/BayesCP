#' Benchmark exact posterior computation
#'
#' Measures elapsed time for exact one-change fits across increasing sample
#' sizes. Posterior draws are disabled so that the benchmark targets the exact
#' collapsed change-point computation.
#'
#' @param n Sample sizes.
#' @param repetitions Number of timed repetitions per sample size.
#' @param delta Standardized mean shift.
#' @param prior Prior specification.
#' @param min_seg Minimum segment length.
#' @param seed Master seed.
#' @return A data frame with median, mean, and quantile runtimes.
#' @export
bayescp_benchmark <- function(n = c(100L, 500L, 1000L, 5000L, 10000L),
                              repetitions = 5L, delta = 1,
                              prior = bayescp_prior(m01 = 0, m02 = 1),
                              min_seg = 5L, seed = 20260724L) {
  n <- as.integer(n); repetitions <- as.integer(repetitions)
  if (any(n < 2L * min_seg) || repetitions < 1L) stop("Invalid benchmark settings.", call. = FALSE)
  rows <- vector("list", length(n))
  for (i in seq_along(n)) {
    ni <- n[i]; tau <- floor(ni / 2)
    times <- numeric(repetitions)
    for (r in seq_len(repetitions)) {
      dat <- bayescp_simulate(ni, tau, 0, delta, 1, seed = seed + i * 1000L + r)
      times[r] <- system.time(bayescp_fit(dat$y, prior, min_seg = min_seg, draws = 0L))[["elapsed"]]
    }
    rows[[i]] <- data.frame(
      n = ni, repetitions = repetitions,
      mean_seconds = mean(times), median_seconds = stats::median(times),
      q025_seconds = stats::quantile(times, 0.025, names = FALSE),
      q975_seconds = stats::quantile(times, 0.975, names = FALSE)
    )
  }
  do.call(rbind, rows)
}

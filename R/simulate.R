#' Simulate a Gaussian or robustness single-change series
#'
#' @param n Number of observations.
#' @param tau Change-point location.
#' @param mu1,mu2 Segment means.
#' @param sigma Common standard deviation retained for backward compatibility.
#' @param seed Optional random seed.
#' @param sigma1,sigma2 Optional segment-specific standard deviations.
#' @param error_distribution One of `"normal"`, `"student_t"`, or
#'   `"contaminated_normal"`.
#' @param df Student-t degrees of freedom.
#' @param ar1 AR(1) coefficient.
#' @param contamination_prob Contamination probability.
#' @param contamination_multiplier Contaminated-normal SD multiplier.
#' @return A list containing the generated series and true parameters.
#' @export
bayescp_simulate <- function(n = 100L, tau = floor(n / 2),
                             mu1 = 0, mu2 = 1, sigma = 1,
                             seed = NULL, sigma1 = sigma, sigma2 = sigma,
                             error_distribution = c("normal", "student_t", "contaminated_normal"),
                             df = 5, ar1 = 0, contamination_prob = 0.05,
                             contamination_multiplier = 5) {
  n <- as.integer(n); tau <- as.integer(tau)
  if (n < 4L || tau < 1L || tau >= n) stop("Require `n >= 4` and `1 <= tau < n`.", call. = FALSE)
  if (!is.finite(sigma1) || !is.finite(sigma2) || sigma1 <= 0 || sigma2 <= 0) stop("Segment standard deviations must be positive.", call. = FALSE)
  error_distribution <- match.arg(error_distribution)
  if (!is.finite(ar1) || abs(ar1) >= 1) stop("`ar1` must lie strictly between -1 and 1.", call. = FALSE)
  if (!is.null(seed)) set.seed(seed)
  z <- switch(error_distribution,
    normal = stats::rnorm(n),
    student_t = {
      if (!is.finite(df) || df <= 2) stop("`df` must exceed 2.", call. = FALSE)
      stats::rt(n, df = df) / sqrt(df / (df - 2))
    },
    contaminated_normal = {
      bad <- stats::runif(n) < contamination_prob
      zz <- stats::rnorm(n)
      if (any(bad)) zz[bad] <- stats::rnorm(sum(bad), sd = contamination_multiplier)
      zz
    }
  )
  sds <- c(rep(sigma1, tau), rep(sigma2, n - tau))
  e <- z * sds
  if (ar1 != 0) {
    for (i in 2:n) e[i] <- ar1 * e[i - 1L] + sqrt(1 - ar1^2) * e[i]
  }
  y <- c(rep(mu1, tau), rep(mu2, n - tau)) + e
  list(y = y, truth = list(n = n, tau = tau, mu1 = mu1, mu2 = mu2,
       sigma = sigma, sigma1 = sigma1, sigma2 = sigma2,
       error_distribution = error_distribution, df = df, ar1 = ar1))
}

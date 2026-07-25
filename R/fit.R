#' Fit an exact Bayesian Gaussian single-change model
#'
#' @param y Numeric ordered observations.
#' @param prior A prior created by [bayescp_prior()].
#' @param min_seg Minimum observations in each segment.
#' @param level Credible level.
#' @param draws Number of direct posterior draws. Use zero to skip draws.
#' @param seed Optional random seed for direct posterior simulation.
#' @return An object of class `bayescp_fit`.
#'
#' @examples
#' dat <- bayescp_simulate(
#'   n = 100,
#'   tau = 50,
#'   mu1 = 0,
#'   mu2 = 1,
#'   sigma = 1,
#'   seed = 123
#' )
#'
#' prior <- bayescp_prior(
#'   m01 = 0,
#'   m02 = 1,
#'   kappa01 = 5,
#'   kappa02 = 5
#' )
#'
#' fit <- bayescp_fit(
#'   dat$y,
#'   prior = prior,
#'   draws = 200,
#'   seed = 123
#' )
#'
#' summary(fit)
#'
#' @export
bayescp_fit <- function(y, prior = bayescp_prior(),
                        min_seg = 5L, level = 0.95,
                        draws = 0L, seed = NULL) {
  .bayescp_assert_numeric_series(y)
  if (!inherits(prior, "bayescp_prior")) {
    stop("`prior` must be created by `bayescp_prior()`.", call. = FALSE)
  }
  n <- length(y)
  min_seg <- as.integer(min_seg)
  if (length(min_seg) != 1L || is.na(min_seg) || min_seg < 1L ||
      2L * min_seg > n) {
    stop("`min_seg` must be positive and leave at least one admissible change point.",
         call. = FALSE)
  }
  if (!is.numeric(level) || length(level) != 1L ||
      level <= 0 || level >= 1) {
    stop("`level` must lie strictly between zero and one.", call. = FALSE)
  }
  draws <- as.integer(draws)
  if (length(draws) != 1L || is.na(draws) || draws < 0L) {
    stop("`draws` must be a non-negative integer.", call. = FALSE)
  }

  tau <- seq.int(min_seg, n - min_seg)
  k <- length(tau)

  cs <- cumsum(y)
  cs2 <- cumsum(y * y)
  total_sum <- cs[n]
  total_sum2 <- cs2[n]

  n1 <- tau
  n2 <- n - tau
  sum1 <- cs[tau]
  sum2 <- total_sum - sum1
  sumsq1 <- cs2[tau]
  sumsq2 <- total_sum2 - sumsq1
  mean1 <- sum1 / n1
  mean2 <- sum2 / n2
  sse1 <- .bayescp_sse(sum1, sumsq1, n1)
  sse2 <- .bayescp_sse(sum2, sumsq2, n2)

  k1n <- prior$kappa01 + n1
  k2n <- prior$kappa02 + n2
  m1n <- (prior$kappa01 * prior$m01 + n1 * mean1) / k1n
  m2n <- (prior$kappa02 * prior$m02 + n2 * mean2) / k2n

  c1 <- prior$kappa01 * n1 / k1n * (mean1 - prior$m01)^2
  c2 <- prior$kappa02 * n2 / k2n * (mean2 - prior$m02)^2
  q_tau <- sse1 + sse2 + c1 + c2
  an <- prior$a0 + n / 2
  bn <- prior$b0 + q_tau / 2

  tau_prob <- prior$tau_prob
  if (is.null(tau_prob)) {
    tau_prob <- rep.int(1 / k, k)
  } else if (length(tau_prob) != k) {
    stop(sprintf("`prior$tau_prob` has length %d but %d admissible locations exist.",
                 length(tau_prob), k), call. = FALSE)
  }

  log_marginal <- -n / 2 * log(2 * pi) +
    0.5 * log(prior$kappa01 / k1n) +
    0.5 * log(prior$kappa02 / k2n) +
    prior$a0 * log(prior$b0) - lgamma(prior$a0) +
    lgamma(an) - an * log(bn)

  log_weight <- log_marginal + log(tau_prob)
  log_norm <- .bayescp_log_sum_exp(log_weight)
  prob <- exp(log_weight - log_norm)

  map_idx <- which(prob == max(prob))
  map_tau <- as.integer(round(stats::median(tau[map_idx])))
  post_mean <- sum(tau * prob)
  post_sd <- sqrt(sum((tau - post_mean)^2 * prob))
  posterior_median <- tau[which(cumsum(prob) >= 0.5)[1L]]
  ci <- .bayescp_equal_tailed_interval(tau, prob, level)
  smallest <- .bayescp_smallest_set(tau, prob, level)
  entropy <- -sum(ifelse(prob > 0, prob * log(prob), 0))
  norm_entropy <- entropy / log(k)
  p_max <- max(prob)

  posterior_draws <- NULL
  if (draws > 0L) {
    if (!is.null(seed)) set.seed(seed)
    tau_draw <- sample(tau, size = draws, replace = TRUE, prob = prob)
    idx <- match(tau_draw, tau)
    sigma2_draw <- 1 / stats::rgamma(draws, shape = an, rate = bn[idx])
    mu1_draw <- stats::rnorm(draws, mean = m1n[idx],
                             sd = sqrt(sigma2_draw / k1n[idx]))
    mu2_draw <- stats::rnorm(draws, mean = m2n[idx],
                             sd = sqrt(sigma2_draw / k2n[idx]))
    posterior_draws <- data.frame(
      tau = tau_draw,
      sigma2 = sigma2_draw,
      mu1 = mu1_draw,
      mu2 = mu2_draw
    )
  }

  out <- list(
    call = match.call(),
    data = y,
    n = n,
    prior = prior,
    min_seg = min_seg,
    level = level,
    tau = tau,
    posterior_prob = prob,
    log_marginal = log_marginal,
    log_normalizer = log_norm,
    sufficient_statistics = data.frame(
      tau = tau, n1 = n1, n2 = n2, mean1 = mean1, mean2 = mean2,
      sse1 = sse1, sse2 = sse2, k1n = k1n, k2n = k2n,
      m1n = m1n, m2n = m2n, C1 = c1, C2 = c2, Q = q_tau,
      bn = bn
    ),
    posterior = list(
      map = map_tau,
      mean = post_mean,
      median = posterior_median,
      sd = post_sd,
      interval = ci,
      smallest_set = smallest,
      entropy = entropy,
      normalized_entropy = norm_entropy,
      p_max = p_max
    ),
    draws = posterior_draws
  )
  class(out) <- "bayescp_fit"
  bayescp_validate_fit(out, error = TRUE)
  out
}

#' Validate a fitted ExactBayesCP object
#'
#' @param object A `bayescp_fit` object.
#' @param tolerance Numerical tolerance.
#' @param error If `TRUE`, stop when validation fails.
#' @return A named logical vector.
#'
#' @examples
#' dat <- bayescp_simulate(
#'   n = 100,
#'   tau = 50,
#'   mu1 = 0,
#'   mu2 = 1,
#'   sigma = 1,
#'   seed = 123
#' )
#'
#' fit <- bayescp_fit(
#'   dat$y,
#'   prior = bayescp_prior(
#'     m01 = 0,
#'     m02 = 1,
#'     kappa01 = 5,
#'     kappa02 = 5
#'   ),
#'   draws = 0
#' )
#'
#' bayescp_validate_fit(fit)
#'
#' @export
bayescp_validate_fit <- function(object, tolerance = 1e-8, error = FALSE) {
  checks <- c(
    correct_class = inherits(object, "bayescp_fit"),
    finite_log_marginal = all(is.finite(object$log_marginal)),
    finite_probabilities = all(is.finite(object$posterior_prob)),
    nonnegative_probabilities = all(object$posterior_prob >= -tolerance),
    normalized_probabilities =
      abs(sum(object$posterior_prob) - 1) <= tolerance,
    valid_map = object$posterior$map %in% object$tau,
    interval_ordered =
      object$posterior$interval[1] <= object$posterior$interval[2],
    interval_in_support =
      all(object$posterior$interval %in% range(object$tau)[1]:range(object$tau)[2]),
    entropy_nonnegative = object$posterior$entropy >= -tolerance,
    pmax_valid = object$posterior$p_max >= 0 &&
      object$posterior$p_max <= 1 + tolerance
  )
  if (isTRUE(error) && !all(checks)) {
    stop("ExactBayesCP validation failed: ",
         paste(names(checks)[!checks], collapse = ", "), call. = FALSE)
  }
  checks
}

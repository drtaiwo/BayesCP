#' Compare no-change and one-change Gaussian models
#'
#' Computes exact marginal likelihoods for a Gaussian no-change model and the
#' one-change model fitted by [bayescp_fit()]. Posterior model probabilities are
#' obtained from user-specified prior model probabilities.
#'
#' @param y Numeric ordered observations.
#' @param prior_change Prior for the one-change model.
#' @param prior_no_change Prior for the no-change mean and variance. Only
#'   `m01`, `kappa01`, `a0`, and `b0` are used.
#' @param prior_prob_change Prior probability assigned to the one-change model.
#' @param min_seg Minimum segment length under the one-change model.
#' @return An object of class `bayescp_model_comparison`.
#' @export
bayescp_compare_models <- function(
    y,
    prior_change = bayescp_prior(),
    prior_no_change = bayescp_prior(),
    prior_prob_change = 0.5,
    min_seg = 5L) {
  .bayescp_assert_numeric_series(y)
  if (!inherits(prior_change, "bayescp_prior") ||
      !inherits(prior_no_change, "bayescp_prior")) {
    stop("Both priors must be created by `bayescp_prior()`.", call. = FALSE)
  }
  .bayescp_assert_scalar(prior_prob_change, "prior_prob_change", lower = 0,
                         strict = TRUE)
  if (prior_prob_change >= 1) {
    stop("`prior_prob_change` must be strictly below one.", call. = FALSE)
  }

  fit1 <- bayescp_fit(y, prior = prior_change, min_seg = min_seg, draws = 0L)
  log_m1 <- fit1$log_normalizer

  n <- length(y)
  ybar <- mean(y)
  sse <- sum((y - ybar)^2)
  k0 <- prior_no_change$kappa01
  kn <- k0 + n
  an <- prior_no_change$a0 + n / 2
  conflict <- k0 * n / kn * (ybar - prior_no_change$m01)^2
  bn <- prior_no_change$b0 + 0.5 * (sse + conflict)
  log_m0 <- -n / 2 * log(2 * pi) +
    0.5 * log(k0 / kn) +
    prior_no_change$a0 * log(prior_no_change$b0) -
    lgamma(prior_no_change$a0) + lgamma(an) - an * log(bn)

  log_post <- c(
    no_change = log1p(-prior_prob_change) + log_m0,
    one_change = log(prior_prob_change) + log_m1
  )
  norm <- .bayescp_log_sum_exp(log_post)
  post <- exp(log_post - norm)

  out <- list(
    call = match.call(),
    log_marginal = c(no_change = log_m0, one_change = log_m1),
    log_bayes_factor_change_vs_no_change = unname(log_m1 - log_m0),
    prior_model_probability = c(no_change = 1 - prior_prob_change,
                                one_change = prior_prob_change),
    posterior_model_probability = post,
    one_change_fit = fit1
  )
  class(out) <- "bayescp_model_comparison"
  out
}

#' @export
print.bayescp_model_comparison <- function(x, ...) {
  cat("ExactBayesCP model comparison\n")
  cat(sprintf("Posterior P(no change): %.4f\n",
              x$posterior_model_probability[["no_change"]]))
  cat(sprintf("Posterior P(one change): %.4f\n",
              x$posterior_model_probability[["one_change"]]))
  cat(sprintf("log BF(change:no change): %.4f\n",
              x$log_bayes_factor_change_vs_no_change))
  invisible(x)
}

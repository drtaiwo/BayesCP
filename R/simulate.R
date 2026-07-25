#' Simulate a Gaussian single-change series
#'
#' @param n Number of observations.
#' @param tau Change-point location.
#' @param mu1,mu2 Segment means.
#' @param sigma Common standard deviation.
#' @param seed Optional random seed.
#' @return A list containing the generated series and true parameters.
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
#' head(dat$y)
#' dat$truth
#'
#' @export
bayescp_simulate <- function(n = 100L, tau = floor(n / 2),
                             mu1 = 0, mu2 = 1, sigma = 1,
                             seed = NULL) {
  n <- as.integer(n)
  tau <- as.integer(tau)
  
  if (n < 4L || tau < 1L || tau >= n) {
    stop(
      "Require `n >= 4` and `1 <= tau < n`.",
      call. = FALSE
    )
  }
  
  if (sigma <= 0) {
    stop(
      "`sigma` must be positive.",
      call. = FALSE
    )
  }
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  y <- c(
    stats::rnorm(tau, mu1, sigma),
    stats::rnorm(n - tau, mu2, sigma)
  )
  
  list(
    y = y,
    truth = list(
      n = n,
      tau = tau,
      mu1 = mu1,
      mu2 = mu2,
      sigma = sigma
    )
  )
}
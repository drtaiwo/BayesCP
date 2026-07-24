#' Fit the classical Nile annual-flow series
#'
#' @param prior Prior specification.
#' @param min_seg Minimum segment length.
#' @param draws Number of direct posterior draws.
#' @param seed Optional random seed.
#' @return A `bayescp_fit` object with calendar-year attributes.
#' @export
bayescp_nile_example <- function(
    prior = bayescp_prior(m01 = 1100, m02 = 850,
                          kappa01 = 0.1, kappa02 = 0.1,
                          a0 = 2, b0 = 10000,
                          label = "Nile weak"),
    min_seg = 5L, draws = 2000L, seed = 1L) {
  y <- as.numeric(datasets::Nile)
  fit <- bayescp_fit(y, prior = prior, min_seg = min_seg,
                     draws = draws, seed = seed)
  start_year <- as.integer(stats::start(datasets::Nile)[1])
  attr(fit, "change_year_map") <- start_year + fit$posterior$map - 1L
  attr(fit, "change_year_mean") <- start_year + fit$posterior$mean - 1
  fit
}

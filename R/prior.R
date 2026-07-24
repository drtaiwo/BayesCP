#' Construct a conjugate prior specification
#'
#' @param m01,m02 Prior means for the pre-change and post-change segments.
#' @param kappa01,kappa02 Prior concentration parameters.
#' @param a0,b0 Shape and scale parameters of the inverse-gamma variance prior,
#'   using density proportional to x^(-a0-1) exp(-b0/x).
#' @param tau_prob Optional prior probabilities for admissible change locations.
#' @param label Human-readable prior label.
#' @return An object of class `bayescp_prior`.
#' @export
bayescp_prior <- function(m01 = 0, m02 = 0,
                          kappa01 = 0.1, kappa02 = kappa01,
                          a0 = 2, b0 = 1,
                          tau_prob = NULL,
                          label = "custom") {
  vals <- list(m01 = m01, m02 = m02, kappa01 = kappa01,
               kappa02 = kappa02, a0 = a0, b0 = b0)
  for (nm in names(vals)) .bayescp_assert_scalar(vals[[nm]], nm)
  if (kappa01 <= 0 || kappa02 <= 0 || a0 <= 0 || b0 <= 0) {
    stop("Prior concentrations and inverse-gamma hyperparameters must be positive.",
         call. = FALSE)
  }
  if (!is.null(tau_prob)) {
    if (!is.numeric(tau_prob) || anyNA(tau_prob) ||
        any(!is.finite(tau_prob)) || any(tau_prob < 0) ||
        sum(tau_prob) <= 0) {
      stop("`tau_prob` must contain finite, non-negative values with positive sum.",
           call. = FALSE)
    }
    tau_prob <- tau_prob / sum(tau_prob)
  }
  structure(
    c(vals, list(tau_prob = tau_prob, label = as.character(label)[1L])),
    class = "bayescp_prior"
  )
}

#' Create weak, moderate, and strong prior specifications
#'
#' @param m01,m02 Segment prior means.
#' @param a0,b0 Inverse-gamma hyperparameters.
#' @return A named list of three `bayescp_prior` objects.
#' @export
bayescp_prior_grid <- function(m01 = 0, m02 = 1, a0 = 2, b0 = 1) {
  list(
    weak = bayescp_prior(m01, m02, 0.1, 0.1, a0, b0, label = "weak"),
    moderate = bayescp_prior(m01, m02, 5, 5, a0, b0, label = "moderate"),
    strong = bayescp_prior(m01, m02, 20, 20, a0, b0, label = "strong")
  )
}

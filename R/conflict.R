#' Prior-data conflict diagnostics
#'
#' Computes standardized prior-data discrepancies and conjugate conflict
#' penalties at either a supplied candidate change point or the posterior MAP.
#'
#' @param object A fitted `bayescp_fit` object.
#' @param tau Optional candidate change point. Defaults to the posterior MAP.
#' @param sigma2_reference Optional positive variance used for standardized
#'   conflict scores. Defaults to the sample variance.
#' @return A one-row data frame.
#' @export
bayescp_conflict <- function(object, tau = NULL, sigma2_reference = NULL) {
  if (!inherits(object, "bayescp_fit")) {
    stop("`object` must be a `bayescp_fit` object.", call. = FALSE)
  }
  if (is.null(tau)) tau <- object$posterior$map
  tau <- as.integer(tau)
  idx <- match(tau, object$tau)
  if (is.na(idx)) stop("`tau` is outside the admissible support.", call. = FALSE)
  if (is.null(sigma2_reference)) sigma2_reference <- stats::var(object$data)
  .bayescp_assert_scalar(sigma2_reference, "sigma2_reference", lower = 0,
                         strict = TRUE)
  s <- object$sufficient_statistics[idx, , drop = FALSE]
  p <- object$prior
  z1 <- (p$m01 - s$mean1) /
    sqrt(sigma2_reference * (1 / p$kappa01 + 1 / s$n1))
  z2 <- (p$m02 - s$mean2) /
    sqrt(sigma2_reference * (1 / p$kappa02 + 1 / s$n2))
  data.frame(
    tau = tau,
    prior_weight1 = p$kappa01 / (p$kappa01 + s$n1),
    prior_weight2 = p$kappa02 / (p$kappa02 + s$n2),
    conflict_penalty1 = s$C1,
    conflict_penalty2 = s$C2,
    conflict_z1 = z1,
    conflict_z2 = z2,
    max_abs_conflict_z = max(abs(z1), abs(z2)),
    rms_conflict_z = sqrt(mean(c(z1^2, z2^2)))
  )
}

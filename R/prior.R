#' Construct a conjugate prior specification
#'
#' Constructs a Normal--Inverse-Gamma prior for the two segment means and the
#' common variance. Version 0.3.0 supports preset priors, direct conjugate
#' hyperparameters, an expert-knowledge interface, and a scaled
#' inverse-chi-square parameterization for the variance prior. All interfaces
#' are converted to one internal Normal--Inverse-Gamma representation, so the
#' exact inference engine is unchanged.
#'
#' @param m01,m02 Prior means for the pre-change and post-change segments.
#' @param kappa01,kappa02 Positive prior concentration parameters for the two
#'   segment means.
#' @param a0,b0 Positive shape and scale parameters of the inverse-gamma
#'   variance prior, using density proportional to
#'   `x^(-a0-1) exp(-b0/x)`.
#' @param tau_prob Optional prior probabilities for admissible change locations.
#' @param label Human-readable prior label.
#' @param preset Optional preset: `"weak"`, `"moderate"`, or `"strong"`.
#'   Presets set both mean-concentration parameters to 0.1, 5, or 20,
#'   respectively. For convenience, `bayescp_prior("weak")` is also accepted.
#' @param variance_prior Variance-prior parameterization: `"inverse_gamma"`
#'   or `"scaled_inv_chisq"`.
#' @param nu0,s02 Degrees of freedom and scale for a scaled inverse-chi-square
#'   prior. Under the package convention these are converted as
#'   `a0 = nu0/2` and `b0 = nu0*s02/2`.
#' @param mu1,mu2 Optional expert-knowledge lists, each containing `mean` and
#'   `sd`. The requested standard deviation is interpreted marginally using
#'   the prior mean of the common variance, so that
#'   `kappa0j = E(sigma^2)/sd_j^2`.
#' @param variance Optional expert-knowledge list containing `mean` and
#'   `strength`. `strength` is interpreted as scaled inverse-chi-square degrees
#'   of freedom and must exceed 2. The conversion preserves the requested prior
#'   mean of `sigma^2`.
#'
#' @return An object of class `bayescp_prior` containing the equivalent
#'   Normal--Inverse-Gamma hyperparameters and input metadata.
#'
#' @examples
#' # Direct conjugate hyperparameters
#' p1 <- bayescp_prior(
#'   m01 = 0, m02 = 1,
#'   kappa01 = 5, kappa02 = 5,
#'   a0 = 2, b0 = 1,
#'   label = "moderate"
#' )
#'
#' # Preset concentration
#' p2 <- bayescp_prior(preset = "strong", m01 = 0, m02 = 1)
#'
#' # Scaled inverse-chi-square variance prior
#' p3 <- bayescp_prior(
#'   m01 = 0, m02 = 1,
#'   kappa01 = 5, kappa02 = 5,
#'   variance_prior = "scaled_inv_chisq",
#'   nu0 = 10, s02 = 4
#' )
#'
#' # Expert-knowledge interface
#' p4 <- bayescp_prior(
#'   mu1 = list(mean = 10, sd = 2),
#'   mu2 = list(mean = 18, sd = 3),
#'   variance = list(mean = 5, strength = 12),
#'   label = "expert prior"
#' )
#'
#' summary(p4)
#'
#' @export
bayescp_prior <- function(m01 = 0, m02 = 0,
                          kappa01 = 0.1, kappa02 = kappa01,
                          a0 = 2, b0 = 1,
                          tau_prob = NULL,
                          label = "custom",
                          preset = NULL,
                          variance_prior = c("inverse_gamma", "scaled_inv_chisq"),
                          nu0 = NULL, s02 = NULL,
                          mu1 = NULL, mu2 = NULL,
                          variance = NULL) {
  # Convenience syntax: bayescp_prior("weak")
  if (is.character(m01) && length(m01) == 1L &&
      m01 %in% c("weak", "moderate", "strong") && is.null(preset)) {
    preset <- m01
    m01 <- 0
  }

  if (!is.null(preset)) {
    preset <- match.arg(preset, c("weak", "moderate", "strong"))
    preset_kappa <- c(weak = 0.1, moderate = 5, strong = 20)
    kappa01 <- unname(preset_kappa[preset])
    kappa02 <- unname(preset_kappa[preset])
    if (identical(label, "custom")) label <- preset
  }

  variance_prior <- match.arg(variance_prior)
  interface <- if (!is.null(mu1) || !is.null(mu2) || !is.null(variance)) {
    "expert_knowledge"
  } else if (!is.null(preset)) {
    "preset"
  } else if (identical(variance_prior, "scaled_inv_chisq")) {
    "scaled_inv_chisq"
  } else {
    "direct_hyperparameters"
  }

  # Expert variance input: preserve E(sigma^2) under a scaled-inv-chi-square
  # prior with nu0 = strength.
  if (!is.null(variance)) {
    if (!is.list(variance) ||
        !all(c("mean", "strength") %in% names(variance))) {
      stop("`variance` must be a list containing `mean` and `strength`.",
           call. = FALSE)
    }
    .bayescp_assert_scalar(variance$mean, "variance$mean", lower = 0,
                           strict = TRUE)
    .bayescp_assert_scalar(variance$strength, "variance$strength", lower = 2,
                           strict = TRUE)
    nu0 <- variance$strength
    s02 <- variance$mean * (nu0 - 2) / nu0
    variance_prior <- "scaled_inv_chisq"
  }

  if (identical(variance_prior, "scaled_inv_chisq")) {
    if (is.null(nu0) || is.null(s02)) {
      stop("Supply both `nu0` and `s02` for `variance_prior = \"scaled_inv_chisq\"`.",
           call. = FALSE)
    }
    .bayescp_assert_scalar(nu0, "nu0", lower = 0, strict = TRUE)
    .bayescp_assert_scalar(s02, "s02", lower = 0, strict = TRUE)
    a0 <- nu0 / 2
    b0 <- nu0 * s02 / 2
  }

  # A finite prior mean of sigma^2 is needed to translate an intuitive mean SD
  # into kappa. E(sigma^2) = b0/(a0-1) under this inverse-gamma convention.
  prior_variance_mean <- if (a0 > 1) b0 / (a0 - 1) else NA_real_

  parse_mean_knowledge <- function(x, name, current_mean, current_kappa) {
    if (is.null(x)) return(list(mean = current_mean, kappa = current_kappa))
    if (!is.list(x) || !all(c("mean", "sd") %in% names(x))) {
      stop(sprintf("`%s` must be a list containing `mean` and `sd`.", name),
           call. = FALSE)
    }
    .bayescp_assert_scalar(x$mean, paste0(name, "$mean"))
    .bayescp_assert_scalar(x$sd, paste0(name, "$sd"), lower = 0,
                           strict = TRUE)
    if (!is.finite(prior_variance_mean) || prior_variance_mean <= 0) {
      stop("The expert mean/SD interface requires a variance prior with a finite positive mean (`a0 > 1` or `nu0 > 2`).",
           call. = FALSE)
    }
    list(mean = x$mean, kappa = prior_variance_mean / (x$sd^2))
  }

  m1 <- parse_mean_knowledge(mu1, "mu1", m01, kappa01)
  m2 <- parse_mean_knowledge(mu2, "mu2", m02, kappa02)
  m01 <- m1$mean
  kappa01 <- m1$kappa
  m02 <- m2$mean
  kappa02 <- m2$kappa

  vals <- list(
    m01 = m01, m02 = m02,
    kappa01 = kappa01, kappa02 = kappa02,
    a0 = a0, b0 = b0
  )

  for (nm in names(vals)) .bayescp_assert_scalar(vals[[nm]], nm)

  if (kappa01 <= 0 || kappa02 <= 0 || a0 <= 0 || b0 <= 0) {
    stop("Prior concentrations and variance-prior hyperparameters must be positive.",
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

  variance_metadata <- if (identical(variance_prior, "scaled_inv_chisq")) {
    list(parameterization = "scaled_inv_chisq", nu0 = nu0, s02 = s02,
         prior_mean = if (nu0 > 2) nu0 * s02 / (nu0 - 2) else Inf)
  } else {
    list(parameterization = "inverse_gamma", a0 = a0, b0 = b0,
         prior_mean = prior_variance_mean)
  }

  structure(
    c(vals, list(
      tau_prob = tau_prob,
      label = as.character(label)[1L],
      interface = interface,
      preset = preset,
      variance_prior = variance_metadata,
      expert_input = list(mu1 = mu1, mu2 = mu2, variance = variance)
    )),
    class = "bayescp_prior"
  )
}

#' Create weak, moderate, and strong prior specifications
#'
#' @param m01,m02 Segment prior means.
#' @param a0,b0 Inverse-gamma hyperparameters.
#' @param variance_prior Variance-prior parameterization.
#' @param nu0,s02 Optional scaled inverse-chi-square parameters.
#' @return A named list of three `bayescp_prior` objects.
#' @export
bayescp_prior_grid <- function(m01 = 0, m02 = 1, a0 = 2, b0 = 1,
                               variance_prior = c("inverse_gamma", "scaled_inv_chisq"),
                               nu0 = NULL, s02 = NULL) {
  variance_prior <- match.arg(variance_prior)
  make_one <- function(preset) {
    bayescp_prior(
      m01 = m01, m02 = m02,
      a0 = a0, b0 = b0,
      preset = preset,
      variance_prior = variance_prior,
      nu0 = nu0, s02 = s02,
      label = preset
    )
  }
  list(weak = make_one("weak"),
       moderate = make_one("moderate"),
       strong = make_one("strong"))
}

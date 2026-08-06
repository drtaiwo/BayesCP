#' @export
print.bayescp_fit <- function(x, ...) {
  cat("Exact Bayesian Gaussian change-point fit\n")
  cat("Observations:", x$n, "\n")
  cat("Prior:", x$prior$label, "\n")
  cat("MAP change point:", x$posterior$map, "\n")
  cat("Posterior mean:", sprintf("%.3f", x$posterior$mean), "\n")
  cat(sprintf("%.1f%% interval: [%s, %s]\n", 100*x$level,
              x$posterior$interval[1], x$posterior$interval[2]))
  cat("Maximum posterior probability:", sprintf("%.4f", x$posterior$p_max), "\n")
  invisible(x)
}

#' @export
summary.bayescp_fit <- function(object, ...) {
  out <- list(n=object$n, prior=object$prior$label,
    map=object$posterior$map, mean=object$posterior$mean,
    median=object$posterior$median, sd=object$posterior$sd,
    interval=object$posterior$interval, entropy=object$posterior$entropy,
    normalized_entropy=object$posterior$normalized_entropy,
    p_max=object$posterior$p_max, validation=bayescp_validate_fit(object))
  class(out) <- "summary.bayescp_fit"; out
}

#' @export
print.summary.bayescp_fit <- function(x, ...) {
  cat("ExactBayesCP posterior summary\n-------------------------\n")
  cat("n:",x$n,"\nprior:",x$prior,"\nMAP:",x$map,"\n")
  cat("mean:",sprintf("%.3f",x$mean),"\nmedian:",x$median,"\n")
  cat("SD:",sprintf("%.3f",x$sd),"\ninterval:",paste(x$interval,collapse=" to "),"\n")
  cat("entropy:",sprintf("%.4f",x$entropy),"\nnormalized entropy:",sprintf("%.4f",x$normalized_entropy),"\n")
  cat("p_max:",sprintf("%.4f",x$p_max),"\nall validation checks passed:",all(x$validation),"\n")
  invisible(x)
}

#' @export
plot.bayescp_fit <- function(x, type=c("posterior","series","histogram","density_mu1","density_mu2","density_sigma2","credible"), ...) {
  type <- match.arg(type)
  if (type=="posterior") {
    graphics::plot(x$tau,x$posterior_prob,type="h",xlab="Candidate change point",ylab="Posterior probability",...)
    graphics::points(x$posterior$map,x$posterior_prob[match(x$posterior$map,x$tau)],pch=19)
  } else if (type=="series") {
    graphics::plot(seq_along(x$data),x$data,type="l",xlab="Index",ylab="Observation",...)
    graphics::abline(v=x$posterior$map,lty=2)
  } else if (type=="histogram") {
    if (is.null(x$draws)) stop("Posterior draws are unavailable. Refit with `draws > 0`.",call.=FALSE)
    graphics::hist(x$draws$tau,breaks=seq(min(x$tau)-0.5,max(x$tau)+0.5,by=1),
      xlab="Change-point location",main="Posterior draws of the change-point location",...)
    graphics::abline(v=x$posterior$map,lty=2)
  } else if (type=="density_mu1") {
    if (is.null(x$draws)) stop("Posterior draws are unavailable. Refit with `draws > 0`.",call.=FALSE)
    graphics::plot(stats::density(x$draws$mu1),xlab=expression(mu[1]),main="Posterior density of the pre-change mean",...)
  } else if (type=="density_mu2") {
    if (is.null(x$draws)) stop("Posterior draws are unavailable. Refit with `draws > 0`.",call.=FALSE)
    graphics::plot(stats::density(x$draws$mu2),xlab=expression(mu[2]),main="Posterior density of the post-change mean",...)
  } else if (type=="density_sigma2") {
    if (is.null(x$draws)) stop("Posterior draws are unavailable. Refit with `draws > 0`.",call.=FALSE)
    graphics::plot(stats::density(x$draws$sigma2),xlab=expression(sigma^2),main="Posterior density of the common variance",...)
  } else {
    graphics::plot(x$tau,x$posterior_prob,type="h",xlab="Candidate change point",ylab="Posterior probability",
      main=sprintf("%.1f%% posterior credible interval",100*x$level),...)
    graphics::abline(v=x$posterior$map,lty=2)
    graphics::segments(x$posterior$interval[1],0,x$posterior$interval[2],0,lwd=4)
    graphics::points(x$posterior$interval,c(0,0),pch=16)
  }
  invisible(x)
}

#' @export
print.bayescp_prior <- function(x, ...) {
  cat("ExactBayesCP conjugate prior\n")
  cat("-----------------------------\n")
  cat("Label:", x$label, "\n")
  cat("Interface:", gsub("_", " ", x$interface), "\n")
  cat("Mean 1: m01 =", format(x$m01), ", kappa01 =", format(x$kappa01), "\n")
  cat("Mean 2: m02 =", format(x$m02), ", kappa02 =", format(x$kappa02), "\n")
  if (identical(x$variance_prior$parameterization, "scaled_inv_chisq")) {
    cat("Variance prior: scaled inverse-chi-square\n")
    cat("  nu0 =", format(x$variance_prior$nu0),
        ", s0^2 =", format(x$variance_prior$s02), "\n")
    cat("Equivalent inverse-gamma: a0 =", format(x$a0),
        ", b0 =", format(x$b0), "\n")
  } else {
    cat("Variance prior: inverse-gamma\n")
    cat("  a0 =", format(x$a0), ", b0 =", format(x$b0), "\n")
  }
  if (is.finite(x$variance_prior$prior_mean)) {
    cat("Prior mean of sigma^2:", format(x$variance_prior$prior_mean), "\n")
  }
  cat("Change-point prior:", if (is.null(x$tau_prob)) "uniform at fit time" else "user supplied", "\n")
  invisible(x)
}

#' @export
summary.bayescp_prior <- function(object, ...) {
  out <- list(
    label = object$label,
    interface = object$interface,
    preset = object$preset,
    mean1 = list(m0 = object$m01, kappa0 = object$kappa01),
    mean2 = list(m0 = object$m02, kappa0 = object$kappa02),
    variance = object$variance_prior,
    equivalent_inverse_gamma = c(a0 = object$a0, b0 = object$b0),
    tau_prior = if (is.null(object$tau_prob)) "uniform at fit time" else object$tau_prob
  )
  class(out) <- "summary.bayescp_prior"
  out
}

#' @export
print.summary.bayescp_prior <- function(x, ...) {
  cat("ExactBayesCP prior summary\n")
  cat("==========================\n")
  cat("Label:", x$label, "\n")
  cat("Input interface:", gsub("_", " ", x$interface), "\n\n")
  cat("Segment 1 mean\n")
  cat("  m01 =", format(x$mean1$m0), "\n")
  cat("  kappa01 =", format(x$mean1$kappa0), "\n\n")
  cat("Segment 2 mean\n")
  cat("  m02 =", format(x$mean2$m0), "\n")
  cat("  kappa02 =", format(x$mean2$kappa0), "\n\n")
  cat("Variance prior\n")
  cat("  parameterization =", gsub("_", " ", x$variance$parameterization), "\n")
  if (identical(x$variance$parameterization, "scaled_inv_chisq")) {
    cat("  nu0 =", format(x$variance$nu0), "\n")
    cat("  s0^2 =", format(x$variance$s02), "\n")
  }
  cat("  equivalent a0 =", format(x$equivalent_inverse_gamma[["a0"]]), "\n")
  cat("  equivalent b0 =", format(x$equivalent_inverse_gamma[["b0"]]), "\n")
  if (is.finite(x$variance$prior_mean)) {
    cat("  E(sigma^2) =", format(x$variance$prior_mean), "\n")
  }
  invisible(x)
}

#' @export
plot.bayescp_prior <- function(x,
                               type = c("all", "mu1", "mu2", "sigma2"),
                               n = 500L, ...) {
  type <- match.arg(type)
  n <- as.integer(n)
  if (n < 50L) stop("`n` must be at least 50.", call. = FALSE)

  # Marginally, mu_j follows a Student-t distribution with 2*a0 degrees
  # of freedom and scale sqrt(b0/(a0*kappa0)).
  draw_mu_density <- function(m0, kappa0, title, xlab) {
    df <- 2 * x$a0
    sc <- sqrt(x$b0 / (x$a0 * kappa0))
    grid <- seq(m0 - 5 * sc, m0 + 5 * sc, length.out = n)
    dens <- stats::dt((grid - m0) / sc, df = df) / sc
    graphics::plot(grid, dens, type = "l", xlab = xlab,
                   ylab = "Prior density", main = title, ...)
  }

  draw_sigma_density <- function() {
    probs <- c(0.001, 0.999)
    # If X ~ IG(a,b), then b/X ~ Gamma(a,1).
    lo <- x$b0 / stats::qgamma(probs[2], shape = x$a0)
    hi <- x$b0 / stats::qgamma(probs[1], shape = x$a0)
    grid <- seq(lo, hi, length.out = n)
    logdens <- x$a0 * log(x$b0) - lgamma(x$a0) -
      (x$a0 + 1) * log(grid) - x$b0 / grid
    graphics::plot(grid, exp(logdens), type = "l",
                   xlab = expression(sigma^2), ylab = "Prior density",
                   main = "Prior density of the common variance", ...)
  }

  if (type == "all") {
    old <- graphics::par(mfrow = c(1, 3))
    on.exit(graphics::par(old), add = TRUE)
    draw_mu_density(x$m01, x$kappa01, "Prior density of mean 1", expression(mu[1]))
    draw_mu_density(x$m02, x$kappa02, "Prior density of mean 2", expression(mu[2]))
    draw_sigma_density()
  } else if (type == "mu1") {
    draw_mu_density(x$m01, x$kappa01, "Prior density of mean 1", expression(mu[1]))
  } else if (type == "mu2") {
    draw_mu_density(x$m02, x$kappa02, "Prior density of mean 2", expression(mu[2]))
  } else {
    draw_sigma_density()
  }
  invisible(x)
}

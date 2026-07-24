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
  cat("BayesCP posterior summary\n-------------------------\n")
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

\name{bayescp_fit}
\alias{bayescp_fit}
\title{Fit an Exact Bayesian Gaussian Single-Change Model}
\usage{
bayescp_fit(y, prior = bayescp_prior(), min_seg = 5L,
  level = 0.95, draws = 0L, seed = NULL)
}
\arguments{
\item{y}{Numeric ordered observations.}
\item{prior}{A prior created by \code{bayescp_prior()}.}
\item{min_seg}{Minimum observations in each segment.}
\item{level}{Credible level.}
\item{draws}{Number of direct posterior draws.}
\item{seed}{Optional random seed.}
}
\value{An object of class \code{bayescp_fit}.}
\description{Computes the collapsed posterior mass function of an unknown Gaussian mean-change location exactly and optionally generates direct posterior draws.}

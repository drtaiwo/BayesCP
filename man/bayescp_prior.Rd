\name{bayescp_prior}
\alias{bayescp_prior}
\title{Construct a flexible conjugate prior specification}
\usage{
bayescp_prior(
  m01 = 0, m02 = 0,
  kappa01 = 0.1, kappa02 = kappa01,
  a0 = 2, b0 = 1,
  tau_prob = NULL, label = "custom",
  preset = NULL,
  variance_prior = c("inverse_gamma", "scaled_inv_chisq"),
  nu0 = NULL, s02 = NULL,
  mu1 = NULL, mu2 = NULL, variance = NULL
)
}
\arguments{
\item{m01, m02}{Prior means for the pre-change and post-change segments.}
\item{kappa01, kappa02}{Positive prior concentration parameters.}
\item{a0, b0}{Inverse-gamma shape and scale under density proportional to \eqn{x^{-a_0-1}\exp(-b_0/x)}.}
\item{tau_prob}{Optional probabilities for admissible change locations.}
\item{label}{Human-readable prior label.}
\item{preset}{One of \code{"weak"}, \code{"moderate"}, or \code{"strong"}.}
\item{variance_prior}{Either \code{"inverse_gamma"} or \code{"scaled_inv_chisq"}.}
\item{nu0, s02}{Scaled inverse-chi-square degrees of freedom and scale.}
\item{mu1, mu2}{Optional lists containing \code{mean} and \code{sd}.}
\item{variance}{Optional list containing \code{mean} and \code{strength}; strength must exceed 2.}
}
\value{An object of class \code{bayescp_prior}.}
\description{
Creates preset, direct-hyperparameter, expert-knowledge, or scaled inverse-chi-square prior specifications. Every interface is converted to one internal Normal--Inverse-Gamma representation so exact inference is preserved.
}
\details{
For a scaled inverse-chi-square prior, \eqn{a_0=\nu_0/2} and \eqn{b_0=\nu_0s_0^2/2}. For the expert interface, the variance \code{strength} is interpreted as \eqn{\nu_0}, and the requested mean of \eqn{\sigma^2} is preserved. The requested standard deviations for the segment means are translated using the prior mean of \eqn{\sigma^2}.
}
\examples{
p <- bayescp_prior(preset = "moderate", m01 = 0, m02 = 1)

p_chisq <- bayescp_prior(
  m01 = 0, m02 = 1,
  kappa01 = 5, kappa02 = 5,
  variance_prior = "scaled_inv_chisq",
  nu0 = 10, s02 = 4
)

p_expert <- bayescp_prior(
  mu1 = list(mean = 10, sd = 2),
  mu2 = list(mean = 18, sd = 3),
  variance = list(mean = 5, strength = 12)
)
summary(p_expert)
}

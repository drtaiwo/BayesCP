\name{bayescp_prior_grid}
\alias{bayescp_prior_grid}
\title{Create weak, moderate, and strong prior specifications}
\usage{
bayescp_prior_grid(
  m01 = 0, m02 = 1, a0 = 2, b0 = 1,
  variance_prior = c("inverse_gamma", "scaled_inv_chisq"),
  nu0 = NULL, s02 = NULL
)
}
\arguments{
\item{m01, m02}{Segment prior means.}
\item{a0, b0}{Inverse-gamma hyperparameters.}
\item{variance_prior}{Variance-prior parameterization.}
\item{nu0, s02}{Optional scaled inverse-chi-square parameters.}
}
\value{A named list of weak, moderate, and strong \code{bayescp_prior} objects.}
\description{Creates a standard prior-sensitivity grid with concentrations 0.1, 5, and 20.}
\examples{
priors <- bayescp_prior_grid(m01 = 0, m02 = 1)
names(priors)
}

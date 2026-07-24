\name{bayescp_validate_fit}
\alias{bayescp_validate_fit}
\title{Validate a BayesCP Fit}
\usage{
bayescp_validate_fit(object, tolerance = 1e-8, error = FALSE)
}
\arguments{
\item{object}{A \code{bayescp_fit} object.}
\item{tolerance}{Numerical tolerance.}
\item{error}{Stop if validation fails.}
}
\value{A named logical vector.}
\description{Checks posterior normalization, finiteness, support, entropy, and interval validity.}

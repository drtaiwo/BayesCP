\name{bayescp_prior}
\alias{bayescp_prior}
\alias{bayescp_prior_grid}
\title{Construct BayesCP Prior Specifications}
\usage{
bayescp_prior(m01 = 0, m02 = 0, kappa01 = 0.1,
  kappa02 = kappa01, a0 = 2, b0 = 1,
  tau_prob = NULL, label = "custom")
bayescp_prior_grid(m01 = 0, m02 = 1, a0 = 2, b0 = 1)
}
\arguments{
\item{m01,m02}{Segment prior means.}
\item{kappa01,kappa02}{Positive prior concentrations.}
\item{a0,b0}{Positive inverse-gamma hyperparameters.}
\item{tau_prob}{Optional prior probabilities for admissible locations.}
\item{label}{Human-readable label.}
}
\value{A prior object or named list of prior objects.}
\description{Constructs custom or standard weak, moderate, and strong conjugate priors.}

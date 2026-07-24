\name{bayescp_nile_example}
\alias{bayescp_nile_example}
\title{Analyse the Classical Nile Annual-Flow Series}
\usage{
bayescp_nile_example(prior = bayescp_prior(m01 = 1100,
  m02 = 850, kappa01 = 0.1, kappa02 = 0.1, a0 = 2,
  b0 = 10000, label = "Nile weak"), min_seg = 5L,
  draws = 2000L, seed = 1L)
}
\arguments{
\item{prior}{Prior specification.}
\item{min_seg}{Minimum segment length.}
\item{draws}{Direct posterior draws.}
\item{seed}{Optional random seed.}
}
\value{A \code{bayescp_fit} object with calendar-year attributes.}
\description{Fits BayesCP to the built-in Nile annual-flow series.}

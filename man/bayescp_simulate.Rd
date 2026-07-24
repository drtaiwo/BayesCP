\name{bayescp_simulate}
\alias{bayescp_simulate}
\title{Simulate a Gaussian Single-Change Series}
\usage{
bayescp_simulate(n = 100L, tau = floor(n/2), mu1 = 0,
  mu2 = 1, sigma = 1, seed = NULL)
}
\arguments{
\item{n}{Number of observations.}
\item{tau}{True change-point location.}
\item{mu1,mu2}{Segment means.}
\item{sigma}{Common standard deviation.}
\item{seed}{Optional random seed.}
}
\value{A list containing the series and truth.}
\description{Generates independent Gaussian observations with one mean change.}

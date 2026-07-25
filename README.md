# ExactBayesCP

**ExactBayesCP** is an initial research-grade R package for exact Bayesian Gaussian
single-change analysis with reproducible, fault-tolerant simulation workflows.

## Core capabilities

- Exact collapsed posterior mass function for an unknown change point.
- Normal--Inverse-Gamma conjugate inference.
- Weak, moderate, strong, and custom informative priors.
- Direct posterior simulation without MCMC.
- MAP, posterior mean, credible intervals, entropy, and posterior concentration.
- Validation of numerical outputs.
- Atomic scenario-level checkpoints.
- Automatic recovery and skipping of completed scenarios.
- Reproducible Monte Carlo study execution.
- Nile River example.

## Installation

Install the development version from GitHub:

```r
install.packages("remotes")
remotes::install_github("drtaiwo/ExactBayesCP")
library(ExactBayesCP)
```

## Quick example

```r
library(ExactBayesCP)

dat <- bayescp_simulate(
  n = 100,
  tau = 50,
  mu1 = 0,
  mu2 = 1,
  sigma = 1,
  seed = 123
)

prior <- bayescp_prior(
  m01 = 0,
  m02 = 1,
  kappa01 = 5,
  kappa02 = 5,
  a0 = 2,
  b0 = 1,
  label = "moderate aligned"
)

fit <- bayescp_fit(dat$y, prior = prior, draws = 2000, seed = 123)
print(fit)
summary(fit)
plot(fit)
```

## Fault-tolerant simulation study

```r
scenarios <- expand.grid(
  n = c(50, 100, 200),
  delta = c(0.5, 1, 2),
  rho = 0.5,
  KEEP.OUT.ATTRS = FALSE
)
scenarios$scenario_id <- sprintf("P%03d", seq_len(nrow(scenarios)))
scenarios <- scenarios[c("scenario_id", "n", "delta", "rho")]

prior_factory <- function(sc) {
  bayescp_prior_grid(m01 = 0, m02 = as.numeric(sc$delta))
}

result <- bayescp_run_study(
  scenarios = scenarios,
  priors = prior_factory,
  replications = 500,
  draws = 2000,
  output_dir = "final_study",
  master_seed = 20260724
)
```

If the computer shuts down, run the same command again. Valid scenario
checkpoints are loaded automatically, and execution resumes at the first
unfinished or invalid scenario.

## Development status

Version 0.2.2 is the first release under the `ExactBayesCP` name. It retains
the validated exact Gaussian single-change computational core while updating
the package identity, citation metadata, documentation, examples, and repository
references. The current statistical scope is a single change in the Gaussian
mean with a common variance across segments.

## Version 0.2.0 additions

```r
plot(fit, type = "histogram")
plot(fit, type = "density_mu1")
plot(fit, type = "density_mu2")
plot(fit, type = "density_sigma2")
plot(fit, type = "credible")
bayescp_diagnostics(fit)
bayescp_dependencies()
bayescp_save_plots(fit, directory = "figures")
```

Compatible incomplete checkpoints are extended from the first missing replication.

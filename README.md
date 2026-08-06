# ExactBayesCP

**Exact Bayesian Gaussian Single Change-Point Analysis Using Informative Conjugate Priors**

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.21554870.svg)](https://doi.org/10.5281/zenodo.21554870)

ExactBayesCP is an R package for exact Bayesian inference for a single
change point in the mean of a Gaussian sequence under a common variance
using informative conjugate priors.


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

## Flexible prior specification in version 0.3.0

All prior interfaces are converted internally to the same
Normal--Inverse--Gamma representation, preserving exact inference.

```r
# Preset concentration
p_weak <- bayescp_prior(preset = "weak", m01 = 0, m02 = 1)
p_strong <- bayescp_prior("strong")

# Direct conjugate hyperparameters
p_direct <- bayescp_prior(
  m01 = 0, m02 = 1,
  kappa01 = 5, kappa02 = 5,
  a0 = 3, b0 = 2
)

# Scaled inverse-chi-square variance prior
p_chisq <- bayescp_prior(
  m01 = 0, m02 = 1,
  kappa01 = 5, kappa02 = 5,
  variance_prior = "scaled_inv_chisq",
  nu0 = 10, s02 = 4
)

# Expert-knowledge interface
p_expert <- bayescp_prior(
  mu1 = list(mean = 10, sd = 2),
  mu2 = list(mean = 18, sd = 3),
  variance = list(mean = 5, strength = 12),
  label = "expert prior"
)

summary(p_expert)
plot(p_expert)
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


## Citation

If you use ExactBayesCP in research, please cite:

> Adegoke, T. M., Yahya, W. B., & Oladoja, O. M. (2026).
> *ExactBayesCP: Exact Bayesian Gaussian Single Change-Point Analysis Using
> Informative Conjugate Priors* (Version 0.3.0) [Computer software].
> Zenodo. https://doi.org/10.5281/zenodo.21554870

In R, the citation can be obtained using:

```r
citation("ExactBayesCP")
```


## New developer features in version 0.3.0

Version 0.3.0 adds the manuscript-facing tools needed for uncertainty-aware
operating-characteristic analysis:

```r
# No-change versus one-change model comparison
mc <- bayescp_compare_models(
  dat$y,
  prior_change = prior,
  prior_no_change = bayescp_prior(m01 = mean(dat$y))
)
print(mc)

# Prior-data conflict at the posterior MAP
bayescp_conflict(fit)

# Monte Carlo summaries and paired prior comparisons
oc <- bayescp_summarise_study(result)
cmp <- bayescp_compare_priors(
  result,
  benchmark = "weak",
  margins = c(0.10, 0.05, 0.15),
  bootstrap = 1000
)

# Runtime benchmark for exact collapsed inference
bayescp_benchmark()
```

Checkpoint compatibility now depends on a configuration signature incorporating
the scenario, priors, minimum segment length, seed, draw count, and package
version. This prevents stale results from being loaded after a study design or
prior specification changes.

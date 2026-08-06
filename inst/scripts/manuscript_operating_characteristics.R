# ExactBayesCP 0.3.0 manuscript workflow
library(ExactBayesCP)

scenarios <- expand.grid(
  n = c(50L, 100L, 200L),
  delta = c(0.5, 1, 2),
  rho = 0.5,
  KEEP.OUT.ATTRS = FALSE
)
scenarios$scenario_id <- sprintf("P%03d", seq_len(nrow(scenarios)))
scenarios <- scenarios[c("scenario_id", "n", "delta", "rho")]

prior_factory <- function(sc) {
  delta <- as.numeric(sc$delta)  # actual signal: do not replace by max(delta, 1)
  mu1 <- 0
  mu2 <- delta
  d <- c(0, 0.25, 0.50, 0.75, 1.00, 2.00)

  out <- list(
    weak = bayescp_prior(m01 = mu1, m02 = mu2, kappa01 = 0.1, kappa02 = 0.1),
    moderate_aligned_d0 = bayescp_prior(m01 = mu1, m02 = mu2, kappa01 = 5, kappa02 = 5),
    strong_aligned_d0 = bayescp_prior(m01 = mu1, m02 = mu2, kappa01 = 20, kappa02 = 20)
  )
  for (dd in d[d > 0]) {
    out[[paste0("moderate_inward_d", dd)]] <- bayescp_prior(
      m01 = mu1 + dd * delta / 2, m02 = mu2 - dd * delta / 2,
      kappa01 = 5, kappa02 = 5)
    out[[paste0("moderate_outward_d", dd)]] <- bayescp_prior(
      m01 = mu1 - dd * delta / 2, m02 = mu2 + dd * delta / 2,
      kappa01 = 5, kappa02 = 5)
    out[[paste0("strong_inward_d", dd)]] <- bayescp_prior(
      m01 = mu1 + dd * delta / 2, m02 = mu2 - dd * delta / 2,
      kappa01 = 20, kappa02 = 20)
    out[[paste0("strong_outward_d", dd)]] <- bayescp_prior(
      m01 = mu1 - dd * delta / 2, m02 = mu2 + dd * delta / 2,
      kappa01 = 20, kappa02 = 20)
  }
  out
}

# draws = 0 is sufficient for tau operating characteristics and is much faster.
study <- bayescp_run_study(
  scenarios = scenarios,
  priors = prior_factory,
  replications = 500,
  draws = 0,
  output_dir = "publication_final_v030",
  master_seed = 20260724
)

oc <- bayescp_summarise_study(study)
cmp <- bayescp_compare_priors(
  study,
  benchmark = "weak",
  margins = c(0.10, 0.05, 0.15),
  bootstrap = 1000
)

write.csv(oc, "publication_final_v030/operating_characteristics.csv", row.names = FALSE)
write.csv(cmp, "publication_final_v030/paired_prior_comparisons.csv", row.names = FALSE)

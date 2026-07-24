library(BayesCP)

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
  output_dir = "bayescp_final_study",
  master_seed = 20260724
)

saveRDS(result, "bayescp_final_study_result.rds")

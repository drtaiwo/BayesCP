test_that("model comparison probabilities sum to one", {
  dat <- bayescp_simulate(60, 30, 0, 1, seed = 7)
  out <- bayescp_compare_models(
    dat$y,
    prior_change = bayescp_prior(m01 = 0, m02 = 1),
    prior_no_change = bayescp_prior(m01 = 0.5)
  )
  expect_s3_class(out, "bayescp_model_comparison")
  expect_equal(sum(out$posterior_model_probability), 1, tolerance = 1e-10)
})

test_that("conflict diagnostic is finite", {
  dat <- bayescp_simulate(60, 30, 0, 1, seed = 8)
  fit <- bayescp_fit(dat$y, bayescp_prior(m01 = 0, m02 = 1))
  z <- bayescp_conflict(fit)
  expect_true(all(is.finite(unlist(z))))
})

test_that("study summaries and paired comparisons work", {
  td <- tempfile("bayescp-v030-")
  scenarios <- data.frame(scenario_id = "P001", n = 40, delta = 1, rho = 0.5)
  priors <- list(
    weak = bayescp_prior(m01 = 0, m02 = 1, kappa01 = 0.1, kappa02 = 0.1),
    moderate = bayescp_prior(m01 = 0, m02 = 1, kappa01 = 5, kappa02 = 5)
  )
  st <- bayescp_run_study(scenarios, priors, replications = 10,
                          output_dir = td, progress = FALSE)
  oc <- bayescp_summarise_study(st)
  cmp <- bayescp_compare_priors(st, benchmark = "weak", bootstrap = 100)
  expect_true(all(c("rmse_mcse", "coverage_lower") %in% names(oc)))
  expect_true(all(c("relative_rmse_lower", "classification_margin_10pct") %in% names(cmp)))
})

test_that("checkpoint signature invalidates changed prior configuration", {
  td <- tempfile("bayescp-signature-")
  scenarios <- data.frame(scenario_id = "P001", n = 40, delta = 1, rho = 0.5)
  a <- bayescp_run_study(scenarios, list(weak = bayescp_prior()),
                         replications = 2, output_dir = td, progress = FALSE)
  b <- bayescp_run_study(scenarios,
                         list(weak = bayescp_prior(kappa01 = 5, kappa02 = 5)),
                         replications = 2, output_dir = td, progress = FALSE)
  chk <- bayescp_read_checkpoint("P001", td)
  expect_equal(chk$priors$weak$kappa01, 5)
})

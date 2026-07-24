test_that("exact fit produces a valid posterior", {
  dat <- bayescp_simulate(n = 60, tau = 30, mu1 = 0, mu2 = 2, seed = 11)
  p <- bayescp_prior(m01 = 0, m02 = 2, kappa01 = 0.1, kappa02 = 0.1)
  fit <- bayescp_fit(dat$y, p, min_seg = 5, draws = 100, seed = 11)

  expect_s3_class(fit, "bayescp_fit")
  expect_equal(sum(fit$posterior_prob), 1, tolerance = 1e-10)
  expect_true(all(bayescp_validate_fit(fit)))
  expect_equal(nrow(fit$draws), 100)
})

test_that("checkpoint recovery reuses completed scenarios", {
  td <- tempfile("bayescp-test-")
  dir.create(td)
  scenarios <- data.frame(
    scenario_id = "P001", n = 40, delta = 1, rho = 0.5
  )
  priors <- list(weak = bayescp_prior(m01 = 0, m02 = 1))
  first <- bayescp_run_study(
    scenarios, priors, replications = 2, output_dir = td,
    min_seg = 5, master_seed = 99, progress = FALSE
  )
  second <- bayescp_run_study(
    scenarios, priors, replications = 2, output_dir = td,
    min_seg = 5, master_seed = 99, progress = FALSE
  )
  expect_equal(first$results, second$results)
})

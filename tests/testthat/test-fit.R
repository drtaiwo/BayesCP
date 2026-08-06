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

test_that("preset prior interface is backward compatible", {
  p <- bayescp_prior(preset = "moderate", m01 = 0, m02 = 2)
  expect_s3_class(p, "bayescp_prior")
  expect_equal(p$kappa01, 5)
  expect_equal(p$kappa02, 5)
  expect_equal(p$m02, 2)
})

test_that("scaled inverse-chi-square maps to inverse-gamma", {
  p <- bayescp_prior(
    variance_prior = "scaled_inv_chisq",
    nu0 = 10, s02 = 4
  )
  expect_equal(p$a0, 5)
  expect_equal(p$b0, 20)
  expect_equal(p$variance_prior$parameterization, "scaled_inv_chisq")
})

test_that("expert interface preserves requested variance mean", {
  p <- bayescp_prior(
    mu1 = list(mean = 10, sd = 2),
    mu2 = list(mean = 18, sd = 3),
    variance = list(mean = 5, strength = 12)
  )
  expect_equal(p$m01, 10)
  expect_equal(p$m02, 18)
  expect_equal(p$variance_prior$prior_mean, 5, tolerance = 1e-12)
  expect_equal(p$kappa01, 5 / 4, tolerance = 1e-12)
  expect_equal(p$kappa02, 5 / 9, tolerance = 1e-12)
})

test_that("invalid expert variance strength is rejected", {
  expect_error(
    bayescp_prior(variance = list(mean = 5, strength = 2)),
    "greater than 2"
  )
})

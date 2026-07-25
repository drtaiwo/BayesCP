# ============================================================
# ExactBayesCP: Posterior Analysis Example
# ============================================================

library(ExactBayesCP)

# ------------------------------------------------------------
# 1. Simulate an ordered Gaussian series
# ------------------------------------------------------------

dat <- bayescp_simulate(
  n = 100,
  tau = 50,
  mu1 = 0,
  mu2 = 1,
  sigma = 1,
  seed = 123
)

# ------------------------------------------------------------
# 2. Specify an informative prior
# ------------------------------------------------------------

prior <- bayescp_prior(
  m01 = 0,
  m02 = 1,
  kappa01 = 5,
  kappa02 = 5,
  label = "moderate aligned"
)

# ------------------------------------------------------------
# 3. Fit the Bayesian change-point model
# ------------------------------------------------------------

fit <- bayescp_fit(
  y = dat$y,
  prior = prior,
  min_seg = 5,
  level = 0.95,
  draws = 2000,
  seed = 123
)

# ------------------------------------------------------------
# 4. Print the main posterior summary
# ------------------------------------------------------------

print(fit)
summary(fit)

# ------------------------------------------------------------
# 5. Access individual posterior summaries
# ------------------------------------------------------------

fit$posterior$map
fit$posterior$mean
fit$posterior$median
fit$posterior$sd
fit$posterior$interval
fit$posterior$entropy
fit$posterior$normalized_entropy
fit$posterior$p_max

# ------------------------------------------------------------
# 6. Inspect the posterior probability of every candidate
# ------------------------------------------------------------

posterior_table <- data.frame(
  tau = fit$tau,
  posterior_probability = fit$posterior_prob
)

head(posterior_table)
posterior_table[
  order(posterior_table$posterior_probability, decreasing = TRUE),
][1:10, ]

# Verify that the posterior probabilities sum to one
sum(fit$posterior_prob)

# ------------------------------------------------------------
# 7. Inspect direct posterior draws
# ------------------------------------------------------------

head(fit$draws)
summary(fit$draws)

# Posterior summaries for the continuous parameters
quantile(fit$draws$mu1, c(0.025, 0.50, 0.975))
quantile(fit$draws$mu2, c(0.025, 0.50, 0.975))
quantile(fit$draws$sigma2, c(0.025, 0.50, 0.975))

# ------------------------------------------------------------
# 8. Produce posterior plots
# ------------------------------------------------------------

plot(fit, type = "posterior")
plot(fit, type = "credible")
plot(fit, type = "histogram")
plot(fit, type = "density_mu1")
plot(fit, type = "density_mu2")
plot(fit, type = "density_sigma2")

# ------------------------------------------------------------
# 9. Run package diagnostics
# ------------------------------------------------------------

bayescp_diagnostics(fit)

# ------------------------------------------------------------
# 10. Save standard figures
# ------------------------------------------------------------

bayescp_save_plots(
  fit,
  directory = "figures",
  formats = c("png", "pdf"),
  types = c(
    "posterior",
    "series",
    "credible",
    "histogram",
    "density_mu1",
    "density_mu2",
    "density_sigma2"
  )
)
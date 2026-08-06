library(ExactBayesCP)

# Preset prior
p1 <- bayescp_prior(preset = "weak", m01 = 0, m02 = 1)

# Direct Normal--Inverse--Gamma hyperparameters
p2 <- bayescp_prior(
  m01 = 0, m02 = 1,
  kappa01 = 5, kappa02 = 5,
  a0 = 3, b0 = 2
)

# Scaled inverse-chi-square parameterization
p3 <- bayescp_prior(
  m01 = 0, m02 = 1,
  kappa01 = 5, kappa02 = 5,
  variance_prior = "scaled_inv_chisq",
  nu0 = 10, s02 = 4
)

# Expert-knowledge specification
p4 <- bayescp_prior(
  mu1 = list(mean = 10, sd = 2),
  mu2 = list(mean = 18, sd = 3),
  variance = list(mean = 5, strength = 12),
  label = "expert prior"
)

print(p4)
summary(p4)
plot(p4)

# ExactBayesCP 0.3.0

- Added exact no-change versus one-change model comparison.
- Added standardized prior-data conflict diagnostics.
- Added Monte Carlo standard errors and confidence intervals for study summaries.
- Added paired bootstrap relative-RMSE comparisons and cutoff-sensitivity classifications.
- Added break-even threshold estimation and exact-runtime benchmarking.
- Expanded simulation to Student-t errors, unequal segment variances, contaminated normal errors, and AR(1) dependence.
- Added checkpoint configuration signatures to reject stale or incompatible results.
- Added exact/within-two recovery and data seeds to replicate-level study output.
- Preserved backward compatibility with version 0.2.4 core interfaces.

# ExactBayesCP 0.3.0

- Added flexible prior specification while preserving the exact Normal--Inverse--Gamma inference engine.
- Added preset prior inputs through `preset = "weak"`, `"moderate"`, or `"strong"`.
- Added scaled inverse-chi-square variance-prior input via `nu0` and `s02`, with automatic conversion to inverse-gamma hyperparameters.
- Added an expert-knowledge interface using prior means, prior standard deviations, a variance mean, and a variance-strength parameter.
- Added `print()`, `summary()`, and `plot()` methods for `bayescp_prior` objects.
- Extended `bayescp_prior_grid()` to support either inverse-gamma or scaled inverse-chi-square variance inputs.
- Retained full backward compatibility with direct `m01`, `m02`, `kappa01`, `kappa02`, `a0`, and `b0` inputs.

# ExactBayesCP 0.2.2

- Renamed the package from `BayesCP` to `ExactBayesCP` to provide a distinct software identity and avoid confusion with previously used Bayesian change-point software names.
- Retained the validated `bayescp_*` user-facing function names and `bayescp_fit` S3 class for API continuity.
- Updated package metadata, documentation, examples, tests, citation information, and repository references.
- Updated the recommended software citation to credit all three package authors.

# ExactBayesCP 0.2.0

- Added histogram, density, and credible-interval plot types.
- Added `bayescp_diagnostics()` and `bayescp_dependencies()`.
- Added `bayescp_save_plots()` for PNG/PDF export.
- Added incremental continuation from compatible checkpoints.
- Removed unused `LazyData` metadata.

# ExactBayesCP 0.1.0

- Initial exact Gaussian single-change model and scenario-level checkpointing.

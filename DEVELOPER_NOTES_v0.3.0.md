# ExactBayesCP developer notes — v0.3.0

## Manuscript-facing additions

- `bayescp_compare_models()` supplies exact no-change versus one-change posterior model probabilities.
- `bayescp_conflict()` reports prior weights, conflict penalties, and standardized conflict scores.
- `bayescp_summarise_study()` reports MCSEs and confidence intervals.
- `bayescp_compare_priors()` performs paired bootstrap relative-RMSE comparisons and 5/10/15% cutoff sensitivity.
- `bayescp_break_even()` estimates first crossing thresholds from comparison tables.
- `bayescp_benchmark()` produces the runtime table required for the computational manuscript.

## Safe long-run workflow

Use `draws = 0` for the full change-point operating-characteristic study. All tau summaries are exact; posterior draws are only needed when continuous-parameter summaries are required. Checkpoints now include a configuration signature and are invalidated after changes to scenarios, priors, minimum segment length, seed, draw count, or package version.

## Prior displacement

The package does not silently rescale `delta`. A prior factory must use the actual scenario signal. For example, inward displacement is based on `as.numeric(sc$delta)`, not `max(abs(delta), 1)`.

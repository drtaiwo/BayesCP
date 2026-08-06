# Internal Monte Carlo helpers
.bayescp_mcse_mean <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2L) return(NA_real_)
  stats::sd(x) / sqrt(length(x))
}

.bayescp_mcse_prop <- function(x) {
  x <- as.numeric(x[!is.na(x)])
  if (!length(x)) return(NA_real_)
  p <- mean(x)
  sqrt(p * (1 - p) / length(x))
}

.bayescp_normal_ci <- function(est, se, level = 0.95, lower = -Inf, upper = Inf) {
  z <- stats::qnorm(1 - (1 - level) / 2)
  c(lower = max(lower, est - z * se), upper = min(upper, est + z * se))
}

#' Summarize Monte Carlo operating characteristics
#'
#' Summarizes replicate-level output from [bayescp_run_study()] and reports
#' Monte Carlo standard errors and confidence intervals.
#'
#' @param x A result returned by `bayescp_run_study()` or its results data frame.
#' @param level Confidence level for Monte Carlo intervals.
#' @return A data frame of operating-characteristic summaries.
#' @export
bayescp_summarise_study <- function(x, level = 0.95) {
  dat <- if (is.list(x) && is.data.frame(x$results)) x$results else x
  if (!is.data.frame(dat)) stop("Supply study results or a results data frame.", call. = FALSE)
  req <- c("scenario_id", "prior", "error", "absolute_error", "squared_error",
           "covered", "entropy", "normalized_entropy", "p_max")
  if (!all(req %in% names(dat))) {
    stop("Study results are missing required replicate-level columns.", call. = FALSE)
  }
  keys <- intersect(c("scenario_id", "n", "delta", "rho", "prior"), names(dat))
  groups <- interaction(dat[keys], drop = TRUE, lex.order = TRUE)
  pieces <- split(dat, groups)
  out <- lapply(pieces, function(d) {
    mse <- mean(d$squared_error)
    rmse <- sqrt(mse)
    rmse_se <- if (mse > 0) .bayescp_mcse_mean(d$squared_error) / (2 * rmse) else 0
    cov <- mean(d$covered)
    cov_se <- .bayescp_mcse_prop(d$covered)
    rmse_ci <- .bayescp_normal_ci(rmse, rmse_se, level, lower = 0)
    cov_ci <- .bayescp_normal_ci(cov, cov_se, level, lower = 0, upper = 1)
    data.frame(
      d[1, keys, drop = FALSE],
      R = nrow(d),
      bias = mean(d$error), bias_mcse = .bayescp_mcse_mean(d$error),
      mae = mean(d$absolute_error), mae_mcse = .bayescp_mcse_mean(d$absolute_error),
      rmse = rmse, rmse_mcse = rmse_se,
      rmse_lower = rmse_ci[1], rmse_upper = rmse_ci[2],
      exact_recovery = if ("exact" %in% names(d)) mean(d$exact) else mean(d$error == 0),
      within2_recovery = if ("within2" %in% names(d)) mean(d$within2) else mean(abs(d$error) <= 2),
      coverage = cov, coverage_mcse = cov_se,
      coverage_lower = cov_ci[1], coverage_upper = cov_ci[2],
      mean_interval_width = mean(d$interval_upper - d$interval_lower),
      mean_entropy = mean(d$entropy),
      mean_normalized_entropy = mean(d$normalized_entropy),
      mean_p_max = mean(d$p_max),
      row.names = NULL
    )
  })
  do.call(rbind, out)
}

#' Paired comparison of informative priors with a benchmark
#'
#' Uses paired bootstrap resampling of simulation replications to estimate
#' relative RMSE and its uncertainty.
#'
#' @param x Study result object or replicate-level results data frame.
#' @param benchmark Name of the benchmark prior.
#' @param margins Practical-equivalence margins. The first is used for the
#'   primary classification; all are returned in sensitivity columns.
#' @param bootstrap Number of paired bootstrap samples.
#' @param level Bootstrap confidence level.
#' @param seed Random seed.
#' @return A data frame of paired prior comparisons.
#' @export
bayescp_compare_priors <- function(x, benchmark = "weak",
                                   margins = c(0.10, 0.05, 0.15),
                                   bootstrap = 1000L, level = 0.95,
                                   seed = 20260724L) {
  dat <- if (is.list(x) && is.data.frame(x$results)) x$results else x
  req <- c("scenario_id", "replication", "prior", "squared_error", "absolute_error")
  if (!is.data.frame(dat) || !all(req %in% names(dat))) {
    stop("Replicate-level study results are required.", call. = FALSE)
  }
  margins <- as.numeric(margins)
  if (!length(margins) || any(!is.finite(margins)) || any(margins <= 0 | margins >= 1)) {
    stop("`margins` must contain values strictly between zero and one.", call. = FALSE)
  }
  bootstrap <- as.integer(bootstrap)
  if (bootstrap < 100L) stop("Use at least 100 bootstrap samples.", call. = FALSE)
  set.seed(seed)
  key_extra <- intersect(c("n", "delta", "rho"), names(dat))
  scenarios <- unique(dat$scenario_id)
  rows <- list(); pos <- 1L
  alpha <- (1 - level) / 2
  classify <- function(lo, hi, margin) {
    if (hi < 1 - margin) "Beneficial" else if (lo > 1 + margin) "Harmful" else "Practically negligible / inconclusive"
  }
  for (sid in scenarios) {
    ds <- dat[dat$scenario_id == sid, , drop = FALSE]
    b <- ds[ds$prior == benchmark, c("replication", "squared_error", "absolute_error"), drop = FALSE]
    names(b)[-1] <- c("benchmark_squared_error", "benchmark_absolute_error")
    if (!nrow(b)) stop("Benchmark prior not found in scenario: ", sid, call. = FALSE)
    for (pr in setdiff(unique(ds$prior), benchmark)) {
      q <- ds[ds$prior == pr, , drop = FALSE]
      q <- merge(q, b, by = "replication", all = FALSE, sort = FALSE)
      if (!nrow(q)) next
      rhat <- sqrt(mean(q$squared_error)) / sqrt(mean(q$benchmark_squared_error))
      br <- replicate(bootstrap, {
        ii <- sample.int(nrow(q), nrow(q), replace = TRUE)
        sqrt(mean(q$squared_error[ii])) / sqrt(mean(q$benchmark_squared_error[ii]))
      })
      ci <- stats::quantile(br, c(alpha, 1 - alpha), na.rm = TRUE, names = FALSE)
      base <- data.frame(
        scenario_id = sid,
        prior = pr,
        benchmark = benchmark,
        R = nrow(q),
        rmse_informative = sqrt(mean(q$squared_error)),
        rmse_benchmark = sqrt(mean(q$benchmark_squared_error)),
        relative_rmse = rhat,
        relative_rmse_lower = ci[1],
        relative_rmse_upper = ci[2],
        paired_mean_squared_error_difference = mean(q$squared_error - q$benchmark_squared_error),
        paired_mean_absolute_error_difference = mean(q$absolute_error - q$benchmark_absolute_error),
        stringsAsFactors = FALSE
      )
      for (nm in key_extra) base[[nm]] <- q[[nm]][1]
      for (m in margins) {
        nm <- paste0("classification_margin_", formatC(100 * m, format = "fg"), "pct")
        base[[nm]] <- classify(ci[1], ci[2], m)
      }
      rows[[pos]] <- base; pos <- pos + 1L
    }
  }
  do.call(rbind, rows)
}

#' Estimate break-even prior-displacement thresholds
#'
#' Estimates the first displacement at which relative RMSE reaches one. If
#' replicate-level results are supplied, a paired bootstrap confidence interval
#' for the threshold is also computed.
#'
#' @param comparisons Output from [bayescp_compare_priors()].
#' @param displacement Numeric displacement corresponding to each prior. Either
#'   a named vector keyed by prior name or a column name in `comparisons`.
#' @param direction Optional direction corresponding to each prior. Either a
#'   named vector or a column name.
#' @param group Additional grouping columns.
#' @return A data frame of break-even estimates and crossing status.
#' @export
bayescp_break_even <- function(comparisons, displacement = "displacement",
                               direction = "direction",
                               group = c("scenario_id")) {
  if (!is.data.frame(comparisons) || !"relative_rmse" %in% names(comparisons)) {
    stop("`comparisons` must be produced by `bayescp_compare_priors()` or contain `relative_rmse`.", call. = FALSE)
  }
  attach_value <- function(spec, name) {
    if (length(spec) == 1L && is.character(spec) && spec %in% names(comparisons)) return(comparisons[[spec]])
    if (!is.null(names(spec))) return(unname(spec[as.character(comparisons$prior)]))
    if (length(spec) == nrow(comparisons)) return(spec)
    stop("Unable to resolve `", name, "`.", call. = FALSE)
  }
  comparisons$.displacement <- as.numeric(attach_value(displacement, "displacement"))
  comparisons$.direction <- as.character(attach_value(direction, "direction"))
  if (anyNA(comparisons$.displacement)) stop("Missing displacement values.", call. = FALSE)
  gcols <- unique(c(group, ".direction"))
  gcols <- gcols[gcols %in% names(comparisons)]
  groups <- interaction(comparisons[gcols], drop = TRUE, lex.order = TRUE)
  parts <- split(comparisons, groups)
  out <- lapply(parts, function(d) {
    d <- d[order(d$.displacement), , drop = FALSE]
    x <- d$.displacement; y <- d$relative_rmse - 1
    crossing <- which(y[-length(y)] * y[-1] <= 0 & y[-length(y)] != y[-1])
    status <- "Interpolated"
    if (all(y < 0)) { est <- NA_real_; status <- "Not reached: informative prior remained better" }
    else if (all(y > 0)) { est <- min(x); status <- "Harmful at first evaluated displacement" }
    else if (!length(crossing)) { est <- NA_real_; status <- "No stable crossing" }
    else {
      i <- crossing[1]
      est <- x[i] - y[i] * (x[i + 1] - x[i]) / (y[i + 1] - y[i])
      if (length(crossing) > 1L) status <- "Multiple crossings; first reported"
    }
    data.frame(d[1, gcols, drop = FALSE], break_even_displacement = est,
               status = status, number_of_crossings = length(crossing), row.names = NULL)
  })
  do.call(rbind, out)
}

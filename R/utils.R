.bayescp_assert_numeric_series <- function(y) {
  if (!is.numeric(y) || length(y) < 4L) {
    stop("`y` must be a numeric vector containing at least four observations.",
         call. = FALSE)
  }
  if (anyNA(y) || any(!is.finite(y))) {
    stop("`y` must contain only finite, non-missing values.", call. = FALSE)
  }
  invisible(TRUE)
}

.bayescp_assert_scalar <- function(x, name, lower = -Inf, strict = FALSE) {
  ok <- is.numeric(x) && length(x) == 1L && is.finite(x)
  if (strict) ok <- ok && x > lower else ok <- ok && x >= lower
  if (!ok) {
    cmp <- if (strict) "greater than" else "at least"
    stop(sprintf("`%s` must be a finite scalar %s %s.", name, cmp, lower),
         call. = FALSE)
  }
  invisible(TRUE)
}

.bayescp_log_sum_exp <- function(x) {
  if (!length(x) || any(!is.finite(x))) {
    stop("Log weights must be finite and non-empty.", call. = FALSE)
  }
  m <- max(x)
  m + log(sum(exp(x - m)))
}

.bayescp_atomic_save_rds <- function(object, file) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(file, ".tmp-", Sys.getpid(), "-", sprintf("%.0f", runif(1, 1, 1e9)))
  saveRDS(object, tmp)
  if (!file.rename(tmp, file)) {
    unlink(tmp)
    stop("Unable to atomically write checkpoint: ", file, call. = FALSE)
  }
  invisible(file)
}

.bayescp_equal_tailed_interval <- function(tau, prob, level) {
  cdf <- cumsum(prob)
  alpha <- (1 - level) / 2
  lower <- tau[which(cdf >= alpha)[1L]]
  upper <- tau[which(cdf >= 1 - alpha)[1L]]
  c(lower = lower, upper = upper)
}

.bayescp_smallest_set <- function(tau, prob, level) {
  ord <- order(prob, decreasing = TRUE)
  keep <- ord[seq_len(which(cumsum(prob[ord]) >= level)[1L])]
  sort(tau[keep])
}

.bayescp_sse <- function(sum_y, sum_y2, n) {
  out <- sum_y2 - (sum_y * sum_y) / n
  pmax(out, 0)
}

.bayescp_checkpoint_file <- function(output_dir, scenario_id) {
  file.path(output_dir, "checkpoints", paste0(scenario_id, ".rds"))
}

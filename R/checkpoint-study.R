.bayescp_validate_scenarios <- function(scenarios) {
  req <- c("scenario_id", "n", "delta", "rho")
  if (!is.data.frame(scenarios) || !all(req %in% names(scenarios))) {
    stop("`scenarios` must be a data frame containing: ",
         paste(req, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(scenarios$scenario_id)) {
    stop("Scenario identifiers must be unique.", call. = FALSE)
  }
  invisible(TRUE)
}

.bayescp_validate_checkpoint <- function(x, expected) {
  is.list(x) && is.data.frame(x$results) &&
    identical(x$metadata$scenario_id, expected$scenario_id) &&
    identical(x$metadata$draws, expected$draws) &&
    identical(x$metadata$master_seed, expected$master_seed) &&
    nrow(x$results) > 0L &&
    all(c("replication","prior","tau_hat","error","squared_error") %in% names(x$results))
}

#' Run a fault-tolerant Monte Carlo study
#'
#' Completed scenario checkpoints are validated and reused automatically.
#' Invalid or incompatible checkpoints are recomputed.
#'
#' @param scenarios Data frame with columns `scenario_id`, `n`, `delta`, and
#'   `rho`. Optional columns are `mu1` and `sigma`.
#' @param priors Named list of `bayescp_prior` objects or a function taking a
#'   scenario row and returning such a list.
#' @param replications Number of Monte Carlo replications per scenario.
#' @param draws Direct posterior draws per fitted prior.
#' @param output_dir Output directory.
#' @param min_seg Minimum segment length.
#' @param master_seed Master seed.
#' @param overwrite Recompute valid checkpoints when `TRUE`.
#' @param progress Print progress messages.
#' @return A list containing scenario results and checkpoint information.
#' @export
bayescp_run_study <- function(scenarios, priors,
                              replications = 100L,
                              draws = 0L,
                              output_dir = "bayescp_study",
                              min_seg = 5L,
                              master_seed = 20260724L,
                              overwrite = FALSE,
                              progress = TRUE) {
  .bayescp_validate_scenarios(scenarios)
  replications <- as.integer(replications)
  draws <- as.integer(draws)
  if (replications < 1L || draws < 0L) {
    stop("`replications` must be positive and `draws` non-negative.",
         call. = FALSE)
  }
  dir.create(file.path(output_dir, "checkpoints"),
             recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_dir, "logs"),
             recursive = TRUE, showWarnings = FALSE)

  collected <- vector("list", nrow(scenarios))
  names(collected) <- scenarios$scenario_id

  for (i in seq_len(nrow(scenarios))) {
    sc <- scenarios[i, , drop = FALSE]
    scenario_id <- as.character(sc$scenario_id)
    checkpoint <- .bayescp_checkpoint_file(output_dir, scenario_id)
    expected <- list(
      scenario_id = scenario_id,
      replications = replications,
      draws = draws,
      master_seed = as.integer(master_seed)
    )

    existing_results <- NULL
    start_rep <- 1L
    if (!overwrite && file.exists(checkpoint)) {
      saved <- tryCatch(readRDS(checkpoint), error = function(e) NULL)
      if (!is.null(saved) && .bayescp_validate_checkpoint(saved, expected)) {
        max_completed <- max(saved$results$replication)
        if (max_completed >= replications) {
          saved$results <- saved$results[saved$results$replication <= replications,,drop=FALSE]
          saved$metadata$replications <- replications
          if (progress) message("Loading valid checkpoint: ", scenario_id)
          collected[[scenario_id]] <- saved
          next
        }
        existing_results <- saved$results
        start_rep <- max_completed + 1L
        if (progress) message(sprintf("Resuming scenario: %s | completed=%d | continuing=%d:%d",scenario_id,max_completed,start_rep,replications))
      } else if (progress) message("Ignoring incompatible checkpoint: ", scenario_id)
    }

    n <- as.integer(sc$n)
    delta <- as.numeric(sc$delta)
    rho <- as.numeric(sc$rho)
    mu1 <- if ("mu1" %in% names(sc)) as.numeric(sc$mu1) else 0
    sigma <- if ("sigma" %in% names(sc)) as.numeric(sc$sigma) else 1
    tau0 <- max(min_seg, min(n - min_seg, floor(rho * n)))
    mu2 <- mu1 + delta * sigma

    scenario_priors <- if (is.function(priors)) priors(sc) else priors
    if (!is.list(scenario_priors) ||
        !all(vapply(scenario_priors, inherits, logical(1), "bayescp_prior"))) {
      stop("`priors` must be a list of ExactBayesCP priors or a function returning one.",
           call. = FALSE)
    }
    if (is.null(names(scenario_priors))) {
      names(scenario_priors) <- paste0("prior", seq_along(scenario_priors))
    }

    if (progress) {
      message(sprintf(
        "Running scenario: %s | n=%d | delta=%s | rho=%s",
        scenario_id, n, format(delta), format(rho)
      ))
    }

    rows <- vector("list", max(0L, replications-start_rep+1L) * length(scenario_priors))
    pos <- 1L

    for (r in seq.int(start_rep, replications)) {
      seed_r <- as.integer(master_seed + i * 100000L + r)
      dat <- bayescp_simulate(n, tau0, mu1, mu2, sigma, seed = seed_r)

      for (p in seq_along(scenario_priors)) {
        fit <- bayescp_fit(
          dat$y,
          prior = scenario_priors[[p]],
          min_seg = min_seg,
          draws = draws,
          seed = seed_r + p * 1000L
        )
        err <- fit$posterior$map - tau0
        rows[[pos]] <- data.frame(
          scenario_id = scenario_id,
          replication = r,
          prior = names(scenario_priors)[p],
          n = n,
          delta = delta,
          rho = rho,
          tau0 = tau0,
          tau_hat = fit$posterior$map,
          posterior_mean = fit$posterior$mean,
          interval_lower = fit$posterior$interval[1],
          interval_upper = fit$posterior$interval[2],
          covered = tau0 >= fit$posterior$interval[1] &&
            tau0 <= fit$posterior$interval[2],
          error = err,
          absolute_error = abs(err),
          squared_error = err^2,
          entropy = fit$posterior$entropy,
          normalized_entropy = fit$posterior$normalized_entropy,
          p_max = fit$posterior$p_max
        )
        pos <- pos + 1L
      }
    }

    new_results <- if (length(rows)) do.call(rbind, rows) else NULL
    result <- if (is.null(existing_results)) new_results else if (is.null(new_results)) existing_results else rbind(existing_results,new_results)
    checkpoint_object <- list(
      metadata = list(
        scenario_id = scenario_id,
        replications = replications,
        draws = draws,
        master_seed = master_seed,
        created_at = as.character(Sys.time()),
        package_version = "0.2.0"
      ),
      scenario = sc,
      results = result
    )
    .bayescp_atomic_save_rds(checkpoint_object, checkpoint)
    collected[[scenario_id]] <- checkpoint_object
  }

  combined <- do.call(rbind, lapply(collected, `[[`, "results"))
  final <- list(
    metadata = list(
      replications = replications,
      draws = draws,
      output_dir = normalizePath(output_dir, mustWork = FALSE),
      completed_at = as.character(Sys.time())
    ),
    scenarios = scenarios,
    results = combined,
    checkpoints = unname(vapply(
      scenarios$scenario_id,
      function(id) .bayescp_checkpoint_file(output_dir, id),
      character(1)
    ))
  )
  .bayescp_atomic_save_rds(final, file.path(output_dir, "combined_results.rds"))
  final
}

#' List ExactBayesCP checkpoints
#'
#' @param output_dir Study output directory.
#' @return A data frame describing available checkpoint files.
#' @export
bayescp_list_checkpoints <- function(output_dir = "bayescp_study") {
  path <- file.path(output_dir, "checkpoints")
  f <- list.files(path, pattern = "\\.rds$", full.names = TRUE)
  if (!length(f)) {
    return(data.frame(scenario_id = character(), file = character(),
                      valid_rds = logical()))
  }
  data.frame(
    scenario_id = sub("\\.rds$", "", basename(f)),
    file = f,
    valid_rds = vapply(f, function(z) {
      !is.null(tryCatch(readRDS(z), error = function(e) NULL))
    }, logical(1))
  )
}

#' Read one ExactBayesCP checkpoint
#'
#' @param scenario_id Scenario identifier.
#' @param output_dir Study output directory.
#' @return The checkpoint object.
#' @export
bayescp_read_checkpoint <- function(scenario_id,
                                    output_dir = "bayescp_study") {
  file <- .bayescp_checkpoint_file(output_dir, scenario_id)
  if (!file.exists(file)) stop("Checkpoint not found: ", scenario_id,
                               call. = FALSE)
  readRDS(file)
}

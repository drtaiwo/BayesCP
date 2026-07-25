#' Report ExactBayesCP dependency information
#'
#' @description
#' Reports the installed ExactBayesCP version, R version, declared imports,
#' suggested packages, and current session information.
#'
#' @return
#' A list containing the package version, R version, imported packages,
#' suggested packages, and session information.
#'
#' @examples
#' info <- bayescp_dependencies()
#' info$package_version
#' info$r_version
#'
#' @export
bayescp_dependencies <- function() {
  desc <- utils::packageDescription("ExactBayesCP")
  
  split_field <- function(x) {
    if (is.null(x) || !nzchar(x)) {
      character()
    } else {
      trimws(unlist(strsplit(x, ",")))
    }
  }
  
  list(
    package_version =
      as.character(utils::packageVersion("ExactBayesCP")),
    r_version = R.version.string,
    imports = split_field(desc$Imports),
    suggests = split_field(desc$Suggests),
    session_info = utils::sessionInfo()
  )
}


#' Run numerical diagnostics for a fitted ExactBayesCP model
#'
#' @description
#' Performs internal numerical and structural checks on an object returned by
#' [bayescp_fit()]. When posterior draws are available, the function also checks
#' that draws are present, finite, and that sampled change-point values lie
#' within the supported candidate set.
#'
#' @param object An object of class `bayescp_fit`.
#' @param print Logical. If `TRUE`, print the diagnostic results.
#'
#' @return
#' Invisibly returns a named logical vector containing the diagnostic checks.
#'
#' @examples
#' dat <- bayescp_simulate(
#'   n = 100,
#'   tau = 50,
#'   mu1 = 0,
#'   mu2 = 1,
#'   sigma = 1,
#'   seed = 123
#' )
#'
#' prior <- bayescp_prior(
#'   m01 = 0,
#'   m02 = 1,
#'   kappa01 = 5,
#'   kappa02 = 5
#' )
#'
#' fit <- bayescp_fit(
#'   dat$y,
#'   prior = prior,
#'   draws = 200,
#'   seed = 123
#' )
#'
#' bayescp_diagnostics(fit)
#'
#' @export
bayescp_diagnostics <- function(object, print = TRUE) {
  checks <- bayescp_validate_fit(object)
  
  if (!is.null(object$draws)) {
    checks <- c(
      checks,
      draws_present = nrow(object$draws) > 0,
      draws_finite = all(
        vapply(
          object$draws,
          function(z) all(is.finite(z)),
          logical(1)
        )
      ),
      tau_draws_in_support =
        all(object$draws$tau %in% object$tau)
    )
  }
  
  if (isTRUE(print)) {
    cat(
      "ExactBayesCP diagnostics\n",
      "------------------------\n",
      sep = ""
    )
    
    for (nm in names(checks)) {
      cat(
        sprintf(
          "%-30s %s\n",
          nm,
          if (checks[[nm]]) "PASSED" else "FAILED"
        )
      )
    }
    
    cat(
      "------------------------\n",
      "Package status: ",
      if (all(checks)) "HEALTHY" else "ATTENTION REQUIRED",
      "\n",
      sep = ""
    )
  }
  
  invisible(checks)
}


#' Save standard ExactBayesCP plots
#'
#' @description
#' Exports selected graphical summaries from a fitted `bayescp_fit` object
#' to PNG and/or PDF files.
#'
#' @param object An object of class `bayescp_fit`.
#' @param directory Character string giving the directory in which plot files
#'   should be saved.
#' @param formats Character vector containing one or both of `"png"` and
#'   `"pdf"`.
#' @param types Character vector specifying the plot types to export.
#' @param width Width of PNG output in pixels.
#' @param height Height of PNG output in pixels.
#' @param res Resolution of PNG output in dots per inch.
#'
#' @return
#' Invisibly returns a character vector containing the paths of created files.
#'
#' @examples
#' dat <- bayescp_simulate(
#'   n = 100,
#'   tau = 50,
#'   mu1 = 0,
#'   mu2 = 1,
#'   sigma = 1,
#'   seed = 123
#' )
#'
#' fit <- bayescp_fit(
#'   dat$y,
#'   prior = bayescp_prior(
#'     m01 = 0,
#'     m02 = 1,
#'     kappa01 = 5,
#'     kappa02 = 5
#'   ),
#'   draws = 100,
#'   seed = 123
#' )
#'
#' tmp <- tempfile("exactbayescp_plots_")
#'
#' files <- bayescp_save_plots(
#'   fit,
#'   directory = tmp,
#'   formats = "png",
#'   types = "posterior",
#'   width = 800,
#'   height = 600,
#'   res = 100
#' )
#'
#' files
#'
#' unlink(tmp, recursive = TRUE)
#'
#' @export
bayescp_save_plots <- function(
    object,
    directory = "bayescp_figures",
    formats = c("png", "pdf"),
    types = c("posterior", "series", "credible"),
    width = 1800,
    height = 1200,
    res = 200) {
  
  valid_formats <- c("png", "pdf")
  
  if (any(!formats %in% valid_formats)) {
    stop(
      "`formats` must contain only 'png' and/or 'pdf'.",
      call. = FALSE
    )
  }
  
  dir.create(
    directory,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  created <- character()
  
  for (fmt in formats) {
    for (tp in types) {
      
      file <- file.path(
        directory,
        paste0("bayescp_", tp, ".", fmt)
      )
      
      if (fmt == "png") {
        grDevices::png(
          file,
          width = width,
          height = height,
          res = res
        )
      } else {
        grDevices::pdf(
          file,
          width = width / 300,
          height = height / 300
        )
      }
      
      tryCatch(
        plot(object, type = tp),
        finally = grDevices::dev.off()
      )
      
      created <- c(created, file)
    }
  }
  
  invisible(created)
}
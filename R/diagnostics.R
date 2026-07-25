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
#' @export
bayescp_dependencies <- function() {
  desc <- utils::packageDescription("BayesCP")
  split_field <- function(x) if (is.null(x)||!nzchar(x)) character() else trimws(unlist(strsplit(x,",")))
  list(package_version=as.character(utils::packageVersion("BayesCP")),
       r_version=R.version.string, imports=split_field(desc$Imports),
       suggests=split_field(desc$Suggests), session_info=utils::sessionInfo())
}

#' @export
bayescp_diagnostics <- function(object, print=TRUE) {
  checks <- bayescp_validate_fit(object)
  if (!is.null(object$draws)) checks <- c(checks,
    draws_present=nrow(object$draws)>0,
    draws_finite=all(vapply(object$draws,function(z) all(is.finite(z)),logical(1))),
    tau_draws_in_support=all(object$draws$tau %in% object$tau))
  if (isTRUE(print)) {
    cat("BayesCP diagnostics\n-------------------\n")
    for (nm in names(checks)) cat(sprintf("%-30s %s\n",nm,if(checks[[nm]]) "PASSED" else "FAILED"))
    cat("-------------------\nPackage status:",if(all(checks)) "HEALTHY" else "ATTENTION REQUIRED","\n")
  }
  invisible(checks)
}

#' @export
bayescp_save_plots <- function(object,directory="bayescp_figures",formats=c("png","pdf"),
  types=c("posterior","series","credible"),width=1800,height=1200,res=200) {
  valid_formats <- c("png","pdf")
  if (any(!formats %in% valid_formats)) stop("`formats` must contain only 'png' and/or 'pdf'.",call.=FALSE)
  dir.create(directory,recursive=TRUE,showWarnings=FALSE); created <- character()
  for (fmt in formats) for (tp in types) {
    file <- file.path(directory,paste0("bayescp_",tp,".",fmt))
    if (fmt=="png") grDevices::png(file,width=width,height=height,res=res)
    else grDevices::pdf(file,width=width/300,height=height/300)
    tryCatch(plot(object,type=tp),finally=grDevices::dev.off()); created <- c(created,file)
  }
  invisible(created)
}

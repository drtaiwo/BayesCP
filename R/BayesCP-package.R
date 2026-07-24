#' BayesCP: Exact Bayesian Gaussian Change-Point Analysis
#'
#' @description
#' BayesCP provides exact conjugate Bayesian inference for a single
#' Gaussian change point with segment-specific means and a common variance.
#' The package supports collapsed posterior computation, direct posterior
#' simulation, posterior summaries, visualization, numerical validation,
#' checkpoint recovery, and reproducible Monte Carlo simulation workflows.
#'
#' @details
#' The principal functions are:
#'
#' \itemize{
#'   \item \code{\link{bayescp_fit}} for fitting a Bayesian change-point model;
#'   \item \code{\link{bayescp_prior}} for constructing conjugate priors;
#'   \item \code{\link{bayescp_simulate}} for generating Gaussian change-point data;
#'   \item \code{\link{bayescp_run_study}} for fault-tolerant simulation studies;
#'   \item \code{\link{bayescp_diagnostics}} for numerical validation;
#'   \item \code{\link{bayescp_save_plots}} for exporting standard figures.
#' }
#'
#' @author
#' Taiwo Adegoke
#'
#' @references
#' Adegoke, T. M. BayesCP: Exact Bayesian Gaussian Change-Point
#' Analysis with Reproducible Simulation Workflows.
#'
#' @keywords internal
#'
#' @docType package
#' @name BayesCP-package
#'
#' @aliases BayesCP
#'
#' @seealso
#' \code{\link{bayescp_fit}},
#' \code{\link{bayescp_prior}},
#' \code{\link{bayescp_simulate}},
#' \code{\link{bayescp_run_study}}
#'
#' @examples
#' dat <- bayescp_simulate(
#'   n = 100,
#'   tau = 50,
#'   mu1 = 0,
#'   mu2 = 1,
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
#'   draws = 100
#' )
#'
#' summary(fit)
#'
NULL
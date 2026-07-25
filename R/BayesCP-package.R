#' ExactBayesCP: Exact Bayesian Gaussian Single Change-Point Analysis
#'
#' @description
#' ExactBayesCP provides exact conjugate Bayesian inference for a single
#' Gaussian change point in the mean under a common variance. The package
#' supports weakly informative and informative conjugate priors, collapsed
#' posterior computation, direct posterior simulation, posterior summaries,
#' prior-sensitivity analysis, prior-data conflict assessment, posterior
#' robustness evaluation, numerical validation, checkpoint recovery, and
#' reproducible Monte Carlo simulation workflows.
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
#' ExactBayesCP is designed to support reproducible computational studies of
#' finite-sample Bayesian change-point inference, including investigations of
#' operating characteristics, informative-prior behaviour, prior-data conflict,
#' and posterior robustness.
#'
#' @author
#' Taiwo M. Adegoke, W. B. Yahya, and O. M. Oladoja
#'
#' @references
#' Adegoke, T. M., Yahya, W. B., and Oladoja, O. M. ExactBayesCP:
#' Exact Bayesian Gaussian Single Change-Point Analysis Using Informative
#' Conjugate Priors.
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
#' @keywords internal
"_PACKAGE"
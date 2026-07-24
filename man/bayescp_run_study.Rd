\name{bayescp_run_study}
\alias{bayescp_run_study}
\alias{bayescp_list_checkpoints}
\alias{bayescp_read_checkpoint}
\title{Run and Recover a Fault-Tolerant BayesCP Study}
\usage{
bayescp_run_study(scenarios, priors, replications = 100L,
  draws = 0L, output_dir = "bayescp_study", min_seg = 5L,
  master_seed = 20260724L, overwrite = FALSE, progress = TRUE)
bayescp_list_checkpoints(output_dir = "bayescp_study")
bayescp_read_checkpoint(scenario_id, output_dir = "bayescp_study")
}
\arguments{
\item{scenarios}{Scenario data frame.}
\item{priors}{List of priors or scenario-dependent prior function.}
\item{replications}{Replications per scenario.}
\item{draws}{Direct posterior draws per fit.}
\item{output_dir}{Study output directory.}
\item{min_seg}{Minimum segment length.}
\item{master_seed}{Master random seed.}
\item{overwrite}{Whether to recompute valid checkpoints.}
\item{progress}{Whether to print progress.}
\item{scenario_id}{Scenario identifier.}
}
\value{Study output, checkpoint index, or one checkpoint object.}
\description{Runs scenario-level simulations using atomic checkpoints and automatically reloads compatible completed work.}

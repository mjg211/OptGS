#' Simulate group-sequential clinical trials for a normally distributed primary
#' outcome
#'
#' \code{sim()} simulates group-sequential clinical trials in order to validate
#' certain analytically determined operating characteristics (as returned by
#' \code{\link{opchar}}) and also to evaluate additional characteristics
#' relating to post-trial inference.
#'
#' @param des An object of class OptGS_des, as returned by \code{\link{des_gs}},
#' \code{\link{des_nearopt}}, \code{\link{des_opt}}, or \code{\link{build}}.
#' Defaults to \code{des_gs()}.
#' @param tau A \code{\link{numeric}} \code{\link{vector}} indicating the values
#' of \ifelse{html}{\out{<i>&tau;</i>}}{\eqn{\tau}} to perform simulations for.
#' Defaults to \code{des$opchar$tau}.
#' @param adjusted A \code{\link{logical}} variable indicating whether adjusted
#' inference should also be carried out in the simulations. Defaults to
#' \code{F}.
#' @param alpha A \code{\link{numeric}} indicating the value of
#' \ifelse{html}{\out{<i>&alpha;</i>}}{\eqn{\alpha}} to use in confidence
#' interval construction. Defaults to \code{des$alpha}.
#' @param replicates A \code{\link{numeric}} indicating the number of replicate
#' simulations to use for each value of
#' \ifelse{html}{\out{<i>&tau;</i>}}{\eqn{\tau}}. Must be a whole number.
#' Defaults to \code{1e5}.
#' @param summary A \code{\link{logical}} variable indicating whether a summary
#' of the function's progress should be printed to the console. Defaults to
#' \code{F}.
#' @return A \code{\link{list}} with additional class \code{"OptGS_sim"}. It
#' will contain each of the input variables (subject to internal modification),
#' along with the following elements:
#' \itemize{
#' \item \code{sim}: A \code{\link{tibble}} giving the estimated operating
#' characteristics.
#' }
#' @examples
#' # Simulations for the default parameters
#' sim     <- sim()
#' # Increasing the number of tau values
#' sim_tau <- sim(tau = seq(-des$delta, 2*des$delta, length.out = 10))
#' @seealso \code{\link{an}}, \code{\link{des_gs}}, \code{\link{des_nearopt}},
#' \code{\link{des_opt}}, \code{\link{opchar}}, \code{\link{build}},
#' \code{\link{plot.OptGS_des}}, \code{\link{plot.OptGS_opchar}},
#' \code{\link{print.OptGS_des}}, \code{\link{summary.OptGS_des}}.
#' @export
sim <- function(des = des_gs(), tau = des$opchar$tau, adjusted = F,
                alpha = des$alpha, replicates = 1e5, summary = F) {

  ##### Check input variables ##################################################

  #check_OptGS_des(des, "des")
  #check_tau(tau)
  check_logical(adjusted, "adjusted")
  check_real_range_strict(alpha, "alpha", c(0, 1), 1)
  check_integer_range(replicates, "replicates", c(0, Inf), 1)
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    #summary_sim(des, tau, adjusted, alpha, replicates)
    message("")
  }

  ##### Perform main computations ##############################################

  len_tau       <- length(tau)
  sim           <- matrix(0, len_tau, 15)
  for (i in 1:len_tau) {
    sim[i, ]    <- sim_internal(des, tau[i], adjusted, alpha, replicates,
                                summary)
    if (summary) {
      message("...completed the simulations for tau = ", round(tau[i], 4),
              "...")
    }
  }
  sim           <- tibble::as_tibble(sim)
  colnames(sim) <- c("tau", "P_emp(tau)", "ESS_emp(tau)", "SDSS_emp(tau)",
                     "MSS_emp(tau)", "EST_naive(tau)", "EST_adj(tau)",
                     "BIAS_naive(tau)", "BIAS_adj(tau)", "RMSE_naive(tau)",
                     "RMSE_adj(tau)", "p_naive(tau)", "p_adj(tau)",
                     "COV_naive(tau)", "COV_adj(tau)")

  ##### Output #################################################################

  sim        <- list(alpha = alpha,
                     des   = des,
                     sim   = sim,
                     tau   = tau)
  class(sim) <- c(class(sim), "OptGS_sim")
  sim

}

#' Simulate group-sequential clinical trials for a normally distributed primary
#' outcome
#'
#' \code{sim()} simulates group-sequential clinical trials in order to validate
#' certain analytically determined operating characteristics (as returned by
#' \code{\link{opchar}}) .
#'
#' @param des An object of class \code{"OptGS_des"}, as returned by
#' \code{\link{des_gs}}, \code{\link{des_nearopt}}, \code{\link{des_opt}}, or
#' \code{\link{build}}. Defaults to \code{des_gs()}.
#' @param tau A \code{\link{numeric}} \code{\link{vector}} indicating the values
#' of \ifelse{html}{\out{<i>&tau;</i>}}{\eqn{\tau}} to perform simulations for.
#' Defaults to \code{des$opchar$tau}.
#' @param replicates A \code{\link{numeric}} indicating the number of replicate
#' simulations to use for each value of
#' \ifelse{html}{\out{<i>&tau;</i>}}{\eqn{\tau}}. Must be a whole number.
#' Defaults to \code{1000}.
#' @param summary A \code{\link{logical}} variable indicating whether a summary
#' of the function's progress should be printed to the console. Defaults to
#' \code{FALSE}.
#' @return A \code{\link{list}} with additional class \code{"OptGS_sim"}. It
#' will contain each of the input variables (subject to internal modification),
#' along with the following element:
#' \itemize{
#' \item \code{sim}: A \code{\link{tibble}} giving the estimated operating
#' characteristics.
#' }
#' @examples
#' # Simulations for the default parameters
#' sim     <- sim()
#' @seealso \code{\link{build}}, \code{\link{des_gs}},
#' \code{\link{des_nearopt}}, \code{\link{des_opt}}, \code{\link{opchar}}
#' @export
sim <- function(des = des_gs(),
                tau = seq(-des$delta, 2*des$delta, length.out = 100),
                estimators = FALSE, replicates = 1000, summary = FALSE) {

  ##### Check input variables ##################################################

  check_OptGS_des(des, "des")
  check_tau(tau)
  check_integer_range(replicates, "replicates", c(0, Inf), 1)
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    #summary_sim(des, tau, replicates)
    message("")
  }

  ##### Perform main computations ##############################################

  len_tau       <- length(tau)
  sim           <- matrix(0, len_tau, 7 + 4*des$J)
  for (i in 1:len_tau) {
    sim[i, ]    <- sim_internal(des, tau[i], replicates, summary)
    if (summary) {
      message("...completed the simulations for tau = ", round(tau[i], 4),
              "...")
    }
  }
  colnames(sim) <- c("tau", "P(tau)", "ESS(tau)", "SDSS(tau)", "MeSS(tau)",
                     "MoSS(tau)", paste(rep(c("E", "F", "S"), each = des$J),
                                        rep(1:des$J, 3), "(tau)", sep = ""),
                     paste("cum{S", 1:des$J, "(tau)}", sep = ""), "max(n)")
  sim           <- tibble::as_tibble(sim)

  ##### Output #################################################################

  output        <- list(des        = des,
                        replicates = replicates,
                        sim        = sim,
                        summary    = summary,
                        tau        = tau)
  class(output) <- c(class(sim), "OptGS_sim")
  output

}

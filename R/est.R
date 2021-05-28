#' Evaluate point estimators of the treatment effect for two-stage
#' group-sequential clinical trial designs with a normally distributed
#' primary outcome
#'
#' \code{est()} computes unadjusted and adjusted point estimates of the
#' treatment effect based on results from a two-stage group-sequential clinical
#' trial. It uses these to determine point estimator (conditional and marginal)
#' bias and residual mean squared error.
#'
#' @param des An object of class OptGS_des, as returned by \code{\link{des_gs}},
#' \code{\link{des_nearopt}}, \code{\link{des_opt}}, or \code{\link{build}}.
#' Defaults to \code{des_gs()}. Currently, this must be a two-stage trial.
#' @param tau A \code{\link{numeric}} \code{\link{vector}} indicating the values
#' of \ifelse{html}{\out{<i>&tau;</i>}}{\eqn{\tau}} to perform calculations for.
#' Defaults to \code{seq(-des$delta, 2*des$delta, length.out = 100)}.
#' @param density A \code{\link{numeric}} whole number indicating the number of
#' standardised test statistics to consider when computing the bias and RMSE.
#' Larger values increase accuracy of integral evaluations but also increase run
#' time. Defaults to \code{1000}.
#' @param summary A \code{\link{logical}} variable indicating whether a summary
#' of the function's progress should be printed to the console. Defaults to
#' \code{FALSE}.
#' @return A \code{\link{list}} with additional class \code{"OptGS_est"}. It
#' will contain each of the input variables (subject to internal modification),
#' along with the following elements:
#' \itemize{
#' \item \code{estimates}: A \code{\link{tibble}} giving details on point
#' estimates.
#' \item \code{estimators}: A \code{\link{tibble}} giving performance of the
#' point estimators.
#' }
#' @examples
#' # Calculations for the default parameters
#' perf <- est()
#' @seealso \code{\link{build}}, \code{\link{des_gs}},
#' \code{\link{des_nearopt}}, \code{\link{des_opt}}, \code{\link{sim}}.
#' @export
est <- function(des = OptGS::des_gs(),
                tau = seq(-des$delta, 2*des$delta, length.out = 100),
                density = 1000, summary = FALSE) {

  ##### Check input variables ##################################################

  check_OptGS_des(des, "des", 2)
  check_tau(tau)
  check_integer_range(density, "density", c(1, Inf), 1)
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    #summary_est(des, tau)
    message("")
  }

  ##### Perform main computations ##############################################

  temp          <- est_int(tau, des$e[1], des$f[1], des$I, des$delta, summary,
                           density)

  ##### Output #################################################################

  output        <- list(des        = des,
                        estimates  = temp$estimates,
                        estimators = temp$estimators,
                        summary    = summary,
                        tau        = tau)
  class(output) <- c(class(output), "OptGS_est")
  output

}

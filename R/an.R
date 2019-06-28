#' Analyse results from a group-sequential clinical trial design for a normally
#' distributed primary outcome
#'
#' \code{an()} analyses data from a group-sequential clinical trial, using both
#' naive and adjusted procedures.
#'
#' @param des An object of class OptGS_des, as returned by \code{\link{des_gs}},
#' \code{\link{des_nearopt}}, \code{\link{des_opt}}, or \code{\link{build}}.
#' Defaults to \code{des_gs()}.
#' @param Zk A \code{\link{numeric}} \code{\link{matrix}} giving results from
#' the group-sequential trial. Must have two columns, the first of which
#' corresponds to values of \ifelse{html}{\out{<i>Z<sub>k</sub></i>}}{\eqn{Z_k}}
#' on trial conclusion and the second the corresponding value of
#' \ifelse{html}{\out{<i>k</i>}}{\eqn{k}}. Defaults to
#' \code{matrix(c(des$e[des$J], des$J), 1, 2)}.
#' @param alpha A \code{\link{numeric}} indicating the value of
#' \ifelse{html}{\out{<i>&alpha;</i>}}{\eqn{\alpha}} to use in confidence
#' interval construction. Defaults to \code{des$alpha}.
#' @param summary A \code{\link{logical}} variable indicating whether a summary
#' of the function's progress should be printed to the console. Defaults to
#' \code{F}.
#' @return A \code{\link{list}} with additional class \code{"OptGS_an"}. It
#' will contain each of the input variables (subject to internal modification),
#' along with the following elements:
#' \itemize{
#' \item \code{an}: A \code{\link{tibble}} giving details on the calculated
#' point estimated, p-values, and confidence intervals.
#' }
#' @examples
#' # Calculations for the default parameters
#' opchar     <- an()
#' @seealso \code{\link{opchar}}, \code{\link{des_gs}}, \code{\link{des_nearopt}},
#' \code{\link{des_opt}}, \code{\link{build}}, \code{\link{sim}},
#' \code{\link{plot.OptGS_des}}, \code{\link{plot.OptGS_opchar}},
#' \code{\link{print.OptGS_des}}, \code{\link{summary.OptGS_des}}.
#' @export
an <- function(des, Zk = matrix(c(des$e[des$J], des$J), 1, 2),
               alpha = des$alpha, summary = F) {

  ##### Check input variables ##################################################

  #check_OptGS_des(des, "des")
  #check_Zk(Zk, des)
  check_real_range_strict(alpha, "alpha", c(0, 1), 1)
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    #summary_an(des, Zk, alpha)
    message("")
  }

  ##### Perform main computations ##############################################

  an <- an_internal(des, Zk, alpha, summary)

  ##### Output #################################################################

  an        <- list(alpha   = alpha,
                    an      = an,
                    des     = des,
                    summary = summary,
                    Zk      = Zk)
  class(an) <- c(class(an), "OptGS_an")
  an

}

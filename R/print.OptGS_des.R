#' Print the key details of a group-sequential clinical trial design for a
#' normally distributed primary outcome
#'
#' \code{print.OptGS_des()} prints the key details from a group-sequential
#' clinical trial design object for a normally distributed primary outcome.
#'
#' @param x An object of class OptGS_des, as returned by \code{\link{des_gs}},
#' \code{\link{des_nearopt}}, \code{\link{des_opt}}, or \code{\link{build}}.
#' Defaults to \code{des_gs()}.
#' @param ... For compatability with the generic.
#' @examples
#' # A two-stage design
#' print(opt_gs())
#' # A three-stage design
#' print(opt_gs(J = 3))
#' @seealso \code{\link{an}}, \code{\link{des_gs}}, \code{\link{des_nearopt}},
#' \code{\link{des_opt}}, \code{\link{build}}, \code{\link{sim}},
#' \code{\link{plot.OptGS_des}}, \code{\link{plot.OptGS_opchar}},
#' \code{\link{opchar}}, \code{\link{summary.OptGS_des}}.
#' @export
print.OptGS_des <- function(x, ...) {

  ##### Check input variables ##################################################

  check_OptGS_des(x, "x")

  ##### Print summary ##########################################################

  message("  ", rep("-", 37 + as.numeric(x$J >= 10) + as.numeric(x$J >= 100)))
  message("  ", x$J, "-stage group-sequential trial design")
  message("  ", rep("-", 37 + as.numeric(x$J >= 10) + as.numeric(x$J >= 100)))
  message("")
  message("  Control arm group-size:      n0 = ", round(x$n0, 2))
  message("  Experimental arm group-size: n1 = ", round(x$n1, 2))
  message("")
  message("  Efficacy boundaries: e = (", paste0(round(x$f, 3),
                                                 collapse = ", "), ")")
  message("  Futility boundaries: f = (", paste0(round(x$f, 3),
                                                 collapse = ", "), ")")
  message("")
  len <- length(strsplit(as.character(x$delta), split = "")[[1]])
  message("  ESS(0)", rep(" ", max(11, len)), "= ",
          round(x$opchar$`ESS(tau)`[1], 2))
  message("  ESS(", x$delta, ")", rep(" ", max(1*(len >= 12), 12 - len)),  "= ",
          round(x$opchar$`ESS(tau)`[3], 2))
  message("  max_tau ESS(tau)", rep(" ", max(len - 10, 1)), "= ",
          round(x$opchar$`ESS(tau)`[2], 2))
  message("  max N", rep(" ", max(12, len + 1)), "= ", x$n[x$J])

}

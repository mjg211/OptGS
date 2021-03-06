#' OptGS: Optimal and near-optimal group-sequential designs for clinical trials
#' with continuous outcomes
#'
#' \strong{OptGS} provides a suite of functions to assist with the design,
#' analysis, and visualization of randomized two-arm group-sequential clinical
#' trials with continuous outcome variables. Specifically, support is provided
#' to perform sample size calculations for popular applicable (non-optimal)
#' designs, along with optimal and near-optimal designs. An additional function
#' allows point estimates to be determined and point estimators to be evaluated.
#' Plotting functions also permit the informative depiction of several important
#' quantities.
#'
#' @section Getting started:
#'
#' You can install the latest development version of \strong{OptGS} from
#' \href{https://github.com/}{Github} with:
#'
#' \code{devtools::install_github("mjg211/OptGS")}
#'
#' An introductory example of how to make use of the package's core
#' functionality can be found \href{https://github.com/mjg211/OptGS}{here}.
#' For further help, please contact James Wason
#' (\email{james.wason@@newcastle.ac.uk}) or Michael Grayling
#' (\email{michael.grayling@@newcastle.ac.uk}) for assistance.
#'
#' @section Details:
#' Currently, functions are provided to support trials in which the primary
#' outcome variable is assumed to be normally distributed.
#' In total, 11 functions are available. Their naming conventions are such that
#' several character strings are joined together separated by underscores. The
#' first character string indicates the type of calculation the function
#' performs (e.g., design determination, operating characteristic calculations),
#' and the remainder which type of design it is:
#'
#' \itemize{
#' \item \code{\link{build}}: Build a group-sequential clinical trial design
#' object for a normally distributed primary outcome.
#' \item \code{\link{des_gs}}: Design a group-sequential clinical trial for a
#' normally distributed primary outcome.
#' \item \code{\link{des_nearopt}}: Design a near-optimal group-sequential
#' clinical trial for a normally distributed primary outcome.
#' \item \code{\link{des_opt}}: Design an optimal group-sequential clinical
#' trial for a normally distributed primary outcome.
#' \item \code{\link{est}}: Evaluate point estimators of the treatment effect in
#' group-sequential clinical trial design data for a normally distributed
#' primary outcome.
#' \item \code{\link{gui}}: Provides a graphical user interface to design
#' determination and point estimator evaluation.
#' \item \code{\link{opchar}}: Calculate the operating characteristics of a
#' group-sequential clinical trial design for a normally distributed primary
#' outcome.
#' \item \code{\link{plot.OptGS_des}}: Plot the stopping boundaries of a
#' group-sequential clinical trial design for a normally distributed primary
#' outcome.
#' \item \code{\link{plot.OptGS_opchar}}: Plot the operating characteristics of
#' a group-sequential clinical trial design for a normally distributed primary
#' outcome.
#' \item \code{\link{print.OptGS_des}}: Print the key details of a
#' group-sequential clinical trial design for a normally distributed primary
#' outcome.
#' \item \code{\link{sim}}: Simulate group-sequential clinical trials for a
#' normally distributed primary outcome.
#' \item \code{\link{summary.OptGS_des}}: Summarise the key details of a
#' group-sequential clinical trial design for a normally distributed primary
#' outcome.
#' }
#'
#' @docType package
#' @name OptGS
NULL

if (getRversion() >= "2.15.1") {
  utils::globalVariables(".data")
}

#' Plot the stopping boundaries of a group-sequential clinical trial design for
#' a normally distributed primary outcome
#'
#' \code{plot.OptGS_des()} plots the stopping boundaries from a group-sequential
#' clinical trial design object for a normally distributed primary outcome.
#' Plots are produced by stage and by sample size.
#'
#' @param x An object of class OptGS_des, as returned by \code{\link{des_gs}},
#' \code{\link{des_nearopt}}, \code{\link{des_opt}}, or \code{\link{build}}.
#' @param ... For compatability with the generic.
#' @param output A \code{\link{logical}} variable indicating whether to return
#' output from the function.
#' @return A \code{\link{list}} containing each of the input variables (subject
#' to internal modification), along with the following elements:
#' \itemize{
#' \item \code{plots}: A \code{\link{list}} containing the stopping boundary
#' plotting objects.
#' }
#' @examples
#' # A two-stage design's boundaries
#' plot(opt_gs())
#' # A three-stage design's boundaries
#' plot(opt_gs(J = 3))
#' @seealso \code{\link{an}}, \code{\link{des_gs}}, \code{\link{des_nearopt}},
#' \code{\link{des_opt}}, \code{\link{build}}, \code{\link{sim}},
#' \code{\link{opchar}}, \code{\link{plot.OptGS_opchar}},
#' \code{\link{print.OptGS_des}}, \code{\link{summary.OptGS_des}}.
#' @export
plot.OptGS_des <- function(x, ..., output = F) {

  des <- x

  ##### Input Checking #########################################################

  check_OptGS_des(x, "x")
  check_logical(output, "output")

  ##### Main Computations ######################################################

  plots       <- list()
  boundaries  <- tibble::tibble(Stage               = rep(1:des$J, 2),
                                `Sample size`       = rep(des$n, 2),
                                Type                = rep(c("Efficacy",
                                                            "Futility"),
                                                          each = des$J),
                                `Stopping boundary` = c(des$e, des$f))
  plots$J     <- ggplot2::ggplot(boundaries,
                                 ggplot2::aes(x = Stage,
                                              y = `Stopping boundary`,
                                              colour = Type,
                                              by = Type)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::scale_color_manual(values = c("forestgreen",
                                           "firebrick2")) +
    ggplot2::scale_x_continuous(breaks = 1:des$J) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom",
                   legend.title = ggplot2::element_blank())
  print(plots$J)

  plots$n     <- ggplot2::ggplot(boundaries,
                                 ggplot2::aes(x = `Sample size`,
                                              y = `Stopping boundary`,
                                              colour = Type,
                                              by = Type)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::scale_color_manual(values = c("forestgreen",
                                           "firebrick2")) +
    ggplot2::scale_x_continuous(breaks = des$n) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom",
                   legend.title = ggplot2::element_blank())


  ##### Outputting #############################################################

  if (output) {
    return(list(output = output,
                plots  = plots,
                x      = x))
  }

}

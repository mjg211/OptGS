#' Plot the operating characteristics of a group-sequential clinical trial
#' design for a normally distributed primary outcome
#'
#' \code{plot.OptGS_opchar()} plots values from a group-sequential clinical
#' trial operating characteristic object for a normally distributed primary
#' outcome.
#'
#' @param x An object of class OptGS_opchar, as returned by
#' \code{\link{opchar}}.
#' @param ... For compatability with the generic.
#' @param output A \code{\link{logical}} variable indicating whether to return
#' output from the function.
#' @return A \code{\link{list}} containing each of the input variables (subject
#' to internal modification), along with the following elements:
#' \itemize{
#' \item \code{plots}: A \code{\link{list}} containing the operating
#' characteristic plotting objects.
#' }
#' @examples
#' # A two-stage design's operating characteristics
#' plot(opchar(opt_gs(), tau = seq(-des$delta, 2*des$delta, length.out = 100)))
#' # A three-stage design's operating characteristics
#' plot(opchar(opt_gs(J = 3),
#'      tau = seq(-des$delta, 2*des$delta, length.out = 100)))
#' @seealso \code{\link{an}}, \code{\link{des_gs}}, \code{\link{des_nearopt}},
#' \code{\link{des_opt}}, \code{\link{build}}, \code{\link{sim}},
#' \code{\link{opchar}}, \code{\link{plot.OptGS_des}},
#' \code{\link{print.OptGS_des}}, \code{\link{summary.OptGS_des}}.
#' @export
plot.OptGS_opchar <- function(x, ..., output = F) {

  opchar <- x

  ##### Input Checking #########################################################

  check_OptGS_opchar(x, "x")
  check_logical(output, "output")

  ##### Main Computations ######################################################

  plots               <- list()
  if (min(opchar$tau) < 0) {
    red               <- tibble::tibble(start = min(opchar$tau),
                                        end   = min(0, max(opchar$tau)))
  }
  if (all(min(opchar$tau) <= opchar$des$delta, max(opchar$tau) >= 0)) {
    amber             <- tibble::tibble(start = max(0, min(opchar$tau)),
                                        end   = min(opchar$des$delta,
                                                    max(opchar$tau)))
  }
  if (max(opchar$tau) > opchar$des$delta) {
    green             <- tibble::tibble(start = max(opchar$des$delta,
                                                    min(opchar$tau)),
                                        end   = max(opchar$tau))
  }
  if (length(unique(opchar$tau)) > 1) {
    plots$`ESS(tau)`  <- ggplot2::ggplot()
    if (min(opchar$tau) < 0) {
      plots$`ESS(tau)` <- plots$`ESS(tau)` +
        ggplot2::geom_rect(data  = red,
                           ggplot2::aes(xmin = start,
                                        xmax = end,
                                        ymin = -Inf,
                                        ymax = Inf),
                           alpha = 0.1,
                           fill  = "firebrick2")
    }
    if (all(min(opchar$tau) <= opchar$des$delta, max(opchar$tau) >= 0)) {
      plots$`ESS(tau)` <- plots$`ESS(tau)` +
        ggplot2::geom_rect(data  = amber,
                           ggplot2::aes(xmin = start,
                                        xmax = end,
                                        ymin = -Inf,
                                        ymax = Inf),
                           alpha = 0.1,
                           fill  = "orange")
    }
    if (max(opchar$tau) > opchar$des$delta) {
      plots$`ESS(tau)` <- plots$`ESS(tau)` +
        ggplot2::geom_rect(data  = green,
                           ggplot2::aes(xmin = start,
                                        xmax = end,
                                        ymin = -Inf,
                                        ymax = Inf),
                           alpha = 0.1,
                           fill  = "green4")
    }
    plots$`ESS(tau)` <- plots$`ESS(tau)` +
      ggplot2::geom_line(data = opchar$opchar,
                         ggplot2::aes(x = tau,
                                      y = `ESS(tau)`)) +
      ggplot2::geom_hline(yintercept = opchar$des$n_fixed, linetype = 2) +
      ggplot2::xlab(expression(tau)) +
      ggplot2::ylab(expression(paste(italic(ESS), "(",
                                     tau, ")", sep = ""))) +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(expand = c(0, 0),
                                  limits = c(min(opchar$tau),
                                             max(opchar$tau)))
    print(plots$`ESS(tau)`)
    plots$`SDSS(tau)` <- ggplot2::ggplot()
    if (min(opchar$tau) < 0) {
      plots$`SDSS(tau)` <- plots$`SDSS(tau)` +
        ggplot2::geom_rect(data = red,
                           ggplot2::aes(xmin = start,
                                        xmax = end,
                                        ymin = -Inf,
                                        ymax = Inf),
                           alpha = 0.1,
                           fill = "firebrick2")
    }
    if (all(min(opchar$tau) <= opchar$des$delta, max(opchar$tau) >= 0)) {
      plots$`SDSS(tau)` <- plots$`SDSS(tau)` +
        ggplot2::geom_rect(data = amber,
                           ggplot2::aes(xmin = start,
                                        xmax = end,
                                        ymin = -Inf,
                                        ymax = Inf),
                           alpha = 0.1,
                           fill = "orange")
    }
    if (max(opchar$tau) > opchar$des$delta) {
      plots$`SDSS(tau)` <- plots$`SDSS(tau)` +
        ggplot2::geom_rect(data = green,
                           ggplot2::aes(xmin = start,
                                        xmax = end,
                                        ymin = -Inf,
                                        ymax = Inf),
                           alpha = 0.1,
                           fill = "green4")
    }
    plots$`SDSS(tau)` <- plots$`SDSS(tau)` +
      ggplot2::geom_line(data = opchar$opchar,
                         ggplot2::aes(x = tau,
                                      y = `SDSS(tau)`)) +
      ggplot2::xlab(expression(tau)) +
      ggplot2::ylab(expression(paste(italic(SDSS), "(",
                                     tau, ")",
                                     sep = ""))) +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(expand = c(0, 0),
                                  limits = c(min(opchar$tau),
                                             max(opchar$tau)))

    plots$`MeSS(tau)`   <- ggplot2::ggplot()
    if (min(opchar$tau) < 0) {
      plots$`MeSS(tau)` <- plots$`MeSS(tau)` +
        ggplot2::geom_rect(data  = red,
                           ggplot2::aes(xmin = start,
                                        xmax = end,
                                        ymin = -Inf,
                                        ymax = Inf),
                           alpha = 0.1,
                           fill  = "firebrick2")
    }
    if (all(min(opchar$tau) <= opchar$des$delta, max(opchar$tau) >= 0)) {
      plots$`MeSS(tau)` <- plots$`MeSS(tau)` +
        ggplot2::geom_rect(data  = amber,
                           ggplot2::aes(xmin = start,
                                        xmax = end,
                                        ymin = -Inf,
                                        ymax = Inf),
                           alpha = 0.1,
                           fill  = "orange")
    }
    if (max(opchar$tau) > opchar$des$delta) {
      plots$`MeSS(tau)` <- plots$`MeSS(tau)` +
        ggplot2::geom_rect(data  = green,
                           ggplot2::aes(xmin = start,
                                        xmax = end,
                                        ymin = -Inf,
                                        ymax = Inf),
                           alpha = 0.1,
                           fill  = "green4")
    }
    plots$`MeSS(tau)` <- plots$`MeSS(tau)` +
      ggplot2::geom_line(data = opchar$opchar,
                         ggplot2::aes(x = tau,
                                      y = `MeSS(tau)`)) +
      ggplot2::geom_hline(yintercept = opchar$des$n_fixed, linetype = 2) +
      ggplot2::xlab(expression(tau)) +
      ggplot2::ylab(expression(paste(italic(MeSS), "(",
                                     tau, ")",
                                     sep = ""))) +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(expand = c(0, 0),
                                  limits = c(min(opchar$tau),
                                             max(opchar$tau)))
    plots$`MoSS(tau)`   <- ggplot2::ggplot()
    if (min(opchar$tau) < 0) {
      plots$`MoSS(tau)` <- plots$`MoSS(tau)` +
        ggplot2::geom_rect(data  = red,
                           ggplot2::aes(xmin = start,
                                        xmax = end,
                                        ymin = -Inf,
                                        ymax = Inf),
                           alpha = 0.1,
                           fill  = "firebrick2")
    }
    if (all(min(opchar$tau) <= opchar$des$delta, max(opchar$tau) >= 0)) {
      plots$`MoSS(tau)` <- plots$`MoSS(tau)` +
        ggplot2::geom_rect(data  = amber,
                           ggplot2::aes(xmin = start,
                                        xmax = end,
                                        ymin = -Inf,
                                        ymax = Inf),
                           alpha = 0.1,
                           fill  = "orange")
    }
    if (max(opchar$tau) > opchar$des$delta) {
      plots$`MoSS(tau)` <- plots$`MoSS(tau)` +
        ggplot2::geom_rect(data  = green,
                           ggplot2::aes(xmin = start,
                                        xmax = end,
                                        ymin = -Inf,
                                        ymax = Inf),
                           alpha = 0.1,
                           fill  = "green4")
    }
    plots$`MoSS(tau)` <- plots$`MoSS(tau)` +
      ggplot2::geom_line(data = opchar$opchar,
                         ggplot2::aes(x = tau,
                                      y = `MoSS(tau)`)) +
      ggplot2::geom_hline(yintercept = opchar$des$n_fixed, linetype = 2) +
      ggplot2::xlab(expression(tau)) +
      ggplot2::ylab(expression(paste(italic(MoSS), "(",
                                     tau, ")",
                                     sep = ""))) +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(expand = c(0, 0),
                                  limits = c(min(opchar$tau),
                                             max(opchar$tau)))
    plots$`P(tau)` <- ggplot2::ggplot()
    if (min(opchar$tau) < 0) {
      plots$`P(tau)` <- plots$`P(tau)` +
        ggplot2::geom_rect(data = red,
                           ggplot2::aes(xmin = start,
                                        xmax = end,
                                        ymin = -Inf,
                                        ymax = Inf),
                           alpha = 0.1,
                           fill = "firebrick2")
    }
    if (all(min(opchar$tau) <= opchar$des$delta, max(opchar$tau) >= 0)) {
      plots$`P(tau)` <- plots$`P(tau)` +
        ggplot2::geom_rect(data = amber,
                           ggplot2::aes(xmin = start,
                                        xmax = end,
                                        ymin = -Inf,
                                        ymax = Inf),
                           alpha = 0.1,
                           fill = "orange")
    }
    if (max(opchar$tau) > opchar$des$delta) {
      plots$`P(tau)` <- plots$`P(tau)` +
        ggplot2::geom_rect(data = green,
                           ggplot2::aes(xmin = start,
                                        xmax = end,
                                        ymin = -Inf,
                                        ymax = Inf),
                           alpha = 0.1,
                           fill = "green4")
    }
    plots$`P(tau)` <- plots$`P(tau)` +
      ggplot2::geom_hline(yintercept =
                            c(opchar$des$alpha,
                              1 - opchar$des$beta),
                          linetype = 2) +
      ggplot2::geom_line(data = opchar$opchar,
                         ggplot2::aes(x = tau,
                                      y = `P(tau)`)) +
      ggplot2::xlab(expression(tau)) +
      ggplot2::ylab(expression(paste(italic(P), "(", tau,
                                     ")", sep = ""))) +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(expand = c(0, 0),
                                  limits = c(min(opchar$tau),
                                             max(opchar$tau)))
    print(plots$`P(tau)`)
  } else {
    plots$`ESS(tau)` <- plots$`SDSS(tau)` <- plots$`MeSS(tau)` <-
                        plots$`MoSS(tau)` <- plots$`P(tau)`    <- NULL
  }
  int_tibble            <- tidyr::gather(opchar$opchar, "key", "value",
                                         7:(6 + 2*opchar$des$J))
  int_tibble            <-
    dplyr::mutate(int_tibble,
                  key2 = rep(paste0(rep(c(paste0(expression(italic(E)), "["),
                                          paste0(expression(italic(F)), "[")),
                                        each = opchar$des$J),
                                    rep(1:opchar$des$J, 2), "]"),
                             each = nrow(opchar$opchar)))
  plots$rejection <- ggplot2::ggplot(int_tibble,
                                     ggplot2::aes(tau, value, fill = key2)) +
    ggplot2::geom_area() +
    ggplot2::xlab(expression(tau)) +
    ggplot2::ylab("Stopping probability") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom",
                   legend.title    = ggplot2::element_blank()) +
    ggplot2::scale_fill_manual(values = ggthemes::ptol_pal()(2*opchar$des$J),
                              labels = scales::parse_format()) +
    ggplot2::scale_x_continuous(expand = c(0, 0),
                                limits = c(min(opchar$tau),
                                           max(opchar$tau))) +
    ggplot2::scale_y_continuous(expand = c(0, 0))

  int_tibble            <- tidyr::gather(opchar$opchar, "key", "value",
                                         (7 + 2*opchar$des$J):
                                           (6 + 3*opchar$des$J))
  int_tibble            <-
    dplyr::mutate(int_tibble,
                  key2 = rep(paste0(rep(paste0(expression(italic(S)), "["),
                                        opchar$des$J),
                                    1:opchar$des$J, "]"),
                             each = nrow(opchar$opchar)))
  plots$stopping  <- ggplot2::ggplot(int_tibble,
                                           ggplot2::aes(tau, value,
                                                        fill = key2)) +
    ggplot2::geom_area() +
    ggplot2::xlab(expression(tau)) +
    ggplot2::ylab("Stopping probability") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom",
                   legend.title    = ggplot2::element_blank()) +
    ggplot2::scale_fill_manual(values = ggthemes::ptol_pal()(2*opchar$des$J),
                               labels = scales::parse_format()) +
    ggplot2::scale_x_continuous(expand = c(0, 0),
                                limits = c(min(opchar$tau),
                                           max(opchar$tau))) +
    ggplot2::scale_y_continuous(expand = c(0, 0))


  ##### Outputting #############################################################

  if (output) {
    output <- list(opchar = opchar,
                   output = output,
                   plots  = plots)
    return(output)
  }

}


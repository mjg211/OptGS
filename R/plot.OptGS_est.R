#' Plot point estimator performance for two-stage group-sequential clinical
#' trial designs with a normally distributed primary outcome
#'
#' \code{plot.OptGS_est()} plots (conditional and marginal) point estimator bias
#' and residual mean squared error for a specified two-stage group-sequential
#' clinical trial design.
#'
#' @param x An object of class OptGS_est, as returned by \code{\link{est}}.
#' @param ... For compatability with the generic.
#' @param output A \code{\link{logical}} variable indicating whether to return
#' output from the function.
#' @return A \code{\link{list}} containing each of the input variables (subject
#' to internal modification), along with the following elements:
#' \itemize{
#' \item \code{plots}: A \code{\link{list}} containing the available plots.
#' }
#' @examples
#' # For the default two-stage design
#' plot(est(des = opt_gs(), tau = seq(-des$delta, 2*des$delta,
#'          length.out = 100)))
#' @seealso \code{\link{est}}
#' @export
plot.OptGS_est <- function(x, ..., output = FALSE) {

  ##### Input Checking #########################################################

  check_OptGS_est(x, "x")
  check_logical(output, "output")

  ##### Main Computations ######################################################

  plots                            <- list()
  if (min(x$tau) < 0) {
    red                            <-
      tibble::tibble(start = min(x$tau), end = min(0, max(x$tau)))
  }
  if (all(min(x$tau) <= x$des$delta, max(x$tau) >= 0)) {
    amber                          <-
      tibble::tibble(start = max(0, min(x$tau)),
                     end   = min(x$des$delta, max(x$tau)))
  }
  if (max(x$tau) > x$des$delta) {
    green                          <-
      tibble::tibble(start = max(x$des$delta, min(x$tau)), end = max(x$tau))
  }
  if (length(unique(x$tau)) > 1) {
    plots$`Bias(hat(tau)|tau,1)`   <- ggplot2::ggplot() + theme_OptGS()
    if (min(x$tau) < 0) {
      plots$`Bias(hat(tau)|tau,1)` <- plots$`Bias(hat(tau)|tau,1)` +
        ggplot2::geom_rect(data  = red,
                           ggplot2::aes(xmin = start,
                                        xmax = end,
                                        ymin = -Inf,
                                        ymax = Inf),
                           alpha = 0.1,
                           fill  = "firebrick2")
    }
    if (all(min(x$tau) <= x$des$delta, max(x$tau) >= 0)) {
      plots$`Bias(hat(tau)|tau,1)` <- plots$`Bias(hat(tau)|tau,1)` +
        ggplot2::geom_rect(data  = amber,
                           ggplot2::aes(xmin = start,
                                        xmax = end,
                                        ymin = -Inf,
                                        ymax = Inf),
                           alpha = 0.1,
                           fill  = "orange")
    }
    if (max(x$tau) > x$des$delta) {
      plots$`Bias(hat(tau)|tau,1)` <- plots$`Bias(hat(tau)|tau,1)` +
        ggplot2::geom_rect(data  = green,
                           ggplot2::aes(xmin = start,
                                        xmax = end,
                                        ymin = -Inf,
                                        ymax = Inf),
                           alpha = 0.1,
                           fill  = "green4")
    }
    plots$`Bias(hat(tau)|tau,1)`   <- plots$`Bias(hat(tau)|tau,1)` +
      ggplot2::geom_line(data = x$estimators,
                         ggplot2::aes(x      = tau,
                                      y      = `Bias(hat(tau)|tau,1)`,
                                      colour = estimator)) +
      ggplot2::xlab(expression(tau)) +
      ggplot2::ylab(expression(paste(italic(Bias), "(", hat(tau), "|", tau,
                                     ",1)"))) +
      ggplot2::scale_color_manual(values = c("#000000", "#999999", "#E69F00",
                                             "#56B4E9", "#009E73", "#F0E442",
                                             "#0072B2", "#D55E00", "#CC79A7")) +
      ggplot2::scale_x_continuous(expand = c(0, 0),
                                  limits = c(min(x$tau),
                                             max(x$tau)))
    plots$`Bias(hat(tau)|tau,2)`   <- ggplot2::ggplot() + theme_OptGS()
    if (min(x$tau) < 0) {
      plots$`Bias(hat(tau)|tau,2)` <- plots$`Bias(hat(tau)|tau,2)` +
        ggplot2::geom_rect(data  = red,
                           ggplot2::aes(xmin = start,
                                        xmax = end,
                                        ymin = -Inf,
                                        ymax = Inf),
                           alpha = 0.1,
                           fill  = "firebrick2")
    }
    if (all(min(x$tau) <= x$des$delta, max(x$tau) >= 0)) {
      plots$`Bias(hat(tau)|tau,2)` <- plots$`Bias(hat(tau)|tau,2)` +
        ggplot2::geom_rect(data  = amber,
                           ggplot2::aes(xmin = start,
                                        xmax = end,
                                        ymin = -Inf,
                                        ymax = Inf),
                           alpha = 0.1,
                           fill  = "orange")
    }
    if (max(x$tau) > x$des$delta) {
      plots$`Bias(hat(tau)|tau,2)` <- plots$`Bias(hat(tau)|tau,2)` +
        ggplot2::geom_rect(data  = green,
                           ggplot2::aes(xmin = start,
                                        xmax = end,
                                        ymin = -Inf,
                                        ymax = Inf),
                           alpha = 0.1,
                           fill  = "green4")
    }
    plots$`Bias(hat(tau)|tau,2)`   <- plots$`Bias(hat(tau)|tau,2)` +
      ggplot2::geom_line(data = x$estimators,
                         ggplot2::aes(x      = tau,
                                      y      = `Bias(hat(tau)|tau,2)`,
                                      colour = estimator)) +
      ggplot2::xlab(expression(tau)) +
      ggplot2::ylab(expression(paste(italic(Bias), "(", hat(tau), "|", tau,
                                     ",2)"))) +
      ggplot2::scale_color_manual(values = c("#000000", "#999999", "#E69F00",
                                             "#56B4E9", "#009E73", "#F0E442",
                                             "#0072B2", "#D55E00", "#CC79A7")) +
      ggplot2::scale_x_continuous(expand = c(0, 0),
                                  limits = c(min(x$tau),
                                             max(x$tau)))
    plots$`Bias(hat(tau)|tau)`     <- ggplot2::ggplot() + theme_OptGS()
    if (min(x$tau) < 0) {
      plots$`Bias(hat(tau)|tau)`   <- plots$`Bias(hat(tau)|tau)` +
        ggplot2::geom_rect(data  = red,
                           ggplot2::aes(xmin = start,
                                        xmax = end,
                                        ymin = -Inf,
                                        ymax = Inf),
                           alpha = 0.1,
                           fill  = "firebrick2")
    }
    if (all(min(x$tau) <= x$des$delta, max(x$tau) >= 0)) {
      plots$`Bias(hat(tau)|tau)`   <- plots$`Bias(hat(tau)|tau)` +
        ggplot2::geom_rect(data  = amber,
                           ggplot2::aes(xmin = start,
                                        xmax = end,
                                        ymin = -Inf,
                                        ymax = Inf),
                           alpha = 0.1,
                           fill  = "orange")
    }
    if (max(x$tau) > x$des$delta) {
      plots$`Bias(hat(tau)|tau)`   <- plots$`Bias(hat(tau)|tau)` +
        ggplot2::geom_rect(data  = green,
                           ggplot2::aes(xmin = start,
                                        xmax = end,
                                        ymin = -Inf,
                                        ymax = Inf),
                           alpha = 0.1,
                           fill  = "green4")
    }
    plots$`Bias(hat(tau)|tau)`     <- plots$`Bias(hat(tau)|tau)` +
      ggplot2::geom_line(data = x$estimators,
                         ggplot2::aes(x      = tau,
                                      y      = `Bias(hat(tau)|tau)`,
                                      colour = estimator)) +
      ggplot2::xlab(expression(tau)) +
      ggplot2::ylab(expression(paste(italic(Bias), "(", hat(tau), "|", tau,
                                     ")"))) +
      ggplot2::scale_color_manual(values = c("#000000", "#999999", "#E69F00",
                                             "#56B4E9", "#009E73", "#F0E442",
                                             "#0072B2", "#D55E00", "#CC79A7")) +
      ggplot2::scale_x_continuous(expand = c(0, 0),
                                  limits = c(min(x$tau),
                                             max(x$tau)))
    plots$`RMSE(hat(tau)|tau,1)`   <- ggplot2::ggplot() + theme_OptGS()
    if (min(x$tau) < 0) {
      plots$`RMSE(hat(tau)|tau,1)` <- plots$`RMSE(hat(tau)|tau,1)` +
        ggplot2::geom_rect(data  = red,
                           ggplot2::aes(xmin = start,
                                        xmax = end,
                                        ymin = -Inf,
                                        ymax = Inf),
                           alpha = 0.1,
                           fill  = "firebrick2")
    }
    if (all(min(x$tau) <= x$des$delta, max(x$tau) >= 0)) {
      plots$`RMSE(hat(tau)|tau,1)` <- plots$`RMSE(hat(tau)|tau,1)` +
        ggplot2::geom_rect(data  = amber,
                           ggplot2::aes(xmin = start,
                                        xmax = end,
                                        ymin = -Inf,
                                        ymax = Inf),
                           alpha = 0.1,
                           fill  = "orange")
    }
    if (max(x$tau) > x$des$delta) {
      plots$`RMSE(hat(tau)|tau,1)` <- plots$`RMSE(hat(tau)|tau,1)` +
        ggplot2::geom_rect(data  = green,
                           ggplot2::aes(xmin = start,
                                        xmax = end,
                                        ymin = -Inf,
                                        ymax = Inf),
                           alpha = 0.1,
                           fill  = "green4")
    }
    plots$`RMSE(hat(tau)|tau,1)`   <- plots$`RMSE(hat(tau)|tau,1)` +
      ggplot2::geom_line(data = x$estimators,
                         ggplot2::aes(x      = tau,
                                      y      = `RMSE(hat(tau)|tau,1)`,
                                      colour = estimator)) +
      ggplot2::xlab(expression(tau)) +
      ggplot2::ylab(expression(paste(italic(RMSE), "(", hat(tau), "|", tau,
                                     ",1)"))) +
      ggplot2::scale_color_manual(values = c("#000000", "#999999", "#E69F00",
                                             "#56B4E9", "#009E73", "#F0E442",
                                             "#0072B2", "#D55E00", "#CC79A7")) +
      ggplot2::scale_x_continuous(expand = c(0, 0),
                                  limits = c(min(x$tau),
                                             max(x$tau)))
    plots$`RMSE(hat(tau)|tau,2)`   <- ggplot2::ggplot() + theme_OptGS()
    if (min(x$tau) < 0) {
      plots$`RMSE(hat(tau)|tau,2)` <- plots$`RMSE(hat(tau)|tau,2)` +
        ggplot2::geom_rect(data  = red,
                           ggplot2::aes(xmin = start,
                                        xmax = end,
                                        ymin = -Inf,
                                        ymax = Inf),
                           alpha = 0.1,
                           fill  = "firebrick2")
    }
    if (all(min(x$tau) <= x$des$delta, max(x$tau) >= 0)) {
      plots$`RMSE(hat(tau)|tau,2)` <- plots$`RMSE(hat(tau)|tau,2)` +
        ggplot2::geom_rect(data  = amber,
                           ggplot2::aes(xmin = start,
                                        xmax = end,
                                        ymin = -Inf,
                                        ymax = Inf),
                           alpha = 0.1,
                           fill  = "orange")
    }
    if (max(x$tau) > x$des$delta) {
      plots$`RMSE(hat(tau)|tau,2)` <- plots$`RMSE(hat(tau)|tau,2)` +
        ggplot2::geom_rect(data  = green,
                           ggplot2::aes(xmin = start,
                                        xmax = end,
                                        ymin = -Inf,
                                        ymax = Inf),
                           alpha = 0.1,
                           fill  = "green4")
    }
    plots$`RMSE(hat(tau)|tau,2)`   <- plots$`RMSE(hat(tau)|tau,2)` +
      ggplot2::geom_line(data = x$estimators,
                         ggplot2::aes(x      = tau,
                                      y      = `RMSE(hat(tau)|tau,2)`,
                                      colour = estimator)) +
      ggplot2::xlab(expression(tau)) +
      ggplot2::ylab(expression(paste(italic(RMSE), "(", hat(tau), "|", tau,
                                     ",2)"))) +
      ggplot2::scale_color_manual(values = c("#000000", "#999999", "#E69F00",
                                             "#56B4E9", "#009E73", "#F0E442",
                                             "#0072B2", "#D55E00", "#CC79A7")) +
      ggplot2::scale_x_continuous(expand = c(0, 0),
                                  limits = c(min(x$tau),
                                             max(x$tau)))
    plots$`RMSE(hat(tau)|tau)`     <- ggplot2::ggplot() + theme_OptGS()
    if (min(x$tau) < 0) {
      plots$`RMSE(hat(tau)|tau)`   <- plots$`RMSE(hat(tau)|tau)` +
        ggplot2::geom_rect(data  = red,
                           ggplot2::aes(xmin = start,
                                        xmax = end,
                                        ymin = -Inf,
                                        ymax = Inf),
                           alpha = 0.1,
                           fill  = "firebrick2")
    }
    if (all(min(x$tau) <= x$des$delta, max(x$tau) >= 0)) {
      plots$`RMSE(hat(tau)|tau)`   <- plots$`RMSE(hat(tau)|tau)` +
        ggplot2::geom_rect(data  = amber,
                           ggplot2::aes(xmin = start,
                                        xmax = end,
                                        ymin = -Inf,
                                        ymax = Inf),
                           alpha = 0.1,
                           fill  = "orange")
    }
    if (max(x$tau) > x$des$delta) {
      plots$`RMSE(hat(tau)|tau)`   <- plots$`RMSE(hat(tau)|tau)` +
        ggplot2::geom_rect(data  = green,
                           ggplot2::aes(xmin = start,
                                        xmax = end,
                                        ymin = -Inf,
                                        ymax = Inf),
                           alpha = 0.1,
                           fill  = "green4")
    }
    plots$`RMSE(hat(tau)|tau)`     <- plots$`RMSE(hat(tau)|tau)` +
      ggplot2::geom_line(data = x$estimators,
                         ggplot2::aes(x      = tau,
                                      y      = `RMSE(hat(tau)|tau)`,
                                      colour = estimator)) +
      ggplot2::xlab(expression(tau)) +
      ggplot2::ylab(expression(paste(italic(RMSE), "(", hat(tau), "|", tau,
                                     ")"))) +
      ggplot2::scale_color_manual(values = c("#000000", "#999999", "#E69F00",
                                             "#56B4E9", "#009E73", "#F0E442",
                                             "#0072B2", "#D55E00", "#CC79A7")) +
      ggplot2::scale_x_continuous(expand = c(0, 0),
                                  limits = c(min(x$tau),
                                             max(x$tau)))
    print(plots$`RMSE(hat(tau)|tau)`)
    print(plots$`Bias(hat(tau)|tau)`)
  } else {
    plots$`Bias(hat(tau)|tau,1)`   <- plots$`Bias(hat(tau)|tau,2)` <-
      plots$`Bias(hat(tau)|tau)`   <- plots$`RMSE(hat(tau)|tau,1)` <-
      plots$`RMSE(hat(tau)|tau,2)` <- plots$`RMSE(hat(tau)|tau)`   <- NULL
  }

  ##### Outputting #############################################################

  if (output) {
    return(list(output = output,
                plots  = plots,
                x      = x))
  }

}

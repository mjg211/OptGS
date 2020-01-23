#' Graphical user interface to optimal group sequential trial design
#' determination
#'
#' \code{gui()} run an R Shiny web browser based graphical user interface for
#' running the functions \code{\link{des_gs}}, \code{\link{des_nearopt}},
#' \code{\link{opchar}}, and \code{\link{plot.des_OptGS}}.
#'
#' @examples
#' # Launch the GUI
#' \dontrun{
#' gui()
#' }
#' @seealso \code{\link{des_gs}}, \code{\link{des_nearopt}},
#' \code{\link{opchar}}, \code{\link{plot.des_OptGS}}.
#' @export
gui <- function() {
  app_dir <- system.file("shiny", "OptGS", package = "OptGS")
  if (app_dir == "") {
    stop("Could not find required directory for Shiny graphical user ",
         "interface. Try re-installing multiarm.", call. = F)
  }
  shiny::runApp(app_dir, display.mode = "normal", launch.browser = T)
}

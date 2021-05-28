#' Graphical user interface to group sequential trial design and analysis
#'
#' \code{gui()} run an R Shiny web browser based graphical user interface for
#' running the functions \code{\link{des_fixed}}, \code{\link{des_gs}},
#' \code{\link{des_nearopt}}, \code{\link{est}}, \code{\link{opchar}},
#' \code{\link{plot.OptGS_des}}, and \code{\link{plot.OptGS_opchar}}.
#'
#' @examples
#' # Launch the GUI
#' \dontrun{
#' gui()
#' }
#' @seealso \code{\link{des_fixed}}, \code{\link{des_gs}},
#' \code{\link{des_nearopt}}, \code{\link{est}}, \code{\link{opchar}},
#' \code{\link{plot.OptGS_des}}, \code{\link{plot.OptGS_opchar}}
#' @export
gui <- function() {
  app_dir <- system.file("shiny", "OptGS", package = "OptGS")
  if (app_dir == "") {
    stop("Could not find required directory for Shiny graphical user ",
         "interface. Try re-installing OptGS.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal", launch.browser = TRUE)
}

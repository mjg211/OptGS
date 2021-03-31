.onAttach <- function(libname, pkgname) {
  packageStartupMessage("  ", rep("-", 76), "\n  OptGS: Optimal and near-",
                        "optimal group-sequential designs for clinical trials",
                        " \n         with continuous outcomes\n  ",
                        rep("-", 76), "\n\n  v.2.1: For an overview of the ",
                        "package's functionality enter: ?OptGS\n\n  For news ",
                        "on the latest updates enter: news(package =",
                        " \"OptGS\")")
}

#' Design a fixed-sample (single-stage) clinical trial for a normally
#' distributed primary outcome
#'
#' \code{des_fixed()} determines fixed-sample (i.e., single-stage) clinical
#' trial designs assuming the primary outcome variable is normally distributed.
#'
#' @param alpha A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&alpha;</i>}}{\eqn{\alpha}}, the desired type-I
#' error-rate. Must be strictly between 0 and 1. Defaults to \code{0.05}.
#' @param beta A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&beta;</i>}}{\eqn{\beta}}, the desired type-II
#' error-rate. Must be strictly between 0 and 1. Defaults to \code{0.2}.
#' @param delta A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&delta;</i>}}{\eqn{\delta}}, the treatment effect to
#' power the trial for. Must be strictly positive. Defaults to \code{0.2}.
#' @param sigma0 A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&sigma;</i><sub>0</sub>}}{\eqn{\sigma_0}}, the
#' standard deviation of the responses in the control arm. Must be strictly
#' positive. Defaults to \code{1}.
#' @param sigma1 A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&sigma;</i><sub>1</sub>}}{\eqn{\sigma_1}}, the
#' standard deviation of the responses in the experimental arm. Must be strictly
#' positive. Defaults to \code{sigma0}.
#' @param ratio A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>r</i>}}{\eqn{r}}, the allocation ratio to the
#' experimental arm relative to the control arm. Must be strictly positive.
#' Defaults to \code{1}.
#' @param quantile_sub A \code{\link{logical}} variable indicating whether
#' quantile substitution should be applied to the identified rejection boundary.
#' Defaults to \code{FALSE}.
#' @param integer_n A \code{\link{logical}} variable indicating whether the
#' computed values for \ifelse{html}{\out{<i>n</i><sub>0</sub>}}{\eqn{n_0}} and
#' \ifelse{html}{\out{<i>n</i><sub>1</sub>}}{\eqn{n_1}}, the group sizes in the
#' control and experimental arms, should be forced to be whole numbers. Defaults
#' to \code{TRUE}.
#' @param summary A \code{\link{logical}} variable indicating whether a summary
#' of the function's progress should be printed to the console. Defaults to
#' \code{FALSE}.
#' @return A \code{\link{list}} with additional class \code{"OptGS_des"}. It
#' will contain each of the input variables (subject to internal modification),
#' relating them to the outputs of the various group-sequential design functions
#' in \code{\link{OptGS}}, along with additional elements including:
#' \itemize{
#' \item \item \code{n}: A \code{\link{numeric}} giving
#' \ifelse{html}{\out{<i>n</i>}}{\eqn{n}}, the sample size for the identified
#' design.
#' \item \code{n0}: A \code{\link{numeric}} giving
#' \ifelse{html}{\out{<i>n</i><sub>0</sub>}}{\eqn{n_0}}, the sample size in the
#' control arm for the identified design.
#' \item \code{n1}: A \code{\link{numeric}} giving
#' \ifelse{html}{\out{<i>n</i><sub>1</sub>}}{\eqn{n_1}}, the sample size in the
#' experimental arm for the identified design.
#' \item \code{name}: A \code{\link{character}} string giving a name for the
#' identified design.
#' \item \code{opchar}: A \code{\link[tibble]{tibble}} giving the operating
#' characteristics of the identified design when
#' \ifelse{html}{\out{<i>&tau;</i> = 0}}{\eqn{\tau = 0}} and
#' \ifelse{html}{\out{<i>&tau;</i> = <i>&delta;</i>}}{\eqn{\tau = \delta}}.
#' }
#' @examples
#' # The fixed-sample design for the default parameters
#' des <- des_fixed()
#' @seealso \code{\link{des_gs}}, \code{\link{des_nearopt}},
#' \code{\link{des_opt}}, \code{\link{opchar}},
#' \code{\link{sim}}, \code{\link{plot.OptGS_des}},
#' \code{\link{plot.OptGS_opchar}}, \code{\link{print.OptGS_des}},
#' \code{\link{summary.OptGS_des}}
#' @export
des_fixed <- function(alpha = 0.05, beta = 0.2, delta = 0.2, sigma0 = 1,
                      sigma1 = sigma0, ratio = 1, quantile_sub = FALSE,
                      integer_n = TRUE, summary = FALSE) {

  ##### Check input variables ##################################################

  check_real_range_strict(alpha, "alpha", c(0, 1), 1)
  check_real_range_strict(beta, "beta", c(0, 1), 1)
  check_real_range_strict(delta, "delta", c(0, Inf), 1)
  check_real_range_strict(sigma0, "sigma0", c(0, Inf), 1)
  check_real_range_strict(sigma1, "sigma1", c(0, Inf), 1)
  check_real_range_strict(ratio, "ratio", c(0, Inf), 1)
  check_logical(quantile_sub, "quantile_sub")
  check_logical(integer_n, "integer")
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    summary_des_fixed(alpha, beta, delta, sigma0, sigma1, ratio, quantile_sub,
                      integer)
    message("")
  }

  ##### Perform main computations ##############################################

  e      <- stats::qnorm(1 - alpha)
  n0     <- ((e + stats::qnorm(1 - beta))*sqrt(sigma0^2 + sigma1^2/ratio))^2/
                       delta^2
  if (integer_n) {
    n0   <- ceiling(n0)
    n1   <- n0*ratio
    while (n1%%1 != 0) {
      n0 <- n0 + 1L
      n1 <- n0*ratio
    }
    n0   <- as.integer(n0)
    n1   <- as.integer(n1)
  } else {
    n1          <- n0*ratio
  }
  I             <- information(n0, 1, sigma0, sigma1, ratio)
  n             <- n0 + n1
  if (quantile_sub) {
    e           <- stats::qt(stats::pnorm(e), n*(1 + ratio) - 2)
  }
  opchar        <- opchar_fixed(c(0, delta), e, sqrt(I), n)

  ##### Output results #########################################################

  output        <- list(alpha        = alpha,
                        beta         = beta,
                        CovZ         = 1,
                        delta        = delta,
                        Delta        = NA,
                        e            = e,
                        f            = e,
                        GA           = NA,
                        I            = I,
                        integer_n    = integer_n,
                        J            = 1,
                        method       = NA,
                        n            = n,
                        n0           = n0,
                        n1           = n1,
                        name         = "Fixed-sample",
                        opchar       = opchar,
                        quantile_sub = quantile_sub,
                        ratio        = ratio,
                        shape        = NA,
                        sigma0       = sigma0,
                        sigma1       = sigma1,
                        summary      = summary,
                        w            = NA)
  class(output) <- c(class(output), "OptGS_des")
  output

}

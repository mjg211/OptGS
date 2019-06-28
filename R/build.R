#' Build a group-sequential clinical trial design object for a normally
#' distributed primary outcome
#'
#' \code{build()} allows an object of class \code{OptGS_des}, like those
#' returned by \code{\link{des_gs}}, \code{\link{des_nearopt}}, and
#' \code{\link{des_opt}} to be built. It is for use when the user has a specific
#' group-sequential trial design in mind.
#'
#' @param alpha A \code{\link{numeric}} indicating the value of
#' \ifelse{html}{\out{<i>&alpha;</i>}}{\eqn{\alpha}}, the desired type-I
#' error-rate for the design of interest. Must be strictly between 0 and 1.
#' Defaults to \code{0.05}.
#' @param beta A \code{\link{numeric}} indicating the value of
#' \ifelse{html}{\out{<i>&beta;</i>}}{\eqn{\beta}}, the desired type-II
#' error-rate for the design of interest. Must be strictly between 0 and 1.
#' Defaults to \code{0.2}.
#' @param delta A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&delta;</i>}}{\eqn{\delta}}, the treatment effect to
#' power the design of interest for. Must be strictly positive. Defaults to
#' \code{0.2}.
#' @param e A \code{\link{numeric}} \code{\link{vector}} indicating the values
#' of the efficacy stopping boundaries in the design of interest. Defaults to
#' \code{c(1.88, 1.77)}.
#' @param f A \code{\link{numeric}} \code{\link{vector}} indicating the values
#' of the futility stopping boundaries in the design of interest. Must be of the
#' same length as \code{e}, its last element must be equal to the last element
#' of \code{e}, and the preceeding elements must be strictly less than the
#' corresponding element from \code{e}. Defaults to \code{c(0.63, 1.77)}.
#' @param n0 A \code{\link{numeric}} indicating the value of
#' \ifelse{html}{\out{<i>n</i><sub>0</sub>}}{\eqn{n_0}}, the group size in the
#' control arm, for the design of interest. Defaults to \code{180}.
#' @param n0 A \code{\link{numeric}} indicating the value of
#' \ifelse{html}{\out{<i>n</i><sub>1</sub>}}{\eqn{n_0}}, the group size in the
#' experimental arm, for the design of interest. Defaults to \code{n0}.
#' @param sigma0 A \code{\link{numeric}} indicating the value of
#' \ifelse{html}{\out{<i>&sigma;</i><sub>0</sub>}}{\eqn{\sigma_0}}, the
#' standard deviation of the responses in the control arm in the design of
#' interest. Must be strictly positive. Defaults to \code{1}.
#' @param sigma1 A \code{\link{numeric}} indicating the value of
#' \ifelse{html}{\out{<i>&sigma;</i><sub>1</sub>}}{\eqn{\sigma_1}}, the
#' standard deviation of the responses in the experimental arm in the design of
#' interest. Must be strictly positive. Defaults to \code{sigma0}.
#' @param summary A \code{\link{logical}} variable indicating whether a summary
#' of the function's progress should be printed to the console. Defaults to
#' \code{F}.
#' @return A \code{\link{list}} with additional class \code{"OptGS_des"}. It
#' will contain each of the input variables (subject to internal modification),
#' relating to the various design functions from \code{\link[OptGS]{OptGS}},
#' along with the following elements:
#' \itemize{
#' \item \code{CovZ}: A \code{\link{numeric}} \code{\link{matrix}} giving
#' \ifelse{html}{\out{Cov(<b><i>Z</i></b>)}}{\eqn{Cov(\bold{Z})}}, the
#' covariance between the standardised test statistics for the specified design.
#' \item \code{I}: A \code{\link{numeric}} \code{\link{vector}} giving
#' \ifelse{html}{\out{<b><i>I</i></b>}}{\eqn{\bold{I}}}, the vector of
#' information levels for the specified design.
#' \item \code{n}: A \code{\link{numeric}} \code{\link{vector}} giving
#' \ifelse{html}{\out{<b><i>n</i></b>}}{\eqn{\bold{n}}}, the vector of
#' stage-wise sample sizes for the specified design.
#' \item \code{name}: A \code{\link{character}} string giving a name for the
#' identified design.
#' \item \code{opchar}: A \code{\link[tibble]{tibble}} giving the operating
#' characteristics of the specified design when
#' \ifelse{html}{\out{<i>&tau;</i> = 0}}{\eqn{\tau = 0}},
#' \ifelse{html}{\out{<i>&tau;</i> = <i>&delta;</i>}}{\eqn{\tau = \delta}}, and
#' \ifelse{html}{\out{<i>&tau;</i> =
#' argmax<sub>&theta;</sub><i>ESS</i>(<i>&theta;</i>)}}{
#' \eqn{\tau = argmax_{\theta}ESS(\theta)}}.
#' }
#' @examples
#' # The design for the default parameters
#' des     <- build()
#' # A three-stage design
#' des_3   <- build(e = c(), f = c(), n0 = , n1 = )
#' @seealso \code{\link{an}}, \code{\link{des_gs}}, \code{\link{des_nearopt}},
#' \code{\link{des_opt}}, \code{\link{opchar}}, \code{\link{sim}},
#' \code{\link{plot.OptGS_des}}, \code{\link{plot.OptGS_opchar}},
#' \code{\link{print.OptGS_des}}, \code{\link{summary.OptGS_des}}.
#' @export
build <- function(alpha = 0.05, beta = 0.2, delta = 0.2, e = c(1.88, 1.77),
                  f = c(0.63, 1.77), n0 = 180, n1 = n0, sigma0 = 1,
                  sigma1 = sigma0, summary = F) {

  ##### Check input variables ##################################################

  check_real_range_strict( alpha,  "alpha", c(0,   1), 1)
  check_real_range_strict(  beta,   "beta", c(0,   1), 1)
  check_real_range_strict( delta,  "delta", c(0, Inf), 1)
  #check_ef(e, f)
  check_real_range_strict(    n0,     "n0", c(0, Inf), 1)
  check_real_range_strict(    n1,     "n1", c(0, Inf), 1)
  check_real_range_strict(sigma0, "sigma0", c(0, Inf), 1)
  check_real_range_strict(sigma1, "sigma1", c(0, Inf), 1)
  check_real_range_strict( ratio,  "ratio", c(0, Inf), 1)
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    summary_build(alpha, beta, delta, e, f, n0, n1, sigma0, sigma1)
    message("")
  }

  ##### Perform main computations ##############################################

  J          <- length(e)
  if (n0%%1 == 0) {
    n0       <- as.integer(n0)
  }
  if (n1%%1 == 0) {
    n1       <- as.integer(n1)
  }
  n          <- (n0 + n1)*(1:J)
  ratio      <- n1/n0
  CovZ       <- covariance(sqrt(1:J))
  sqrt_I     <- sqrt(I <- information(n0, J, sigma0, sigma1, ratio))
  argmax_ess <- stats::optim(par    = 0.5*delta,
                             fn     = minus_ess,
                             method = "Brent",
                             lower  = 0,
                             upper  = delta,
                             e      = e,
                             f      = f,
                             sqrt_I = sqrt_I,
                             CovZ   = CovZ,
                             n      = n)$par
  opchar     <- opchar_int(sort(c(0, argmax_ess, delta)), e, f, sqrt_I, CovZ, n)

  ##### Output results #########################################################

  output        <- list(alpha        = alpha,
                        beta         = beta,
                        CovZ         = CovZ,
                        delta        = delta,
                        Delta        = NA,
                        e            = e,
                        f            = f,
                        GA           = NA,
                        I            = I,
                        integer_n    = all(is.integer(n0), is.integer(n1)),
                        J            = J,
                        method       = NA,
                        n            = n,
                        n0           = n0,
                        n1           = n1,
                        name         = "Built",
                        opchar       = opchar,
                        quantile_sub = NA,
                        ratio        = ratio,
                        shape        = NA,
                        sigma0       = sigma0,
                        sigma1       = sigma1,
                        summary      = summary,
                        w            = NA)
  class(output) <- c(class(output), "OptGS_des")
  output

}

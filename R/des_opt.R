#' Design an optimal group-sequential clinical trial for a normally distributed
#' primary outcome
#'
#' \code{des_opt()} determines optimal group-sequential clinical trial designs
#' assuming the primary outcome variable is normally distributed, using the
#' approach proposed in Wason \emph{et al} (2012).
#'
#' @param J A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>J</i>}}{\eqn{J}}, the maximal allowed number of stages.
#' Must be an integer greater than or equal to 2. Defaults to \code{2}.
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
#' @param w A \code{\link{numeric}} \code{\link{vector}} of length 4 indicating
#' the weights to use in the optimality criteria. All elements must be greater
#' than or equal to 0, and at least one of the first 3 elements must be strictly
#' positive. See the vignette for further details. Defaults to
#' \code{c(1, 0, 0, 0)}.
#' @param quantile_sub A \code{\link{logical}} variable indicating whether
#' quantile substitution should be applied to the identified stopping
#' boundaries. Defaults to \code{F}.
#' @param integer_n A \code{\link{logical}} variable indicating whether the
#' computed values for \ifelse{html}{\out{<i>n</i><sub>0</sub>}}{\eqn{n_0}} and
#' \ifelse{html}{\out{<i>n</i><sub>1</sub>}}{\eqn{n_1}}, the group sizes in the
#' control and experimental arms, should be forced to be whole numbers. Defaults
#' to \code{T}.
#' @param parallel See \code{\link[GA]{ga}}.
#' @param popSize See \code{\link[GA]{ga}}.
#' @param maxiter See \code{\link[GA]{ga}}.
#' @param run See \code{\link[GA]{ga}}.
#' @param seed A variable to be passed to \code{\link{set.seed}}, to make the
#' calculations reproducible.
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
#' covariance between the standardised test statistics for the identified
#' design.
#' \item \code{e}: A \code{\link{numeric}} \code{\link{vector}} giving
#' \ifelse{html}{\out{<b><i>e</i></b>}}{\eqn{\bold{e}}}, the efficacy stopping
#' boundaries for the identified design.
#' \item \code{f}: A \code{\link{numeric}} \code{\link{vector}} giving
#' \ifelse{html}{\out{<b><i>f</i></b>}}{\eqn{\bold{f}}}, the futility stopping
#' boundaries for the identified design.
#' \item \code{GA}: A \code{\link{list}} containing the output from the call to
#' \code{\link[GA]{ga}} and each of the \code{\link[GA]{ga}} specific input
#' parameters.
#' \item \code{I}: A \code{\link{numeric}} \code{\link{vector}} giving
#' \ifelse{html}{\out{<b><i>I</i></b>}}{\eqn{\bold{I}}}, the vector of
#' information levels for the identified design.
#' \item \code{n}: A \code{\link{numeric}} \code{\link{vector}} giving
#' \ifelse{html}{\out{<b><i>n</i></b>}}{\eqn{\bold{n}}}, the vector of
#' stage-wise sample sizes for the identified design.
#' \item \code{n0}: A \code{\link{numeric}} giving
#' \ifelse{html}{\out{<i>n</i><sub>0</sub>}}{\eqn{n_0}}, the group size in the
#' control arm for the identified design.
#' \item \code{n1}: A \code{\link{numeric}} giving
#' \ifelse{html}{\out{<i>n</i><sub>1</sub>}}{\eqn{n_1}}, the group size in the
#' experimental arm for the identified design.
#' \item \code{name}: A \code{\link{character}} string giving a name for the
#' identified design.
#' \item \code{opchar}: A \code{\link[tibble]{tibble}} giving the operating
#' characteristics of the identified design when
#' \ifelse{html}{\out{<i>&tau;</i> = 0}}{\eqn{\tau = 0}},
#' \ifelse{html}{\out{<i>&tau;</i> = <i>&delta;</i>}}{\eqn{\tau = \delta}}, and
#' \ifelse{html}{\out{<i>&tau;</i> =
#' argmax<sub>&theta;</sub><i>ESS</i>(<i>&theta;</i>)}}{
#' \eqn{\tau = argmax_{\theta}ESS(\theta)}}.
#' }
#' @examples
#' # The optimal design for the default parameters
#' des     <- des_opt()
#' # A three-stage optimal design
#' des_3   <- des_opt(J = 3)
#' # Optimal under the alternative hypothesis
#' des_alt <- des_opt(w = c(0, 1, 0, 0))
#' @seealso \code{\link{an}}, \code{\link{build}}, \code{\link{des_gs}},
#' \code{\link{des_nearopt}}, \code{\link{opchar}}, \code{\link{sim}},
#' \code{\link{plot.OptGS_des}}, \code{\link{plot.OptGS_opchar}},
#' \code{\link{print.OptGS_des}}, \code{\link{summary.OptGS_des}}.
#' @export
des_opt <- function(J = 2, alpha = 0.05, beta = 0.2, delta = 0.2, sigma0 = 1,
                    sigma1 = sigma0, ratio = 1, w = c(1, 0, 0, 0),
                    quantile_sub = F, integer_n = T, parallel = 1, popSize = 50,
                    maxiter = 100, run = maxiter, seed = Sys.time(),
                    summary = F) {

  set.seed(seed)

  ##### Check input variables ##################################################

  J <- check_integer_range(J, "J", c(1, Inf), 1)
  check_real_range_strict( alpha,  "alpha", c(0,   1), 1)
  check_real_range_strict(  beta,   "beta", c(0,   1), 1)
  check_real_range_strict( delta,  "delta", c(0, Inf), 1)
  check_real_range_strict(sigma0, "sigma0", c(0, Inf), 1)
  check_real_range_strict(sigma1, "sigma1", c(0, Inf), 1)
  check_real_range_strict( ratio,  "ratio", c(0, Inf), 1)
  w <- check_w(w)
  check_logical(quantile_sub, "quantile_sub")
  check_logical(   integer_n,    "integer_n")
  check_logical(     summary,      "summary")

  ##### Print summary ##########################################################

  if (summary) {
    summary_des_nearopt(J, alpha, beta, delta, sigma0, sigma1, ratio, w,
                        quantile_sub, integer_n)
    message("")
  }

  ##### Perform main computations ##############################################

  CovZ       <- covariance(sqrt(1:J))
  penalty    <- (1 + ratio)*((stats::qnorm(1 - alpha)*sigma0*sqrt(1 + 1/ratio) +
                                stats::qnorm(1 - beta)*
                                sqrt(sigma0^2 + sigma1^2/ratio))/delta)^2
  fitness    <- function(...) { -optimal(...) }
  lower      <- c(0, rep(-20, J), numeric(J - 1))
  upper      <- c(penalty, rep(20, 2*J - 1))
  ga         <- GA::ga(type       = "real-valued",
                       fitness    = fitness,
                       J          = J,
                       alpha      = alpha,
                       beta       = beta,
                       delta      = delta,
                       sigma0     = sigma0,
                       sigma1     = sigma1,
                       ratio      = ratio,
                       w          = w,
                       penalty    = penalty,
                       CovZ       = CovZ,
                       lower      = lower,
                       upper      = upper,
                       maxiter    = maxiter,
                       parallel   = parallel,
                       popSize    = popSize,
                       run        = run)
  n0         <- ga@solution[1]
  if (integer_n) {
    n0       <- ceiling(n0)
    n1       <- n0*ratio
    while (n1%%1 != 0) {
      n0     <- n0 + 1L
      n1     <- n0*ratio
    }
    n0       <- as.integer(n0)
    n1       <- as.integer(n1)
  } else {
    n1       <- n0*ratio
  }
  f          <- ga@solution[2:(J + 1)]
  e          <- c(f[1:(J - 1)] + ga@solution[(J + 2):(2*J)], f[J])
  sqrt_I     <- sqrt(I <- information(n0, J, sigma0, sigma1, ratio))
  n          <- (n <- n0 + n1)*(1:J)
  if (quantile_sub) {
    e        <- stats::qt(stats::pnorm(e), (1:J)*(n*(1 + ratio) - 2))
    f        <- stats::qt(stats::pnorm(f), (1:J)*(n*(1 + ratio) - 2))
  }
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
                        GA           =
                          list(ga         = ga,
                               ga_options = list(parallel = parallel,
                                                 popSize  = popSize,
                                                 maxiter  = maxiter,
                                                 run      = run,
                                                 seed     = seed)),
                        I            = I,
                        integer_n    = integer_n,
                        J            = J,
                        method       = NA,
                        n            = n,
                        n0           = n0,
                        n1           = n1,
                        name         = paste("Optimal: w = (",
                                             paste(w, collapse = ", "), ")",
                                             sep = ""),
                        opchar       = opchar,
                        quantile_sub = quantile_sub,
                        ratio        = ratio,
                        shape        = NA,
                        sigma0       = sigma0,
                        sigma1       = sigma1,
                        summary      = summary,
                        w            = w)
  class(output) <- c(class(output), "OptGS_des")
  output

}

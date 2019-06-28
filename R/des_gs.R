#' Design a group-sequential clinical trial for a normally distributed primary
#' outcome
#'
#' \code{des_gs()} determines (non-optimised) group-sequential clinical trial
#' designs assuming the primary outcome variable is normally distributed. It
#' supports a variety of popular boundary shapes: Haybittle-Peto, power-family,
#' triangular, and Wang-Tsiatis designs.
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
#' @param shape A \code{\link{character}} string indicating the chosen
#' stopping boundary shape. Must be one of \code{"haybittle_peto"},
#' \code{"power_family"}, \code{"triangular"}, or \code{"wang_tsiatis"}.
#' Defaults to \code{"power_family"}.
#' @param Delta Only used if \code{shape} is equal to \code{"power_family"} or
#' \code{"wang_tsiatis"}. Then, it is a \code{\link{numeric}} (potentially a
#' \code{\link{numeric}} \code{\link{vector}}) indicating the boundary shape
#' parameter(s). Specifically, for \code{shape = "wang_tsiatis"} it should be a
#' single \code{\link{numeric}}, while for \code{shape = "power_family"} it can
#' be a \code{\link{numeric}} \code{\link{vector}} of length 1 or 2. Defaults to
#' \code{0}.
#' @param quantile_sub A \code{\link{logical}} variable indicating whether
#' quantile substitution should be applied to the identified stopping
#' boundaries. Defaults to \code{F}.
#' @param integer_n A \code{\link{logical}} variable indicating whether the
#' computed values for \ifelse{html}{\out{<i>n</i><sub>0</sub>}}{\eqn{n_0}} and
#' \ifelse{html}{\out{<i>n</i><sub>1</sub>}}{\eqn{n_1}}, the group sizes in the
#' control and experimental arms, should be forced to be whole numbers. Defaults
#' to \code{T}.
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
#' # The design for the default parameters
#' des     <- des_gs()
#' # A three-stage design
#' des_3   <- des_gs(J = 3)
#' # With triangular-test boundaries
#' des_tri <- des_gs(shape = "triangular")
#' @seealso \code{\link[OptGS]{an}}, \code{\link{build}}, \code{\link{des_nearopt}},
#' \code{\link{des_opt}}, \code{\link{opchar}}, \code{\link{sim}},
#' \code{\link{plot.OptGS_des}}, \code{\link{plot.OptGS_opchar}},
#' \code{\link{print.OptGS_des}}, \code{\link{summary.OptGS_des}}.
#' @export
des_gs <- function(J = 2, alpha = 0.05, beta = 0.2, delta = 0.2, sigma0 = 1,
                   sigma1 = sigma0, ratio = 1, shape = "power_family",
                   Delta = 0, quantile_sub = F, integer_n = T, summary = F) {

  ##### Check input variables ##################################################

  J       <- check_integer_range(J, "J", c(1, Inf), 1)
  check_real_range_strict( alpha,  "alpha", c(0,   1), 1)
  check_real_range_strict(  beta,   "beta", c(0,   1), 1)
  check_real_range_strict( delta,  "delta", c(0, Inf), 1)
  check_real_range_strict(sigma0, "sigma0", c(0, Inf), 1)
  check_real_range_strict(sigma1, "sigma1", c(0, Inf), 1)
  check_real_range_strict( ratio,  "ratio", c(0, Inf), 1)
  check_belong(shape, "shape", c("haybittle_peto", "power_family", "triangular",
                                 "wang_tsiatis"), 1L)
  Delta   <- check_Delta(Delta, shape)
  check_logical(quantile_sub, "quantile_sub")
  check_logical(   integer_n,    "integer_n")
  check_logical(     summary,      "summary")
  check_default(shape %in% c("haybittle_peto", "triangular"), "shape", Delta,
                "Delta", 0)
  if (shape %in% c("haybittle_peto", "triangular")) {
    Delta <- NA
  }

  ##### Print summary ##########################################################

  if (summary) {
    summary_des_gs(J, alpha, beta, delta, sigma0, sigma1, ratio, shape, Delta,
                   quantile_sub, integer_n)
    message("")
  }

  ##### Perform main computations ##############################################

  seq_J         <- 1:J
  CovZ          <- covariance(sqrt(seq_J))
  if (shape %in% c("haybittle_peto", "wang_tsiatis")) {
    C           <- stats::uniroot(f        = eval_C_hp_wt,
                                  interval = c(1e-16, 1e16),
                                  alpha    = alpha,
                                  shape    = shape,
                                  Delta    = Delta,
                                  CovZ     = CovZ)$root
    if (shape == "haybittle_peto") {
      e         <- c(rep(3, J - 1), C)
    } else {
      e         <- C*(seq_J/J)^(Delta - 0.5)
    }
    f           <- fu <- c(-e[seq_J[-J]], C)
    names(e)    <- names(f) <- names(fu) <- NULL
    n0          <- stats::uniroot(f        = eval_n0_hp_wt,
                                  interval = c(1e-16, 1e16),
                                  delta    = delta,
                                  sigma0   = sigma0,
                                  sigma1   = sigma1,
                                  ratio    = ratio,
                                  CovZ     = CovZ,
                                  e        = e,
                                  fu       = fu,
                                  power    = 1 - beta)$root
  } else if (shape == "triangular") {
    z_alpha     <- stats::qnorm(1 - alpha)
    delta_tilde <- 2*z_alpha*delta/(z_alpha + stats::qnorm(1 - beta))
    I           <- seq_J*(((sqrt(4*(0.583^2)/J + 8*log(1/(2*alpha))) -
                              2*0.583/sqrt(J))^2)/(delta_tilde^2))/J
    e           <- ((2/delta_tilde)*log(1/(2*alpha)) - 0.583*sqrt(I[J]/J) +
                      delta_tilde*I[J]*seq_J/(4*J))/sqrt(I)
    f           <- (-(2/delta_tilde)*log(1/(2*alpha)) + 0.583*sqrt(I[J]/J) +
                      3*delta_tilde*I[J]*seq_J/(4*J))/sqrt(I)
    n0          <- I[1]*(sigma0^2 + sigma1^2/ratio)
  } else if (shape == "power_family") {
    sqrt_I_fac  <- sqrt(seq_J/J)
    e_fac       <- (seq_J/J)^(Delta[1] - 0.5)
    f_fac       <- (seq_J/J)^(Delta[2] - 0.5)
    C           <-
      stats::optim(par        = c(0.5, 0.5),
                   fn         = eval_C_pf,
                   J          = J,
                   alpha      = alpha,
                   beta       = beta,
                   delta      = delta,
                   CovZ       = CovZ,
                   e_fac      = e_fac,
                   f_fac      = f_fac,
                   sqrt_I_fac = sqrt_I_fac,
                   seq_j      = lapply(seq_J, function(j) { 1:j }),
                   seq_jm1    = lapply(seq_J,
                                       function(j) { seq_len(j - 1) }))$par
    e           <- C[2]*e_fac
    f           <- sqrt_I_fac*abs(sum(C)) - C[1]*f_fac
    n0          <- ((seq_J/J)*(sum(C)/delta)^2)[1]*(sigma0^2 + sigma1^2/ratio)
  }
  if (integer_n) {
    n0          <- ceiling(n0)
    n1          <- n0*ratio
    while (n1%%1 != 0) {
      n0        <- n0 + 1L
      n1        <- n0*ratio
    }
    n0          <- as.integer(n0)
    n1          <- as.integer(n1)
  } else {
    n1          <- n0*ratio
  }
  sqrt_I        <- sqrt(I <- information(n0, J, sigma0, sigma1, ratio))
  n             <- (n0 + n1)*seq_J
  if (quantile_sub) {
    e           <- stats::qt(stats::pnorm(e), seq_J*(n[1]*(1 + ratio) - 2))
    f           <- stats::qt(stats::pnorm(f), seq_J*(n[1]*(1 + ratio) - 2))
  }
  argmax_ess    <- stats::optim(par    = 0.5*delta,
                                fn     = minus_ess,
                                method = "Brent",
                                lower  = 0,
                                upper  = delta,
                                e      = e,
                                f      = f,
                                sqrt_I = sqrt_I,
                                CovZ   = CovZ,
                                n      = n)$par
  opchar        <- opchar_int(sort(c(0, argmax_ess, delta)), e, f, sqrt_I, CovZ,
                              n)

  ##### Output results #########################################################

  if (shape == "haybittle_peto") {
    name        <- "Haybittle-Peto"
  } else if (shape == "power_family") {
    name        <- paste0("Power-family: Delta = (",
                          paste(Delta, collapse = ", "), ")")
  } else if (shape == "triangular") {
    name        <- "Triangular"
  } else if (shape == "wang_tsiatis") {
    name        <- paste0("Wang-Tsiatis: Delta = ", Delta)
  }
  output        <- list(alpha        = alpha,
                        beta         = beta,
                        CovZ         = CovZ,
                        delta        = delta,
                        Delta        = Delta,
                        e            = e,
                        f            = f,
                        GA           = NA,
                        I            = I,
                        integer_n    = integer_n,
                        J            = J,
                        method       = NA,
                        n            = n,
                        n0           = n0,
                        n1           = n1,
                        name         = name,
                        opchar       = opchar,
                        quantile_sub = quantile_sub,
                        ratio        = ratio,
                        shape        = shape,
                        sigma0       = sigma0,
                        sigma1       = sigma1,
                        summary      = summary,
                        w            = NA)
  class(output) <- c(class(output), "OptGS_des")
  output

}

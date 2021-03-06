% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/opchar.R
\name{opchar}
\alias{opchar}
\title{Calculate the operating characteristics of a group-sequential clinical trial
design for a normally distributed primary outcome}
\usage{
opchar(
  des = OptGS::des_gs(),
  tau = seq(-des$delta, 2 * des$delta, length.out = 100),
  summary = FALSE
)
}
\arguments{
\item{des}{An object of class \code{"OptGS_des"}, as returned by
\code{\link{des_gs}}, \code{\link{des_nearopt}}, \code{\link{des_opt}}, or
\code{\link{build}}. Defaults to \code{des_gs()}.}

\item{tau}{A \code{\link{numeric}} \code{\link{vector}} indicating the values
of \ifelse{html}{\out{<i>&tau;</i>}}{\eqn{\tau}} to perform calculations for.
Defaults to \code{seq(-des$delta, 2*des$delta, length.out = 100)}.}

\item{summary}{A \code{\link{logical}} variable indicating whether a summary
of the function's progress should be printed to the console. Defaults to
\code{FALSE}.}
}
\value{
A \code{\link{list}} with additional class \code{"OptGS_opchar"}. It
will contain each of the input variables (subject to internal modification),
along with the following elements:
\itemize{
\item \code{opchar}: A \code{\link{tibble}} giving the calculated operating
characteristics.
}
}
\description{
\code{opchar()} analytically calculates the operating characteristics of a
group-sequential clinical trial.
}
\examples{
# Calculations for the default parameters
opchar     <- opchar()
}
\seealso{
\code{\link{des_gs}}, \code{\link{des_nearopt}},
\code{\link{des_opt}}, \code{\link{build}}, \code{\link{est}},
\code{\link{sim}}, \code{\link{plot.OptGS_opchar}}
}

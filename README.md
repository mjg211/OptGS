
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OptGS

*Optimal and near-optimal group-sequential designs for clinical trials
with continuous outcomes*

## Introduction

**OptGS** is an [R](https://www.r-project.org/) package that provides a
suite of functions to assist with the design, analysis, and
visualization of randomized two-arm group-sequential clinical trials
with continuous outcome variables.

Specifically, support is provided to perform a sample size calculation
for each of the most popular relevant (non-optimal) designs. The unique
focus, however, is on determining optimal and near-optimal designs.

Additional functions then allow point estimates, p-values, and
confidence intervals to be determined for possible results in these
designs. Plotting functions also permit the informative depiction of
several important quantities.

## Installation

You can install the released version of **OptGS** from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("OptGS")
```

Alternatively, the current development version from
[Github](https://github.com/) can be installed with:

``` r
devtools::install_github("mjg211/OptGS")
```

## Example: Near-optimal design

This is a basic example, which demonstrates how to determine an
optimized power-family design (a near-optimal design), plot its stopping
boundaries, determine its operating characteristics, and subsequently
produce a plot of the expected sample size curve.

First, determine the design (for the default parameters) with:

``` r
near_optimal <- des_power_family()
```

We can then plot the stopping boundaries of this design with:

``` r
plot(near_optimal)
```

The operating characteristic of the design can also be determined with:

``` r
opchar <- opchar_gs(near_optimal)
```

Finally, we can then plot the expected sample size curve for this design
using:

``` r
plots <- plot(opchar, output = T)
plots$ESS
```

## Changes: v.1.1.1 vs. v.2.0.0

Between v.1.1.1 (the latest released version on
[CRAN](https://CRAN.R-project.org)) and v.2.0.0 (the current development
version on [Github](https://github.com/)), several major changes were
made to **OptGS**:

  - Dependence on C++ code was replaced with equivalent **Rcpp** or
    **R** functionality for stability and ease of further development.
  - Support for additional plots were added (e.g., stopping boundaries,
    median sample size curves).
  - Functions to determine operating characteristics (`opchar_gs()`),
    perform inference on trial conclusion (`ci_gs()`, `est_gs()`, and
    `pval_gs()`), simulate group-sequential trials (`sim_gs()`), and
    build bespoke designs (`build_gs()`) were added.
  - Arguments in, and names of, previously present functions have been
    modified (e.g., `optgs()` is replaced by `des_power_family()`).

Consequently, if all that you require is the functionality presented in
[Wason (2015)](https://doi.org/10.18637/jss.v066.i02), it may be quicker
to use v.1.1.1 from [CRAN](https://CRAN.R-project.org), which is a
substantially simpler package. However, as time progresses, the
additional support provided in v.2.0.0 and on should make them
preferable with some small time investment to understand the purpose of
the different functions.

## Support

An extensive guide to using **OptGS** is provided in the form of a
package vignette, accessible using `vignette("OptGS")`. For v.1.1.1 and
earlier, [Wason (2015)](https://doi.org/10.18637/jss.v066.i02) also
provides a detailed introduction to the package.

If you cannot find the answer to your problem in either of these
locations, or a function is returning an unexpected error for your
inputs, please contact James Wason (<james.wason@newcastle.ac.uk>) or
Michael Grayling (<michael.grayling@newcastle.ac.uk>) for assistance.

## References

Wason JMS (2015) OptGS: An R package for finding near-optimal
group-sequential designs. *Journal of Statistical Software*
**66**(2)\_\_:\_\_1–13. DOI:
[10.18637/jss.v066.i02](https://doi.org/10.18637/jss.v066.i02).

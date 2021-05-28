bias_mle              <- function(tau, sqrt_I1, f1, e1, fac3) {
  fac3*(stats::dnorm(e1, tau*sqrt_I1, 1) - stats::dnorm(f1, tau*sqrt_I1, 1))
}

cmue_optim            <- function(tau, k, z, sqrt_I1, sqrt_I2, f1, e1,
                                  sqrt_Iratio, fac1, fac2) {
  S1      <- stats::pnorm(f1, tau*sqrt_I1) + (1 - pnorm(e1, tau*sqrt_I1))
  if (k == 1) {
    if (z >= e1) {
      num <- stats::pnorm(f1, tau*sqrt_I1) + stats::pnorm(z, tau*sqrt_I1) -
        stats::pnorm(e1, tau*sqrt_I1)
    } else {
      num <- stats::pnorm(z, tau*sqrt_I1)
    }
    int   <- num/S1
  } else {
    if (is.infinite(e1)) {
      log_S2 <- pnorm(tau*sqrt_I1 - f1, log.p = TRUE)
    } else {
      log_S2 <- log(pnorm(e1, tau*sqrt_I1) - pnorm(f1, tau*sqrt_I1))
    }
    z        <- seq(qnorm(0.0001, z), z, length.out = 1000)
    cf2z     <- exp(f2z(z, tau, sqrt_I1, sqrt_I2, f1, e1, sqrt_Iratio, fac1,
                        fac2, log = TRUE) - log_S2)
    int      <- fda.usc::int.simpson2(z, cf2z)
  }
  (int - 0.5)^2
}

covariance            <- function(sqrt_I) {
  CovZ            <- diag(length(sqrt_I))
  for (k1 in seq_len(length(sqrt_I) - 1) + 1L) {
    vec           <- 1:(k1 - 1)
    CovZ[k1, vec] <- CovZ[vec, k1] <- sqrt_I[vec]/sqrt_I[k1]
  }
  CovZ
}

est_int               <- function(tau, e1, f1, I, tau0, summary,
                                  density = 10000) {
  sqrt_I            <- sqrt(I)
  sqrt_I1           <- sqrt_I[1]
  sqrt_I2           <- sqrt_I[2]
  sqrt_Iratio       <- sqrt(I[1]/I[2])
  CovZ              <- rbind(c(1, sqrt_Iratio), c(sqrt_Iratio, 1))
  fac1              <- sqrt(I[2] - I[1])/sqrt_I2
  fac2              <- sqrt(2*pi)
  fac3              <- (I[2] - I[1])/(I[2]*sqrt_I1)
  L1                <- stats::pnorm(f1, tau*sqrt_I1)
  U1                <- 1 - stats::pnorm(e1, tau*sqrt_I1)
  S1                <- L1 + U1
  len_tau           <- length(tau)
  E11               <- E12 <- E21 <- E22 <- matrix(0, len_tau, 9)
  z1                <- seq(qnorm(1e-6, tau[1]*sqrt_I1),
                           qnorm(1 - 1e-6, tau[len_tau]*sqrt_I1),
                           length.out = density)
  z2                <- seq(qnorm(1e-6, tau[1]*sqrt_I2),
                           qnorm(1 - 1e-6, tau[len_tau]*sqrt_I2),
                           length.out = density)
  est_mle1          <- z1/sqrt_I1
  c                 <- (est_mle1 - tau0)^2/((est_mle1 - tau0)^2 + I[1]^-1)
  tau_star          <- c*est_mle1 + (1 - c)*tau0
  est_mle2          <- z2/sqrt_I2
  est_mae11         <- est_mae12  <- est_cmle1  <- est_cmue1 <- est_cwmae1 <-
                       est_mae21  <- est_mae22  <- est_cmle2 <- est_cmue2  <-
                       est_mue2   <- est_umvue2 <- numeric(density)
  for (i in 1:density) {
    if (z1[i] < f1 | z1[i] > e1) {
      est_mae11[i]  <- uniroot(mae_root, c(-1e10, 1e10), mle = est_mle1[i],
                               sqrt_I1 = sqrt_I1, f1 = f1, e1 = e1,
                               fac3 = fac3)$root
      est_mae12[i]  <- est_mle1[i] - bias_mle(est_mle1[i], sqrt_I1, f1, e1,
                                              fac3)
      est_cmle1[i]  <- optim(est_mle1[i], minus_clog_likelihood, k = 1,
                             z = z1[i], sqrt_I1 = sqrt_I1, sqrt_I2 = sqrt_I2,
                             f1 = f1, e1 = e1, sqrt_Iratio = sqrt_Iratio,
                             fac1 = fac1, fac2 = fac2, method = "BFGS")$par
      est_cmue1[i]  <- optim(est_mle1[i], cmue_optim, k = 1, z = z1[i],
                             sqrt_I1 = sqrt_I1, sqrt_I2 = sqrt_I2, f1 = f1,
                             e1 = e1, sqrt_Iratio = sqrt_Iratio, fac1 = fac1,
                             fac2 = fac2, method = "BFGS")$par
      est_cwmae1[i] <- est_mle1[i] - bias_mle(tau_star[i], sqrt_I1, f1, e1,
                                              fac3)
    }
    est_mae21[i]    <- uniroot(mae_root, c(-1e10, 1e10), mle = est_mle2[i],
                               sqrt_I1 = sqrt_I1, f1 = f1, e1 = e1,
                               fac3 = fac3)$root
    est_mae22[i]    <- est_mle2[i] - bias_mle(est_mle2[i], sqrt_I1, f1, e1,
                                              fac3)
    est_cmle2[i]    <- try(optim(est_mle2[i], minus_clog_likelihood, k = 2,
                                 z = z2[i], sqrt_I1 = sqrt_I1,
                                 sqrt_I2 = sqrt_I2, f1 = f1, e1 = e1,
                                 sqrt_Iratio = sqrt_Iratio, fac1 = fac1,
                                 fac2 = fac2, method = "BFGS")$par,
                           silent = TRUE)
    est_cmue2[i]    <- try(optim(est_mle2[i], cmue_optim, k = 2, z = z2[i],
                                 sqrt_I1 = sqrt_I1, sqrt_I2 = sqrt_I2, f1 = f1,
                                 e1 = e1, sqrt_Iratio = sqrt_Iratio,
                                 fac1 = fac1, fac2 = fac2,
                                 method = "BFGS")$par, silent = TRUE)
    est_umvue2[i]   <- umvue_2(z2[i], sqrt_I2, f1, e1, sqrt_Iratio, fac1, fac3)
    est_mue2[i]     <- uniroot(mue_2_root, c(-1e10, 1e10), z = z2[i],
                               sqrt_I1 = sqrt_I1, sqrt_I = sqrt_I, f1 = f1,
                               e1 = e1, CovZ = CovZ)$root
    if (all(i%%1000 == 0, summary)) {
      message("..", i, "/", density, " required estimates computed..")
    }
  }
  est_cmle2         <- suppressWarnings(as.numeric(est_cmle2))
  est_cmue2         <- suppressWarnings(as.numeric(est_cmue2))
  failed_cmle2      <- which(is.na(est_cmle2))
  failed_cmue2      <- which(is.na(est_cmue2))
  if (length(failed_cmle2) > 0) {
    for (i in 1:length(failed_cmle2)) {
      not_failed    <- which(!is.na(est_cmle2))
      ordered_dist  <- order(abs(not_failed - i))
      est_cmle2[failed_cmle2[i]] <-
        0.5*(est_cmle2[not_failed[ordered_dist[1]]] +
               est_cmle2[not_failed[ordered_dist[2]]])
    }
  }
  if (length(failed_cmue2) > 0) {
    for (i in 1:length(failed_cmue2)) {
      not_failed    <- which(!is.na(est_cmue2))
      ordered_dist  <- order(abs(not_failed - i))
      est_cmue2[failed_cmue2[i]] <- 0.5*(est_cmue2[ordered_dist[1]] +
                                           est_cmue2[ordered_dist[2]])
    }
  }
  est_cmle2[is.na(est_cmle2)]      <- 0
  est_cmue2[is.na(est_cmue2)]      <- 0
  est_cumvue2       <- (z2*sqrt_I2 - I[1]*est_umvue2)/(I[2] - I[1])
  est11             <- rbind(est_mae11, est_mae12, est_cmle1, est_cmue1,
                             est_mle1, est_cwmae1)
  est12             <- est11*est11
  est21             <- rbind(est_mae21, est_mae22, est_cmle2, est_cmue2,
                             est_cumvue2, est_mle2, est_mue2, est_umvue2)
  est22             <- est21*est21
  for (i in 1:len_tau) {
    f1z1            <- f1z(z1, tau[i], sqrt_I1, f1, e1)
    f2z2            <- f2z(z2, tau[i], sqrt_I1, sqrt_I2, f1, e1, sqrt_Iratio,
                          fac1, fac2)
    integrand11     <- matrix(f1z1, 6, density, byrow = TRUE)*est11
    E11[i, ]        <- c(fda.usc::int.simpson2(z1, integrand11[1, ]),
                         fda.usc::int.simpson2(z1, integrand11[2, ]),
                         fda.usc::int.simpson2(z1, integrand11[3, ]),
                         fda.usc::int.simpson2(z1, integrand11[4, ]),
                         rep(fda.usc::int.simpson2(z1, integrand11[5, ]), 4),
                         fda.usc::int.simpson2(z1, integrand11[6, ]))/S1[i]
    integrand12     <- matrix(f1z1, 6, density, byrow = TRUE)*est12
    E12[i, ]        <- c(fda.usc::int.simpson2(z1, integrand12[1, ]),
                         fda.usc::int.simpson2(z1, integrand12[2, ]),
                         fda.usc::int.simpson2(z1, integrand12[3, ]),
                         fda.usc::int.simpson2(z1, integrand12[4, ]),
                         rep(fda.usc::int.simpson2(z1, integrand12[5, ]), 4),
                         fda.usc::int.simpson2(z1, integrand12[6, ]))/S1[i]
    integrand21     <- matrix(f2z2, 8, density, byrow = TRUE)*est21
    temp            <- fda.usc::int.simpson2(z2, integrand21[6, ])
    E21[i, ]        <- c(fda.usc::int.simpson2(z2, integrand21[1, ]),
                         fda.usc::int.simpson2(z2, integrand21[2, ]),
                         fda.usc::int.simpson2(z2, integrand21[3, ]),
                         fda.usc::int.simpson2(z2, integrand21[4, ]),
                         fda.usc::int.simpson2(z2, integrand21[5, ]),
                         temp,
                         fda.usc::int.simpson2(z2, integrand21[7, ]),
                         fda.usc::int.simpson2(z2, integrand21[8, ]),
                         temp)/(1 - S1[i])
    integrand22     <- matrix(f2z2, 8, density, byrow = TRUE)*est22
    temp            <- fda.usc::int.simpson2(z2, integrand22[6, ])
    E22[i, ]        <- c(fda.usc::int.simpson2(z2, integrand22[1, ]),
                         fda.usc::int.simpson2(z2, integrand22[2, ]),
                         fda.usc::int.simpson2(z2, integrand22[3, ]),
                         fda.usc::int.simpson2(z2, integrand22[4, ]),
                         fda.usc::int.simpson2(z2, integrand22[5, ]),
                         temp,
                         fda.usc::int.simpson2(z2, integrand22[7, ]),
                         fda.usc::int.simpson2(z2, integrand22[8, ]),
                         temp)/(1 - S1[i])
  }
  keep              <- which(z1 < f1 | z1 > e1)
  estimates         <-
    tibble::tibble(z               = rep(c(z1[keep], z2), 9),
                   k               = rep(c(rep(1, length(keep)),
                                           rep(2, density)), 9),
                   estimator       =
                     rep(c("MAE1", "MAE2", "CMLE", "CMUE", "CUMVUE", "MLE",
                           "MUE", "UMVUE", "CWMAE"), each = length(z)/9),
                   `hat(tau)(z,k)` = c(est_mae11[keep], est_mae21,
                                       est_mae12[keep], est_mae22,
                                       est_cmle1[keep], est_cmle2,
                                       est_cmue1[keep], est_cmue2,
                                       est_mle1[keep], est_cumvue2,
                                       est_mle1[keep], est_mle2,
                                       est_mle1[keep], est_mue2,
                                       est_mle1[keep], est_umvue2,
                                       est_cwmae1[keep], est_mle2))
  estimators        <-
    tibble::tibble(tau                    = rep(tau, 9),
                   estimator              =
                     rep(c("MAE1", "MAE2", "CMLE", "CMUE", "CUMVUE", "MLE",
                           "MUE", "UMVUE", "CWMAE"), each = len_tau),
                   S1                     = rep(S1, 9),
                   `E(hat(tau)|tau,1)`    = as.vector(E11),
                   `E(hat(tau)^2|tau,1)`  = as.vector(E12),
                   `E(hat(tau)|tau,2)`    = as.vector(E21),
                   `E(hat(tau)^2|tau,2)`  = as.vector(E22),
                   `Var(hat(tau)|tau,1)`  =
                     `E(hat(tau)^2|tau,1)` - `E(hat(tau)|tau,1)`^2,
                   `Var(hat(tau)|tau,2)`  =
                     `E(hat(tau)^2|tau,2)` - `E(hat(tau)|tau,2)`^2,
                   `Bias(hat(tau)|tau,1)` = `E(hat(tau)|tau,1)` - tau,
                   `Bias(hat(tau)|tau,2)` = `E(hat(tau)|tau,2)` - tau,
                   `RMSE(hat(tau)|tau,1)` =
                     sqrt(`Var(hat(tau)|tau,1)` +
                            `Bias(hat(tau)|tau,1)`^2),
                   `RMSE(hat(tau)|tau,2)` =
                     sqrt(`Var(hat(tau)|tau,2)` +
                            `Bias(hat(tau)|tau,2)`^2),
                   `Bias(hat(tau)|tau)`   =
                     S1*`Bias(hat(tau)|tau,1)` +
                     (1 - S1)*`Bias(hat(tau)|tau,2)`,
                   `RMSE(hat(tau)|tau)`   =
                     S1*`RMSE(hat(tau)|tau,1)` +
                     (1 - S1)*`RMSE(hat(tau)|tau,2)`)
  list(estimates = estimates, estimators = estimators)
}

eval_C_hp_wt          <- function(C, alpha, shape, Delta, CovZ) {
  J   <- nrow(CovZ)
  if (shape == "haybittle_peto") {
    e <- c(rep(3, J - 1), C)
  } else {
    e <- C*((1:J)/J)^(Delta - 0.5)
  }
  f   <- c(-e[1:(J - 1)], C)
  alpha - power(0, e, f, numeric(J), CovZ)
}

eval_C_pf             <- function(C, J, alpha, beta, delta, CovZ, e_fac, f_fac,
                                  sqrt_I_fac, seq_j, seq_jm1) {
  means_H1    <- sqrt_I_fac*abs(sum(C))
  e           <- C[2]*e_fac
  f           <- means_H1 - C[1]*f_fac
  notP_H1     <- beta - stats::pnorm(f[1], means_H1[1])[1] -
    pbvnorm(c(f[seq_jm1[[2]]], -Inf), c(e[seq_jm1[[2]]], f[2]),
            means_H1[seq_j[[2]]], CovZ[seq_j[[2]], seq_j[[2]]])
  P_H0        <- alpha - (1 - stats::pnorm(e[1])[1]) -
    pbvnorm(c(f[seq_jm1[[2]]], e[2]), c(e[seq_jm1[[2]]], Inf), numeric(2),
            CovZ[seq_j[[2]], seq_j[[2]]])
  if (J > 2) {
    for (j in 3:J) {
      notP_H1 <- notP_H1 - mvtnorm::pmvnorm(c(f[seq_jm1[[j]]], -Inf),
                                            c(e[seq_jm1[[j]]], f[j]),
                                            means_H1[seq_j[[j]]],
                                            sigma = CovZ[seq_j[[j]],
                                                         seq_j[[j]]])[1]
      P_H0    <- P_H0 - mvtnorm::pmvnorm(c(f[seq_jm1[[j]]], e[j]),
                                         c(e[seq_jm1[[j]]], Inf),
                                         sigma = CovZ[seq_j[[j]],
                                                      seq_j[[j]]])[1]
    }
  }
  P_H0^2 + notP_H1^2
}

eval_Delta_pf         <- function(Delta, J, alpha, beta, delta, sigma0, sigma1,
                                  ratio, w, CovZ, b_fac, sqrt_I_fac, n_fac,
                                  seq_j, seq_jm1) {
  e_fac    <- b_fac^(Delta[1] - 0.5)
  f_fac    <- b_fac^(Delta[2] - 0.5)
  C        <- stats::optim(par        = c(0.5, 0.5),
                           fn         = eval_C_pf,
                           control    = list(abstol = 1e-7),
                           J          = J,
                           alpha      = alpha,
                           beta       = beta,
                           delta      = delta,
                           CovZ       = CovZ,
                           e_fac      = e_fac,
                           f_fac      = f_fac,
                           sqrt_I_fac = sqrt_I_fac,
                           seq_j      = seq_j,
                           seq_jm1    = seq_jm1)$par
  sqrt_I   <- sqrt_I_fac*abs(sum(C))/delta
  e        <- C[2]*e_fac
  f        <- delta*sqrt_I - C[1]*f_fac
  n        <- sqrt_I^2*n_fac
  score    <- 0
  if (w[1] > 0) {
    score  <- score - w[1]*minus_ess(0, e, f, sqrt_I, CovZ, n)
  }
  if (w[2] > 0) {
    score  <- score - w[2]*minus_ess(delta, e, f, sqrt_I, CovZ, n)
  }
  if (w[3] > 0) {
    score  <- score - w[3]*stats::optim(par    = 0.5*delta,
                                        fn     = minus_ess,
                                        method = "Brent",
                                        lower  = 0,
                                        upper  = delta,
                                        e      = e,
                                        f      = f,
                                        sqrt_I = sqrt_I,
                                        CovZ   = CovZ,
                                        n      = n)$value
  }
  if (w[4] > 0) {
    score  <- score - w[4]*n[J]
  }
  score
}

eval_n0_hp_wt         <- function(n0, delta, sigma0, sigma1, ratio, CovZ, e, fu,
                                  power) {
  power - power(delta, e, fu,
                sqrt(information(n0, length(e), sigma0, sigma1, ratio)), CovZ)
}

f1z                   <- function(z, tau, sqrt_I1, f1, e1, log = FALSE) {
  f                         <- stats::dnorm(z, tau*sqrt_I1, log = log)
  f[which(z > f1 & z < e1)] <- 0
  f
}

f2z                   <- function(z, tau, sqrt_I1, sqrt_I2, f1, e1, sqrt_Iratio,
                                  fac1, fac2, log = FALSE) {
  if (!log) {
    exp(-0.5*(z - tau*sqrt_I2)^2)*(stats::pnorm(e1, z*sqrt_Iratio, fac1) -
                                     stats::pnorm(f1, z*sqrt_Iratio, fac1))/fac2
  } else {
    if (is.infinite(e1)) {
      -0.5*(z - tau*sqrt_I2)^2 +
        stats::pnorm((z*sqrt_Iratio - f1)/fac1, log.p = TRUE) - log(sqrt(2*pi))
    } else {
      -0.5*(z - tau*sqrt_I2)^2 +
        log((stats::pnorm(e1, z*sqrt_Iratio, fac1) -
               stats::pnorm(f1, z*sqrt_Iratio, fac1))) - log(sqrt(2*pi))
    }
  }

}

information           <- function(n0, J, sigma0, sigma1, ratio) {
  ((1:J)/J)*(sigma0^2/(n0*J) + sigma1^2/(ratio*n0*J))^-1
}

mae_root              <- function(tau, mle, sqrt_I1, f1, e1, fac3) {
  mle - tau - bias_mle(tau, sqrt_I1, f1, e1, fac3)
}

minus_clog_likelihood <- function(tau, k, z, sqrt_I1, sqrt_I2, f1, e1,
                                  sqrt_Iratio, fac1, fac2) {
  if (k == 1) {
    if (is.infinite(e1)) {
      -f1z(z, tau, sqrt_I1, f1, e1, log = TRUE) +
        stats::pnorm(f1, tau*sqrt_I1, log.p = TRUE)
    } else {
      -f1z(z, tau, sqrt_I1, f1, e1, log = TRUE) +
        log(1 - stats::pnorm(e1, tau*sqrt_I1) + stats::pnorm(f1, tau*sqrt_I1))
    }
  } else {
    if (is.infinite(e1)) {
      -f2z(z, tau, sqrt_I1, sqrt_I2, f1, e1, sqrt_Iratio, fac1, fac2,
          log = TRUE) +
        stats::pnorm(tau*sqrt_I1 - f1, log.p = TRUE)
    } else {
      -f2z(z, tau, sqrt_I1, sqrt_I2, f1, e1, sqrt_Iratio, fac1, fac2,
          log = TRUE) +
        log(stats::pnorm(e1, tau*sqrt_I1) - stats::pnorm(f1, tau*sqrt_I1))
    }
  }
}

minus_ess             <- function(tau, e, f, sqrt_I, CovZ, n) {
  J          <- length(e)
  means      <- tau*sqrt_I
  S          <- c(stats::pnorm(f[1], means[1]) +
                    (1 - stats::pnorm(e[1], means[1])),
                  numeric(J - 1))
  if (J > 2) {
    S[2]     <- pbvnorm(c(f[1], -Inf), c(e[1], f[2]), means[1:2],
                        CovZ[1:2, 1:2]) +
      pbvnorm(c(f[1], e[2]), c(e[1], Inf), means[1:2], CovZ[1:2, 1:2])
    if (J > 3) {
      for (j in 3:(J - 1)) {
        S[j] <-
          mvtnorm::pmvnorm(c(f[1:(j - 1)], -Inf), c(e[1:(j - 1)], f[j]),
                           means[1:j], sigma = CovZ[1:j, 1:j])[1] +
          mvtnorm::pmvnorm(c(f[1:(j - 1)], e[j]), c(e[1:(j - 1)], Inf),
                           means[1:j], sigma = CovZ[1:j, 1:j])[1]
      }
    }
  }
  S[J]       <- 1 - sum(S[1:(J - 1)])
  -sum(n*S)
}

mue_2_root            <- function(tau, z, sqrt_I1, sqrt_I, f1, e1, CovZ) {
  1 - stats::pnorm(e1, tau*sqrt_I1, 1) +
    mvtnorm::pmvnorm(c(f1, z), c(e1, Inf), tau*sqrt_I, sigma = CovZ)[1] - 0.5
}

opchar_fixed          <- function(tau, e, sqrt_I, n) {
  tibble::tibble(tau            = tau,
                 `P(tau)`       = 1 - stats::pnorm(e[1], tau*sqrt_I),
                 `ESS(tau)`     = n,
                 `SDSS(tau)`    = 0,
                 `MeSS(tau)`    = n,
                 `MoSS(tau)`    = n,
                 `E1(tau)`      = `P(tau)`,
                 `F1(tau)`      = 1 - `P(tau)`,
                 `S1(tau)`      = 1,
                 `cum{S1(tau)}` = 1,
                 `max(n)`       = n)
}

opchar_int            <- function(tau, e, f, sqrt_I, CovZ, n) {
  J                <- length(e)
  len_tau          <- length(tau)
  means            <- matrix(tau, ncol = 1)%*%matrix(sqrt_I, 1)
  opchar           <- matrix(0, len_tau, 7 + 4*J)
  E                <- Fu <- numeric(J)
  for (t in 1:len_tau) {
    Fu[1]          <- stats::pnorm(f[1], means[t, 1])
    E[1]           <- 1 - stats::pnorm(e[1], means[t, 1])
    if (J > 2) {
      for (j in 2:(J - 1)) {
        Fu[j]      <-
          mvtnorm::pmvnorm(c(f[1:(j - 1)], -Inf), c(e[1:(j - 1)], f[j]),
                           means[t, 1:j], sigma = CovZ[1:j, 1:j])[1]
        E[j]       <-
          mvtnorm::pmvnorm(c(f[1:(j - 1)], e[j]), c(e[1:(j - 1)], Inf),
                           means[t, 1:j], sigma = CovZ[1:j, 1:j])[1]
      }
    }
    E[J]           <-
      mvtnorm::pmvnorm(c(f[1:(J - 1)], e[J]), c(e[1:(J - 1)], Inf),
                       means[t, ], sigma = CovZ)[1]
    Fu[J]          <- 1 - sum(E) - sum(Fu[1:(J - 1)])
    cum_S          <- cumsum(S <- E + Fu)
    MeSS           <- ifelse(any(cum_S == 0.5),
                             0.5*(n[which(cum_S == 0.5)] +
                                    n[which(cum_S == 0.5) + 1]),
                             n[which(cum_S > 0.5)[1]])
    MoSS           <- mean(n[which(S == max(S))])
    opchar[t, ]    <- c(tau[t], sum(E), sum(n*S),
                        sqrt(sum(n^2*S) - sum(n*S)^2), MeSS, MoSS, E, Fu, S,
                        cum_S, n[J])
  }
  colnames(opchar) <- c("tau", "P(tau)", "ESS(tau)", "SDSS(tau)",
                        "MeSS(tau)", "MoSS(tau)",
                        paste(rep(c("E", "F", "S"), each = J), rep(1:J, 3),
                              "(tau)", sep = ""),
                        paste("cum{S", 1:J, "(tau)}", sep = ""), "max(n)")
  tibble::as_tibble(opchar)
}

optimal               <- function(par, J, alpha, beta, delta, sigma0, sigma1,
                                  ratio, w, penalty, CovZ) {
  n0        <- par[1]
  f         <- par[2:(J + 1)]
  e         <- c(par[2:J] + par[(J + 2):(2*J)], par[J + 1])
  sqrt_I    <- sqrt(information(n0, J, sigma0, sigma1, ratio))
  n         <- n0*(1:J)*(1 + ratio)
  opchar    <- opchar_int(c(0, delta), e, f, sqrt_I, CovZ, n)
  if (w[3] > 0) {
    max_ess <- stats::optim(par    = 0.5*delta,
                            fn     = minus_ess,
                            method = "Brent",
                            lower  = 0,
                            upper  = delta,
                            e      = e,
                            f      = f,
                            sqrt_I = sqrt_I,
                            CovZ   = CovZ,
                            n      = n)$par
  } else {
    max_ess <- 0
  }
  sum(w*c(opchar$`ESS(tau)`[1], opchar$`ESS(tau)`[2], max_ess, n[J])) +
    penalty*((alpha < opchar$`P(tau)`[1])*
               (opchar$`P(tau)`[1] - alpha)/alpha +
               (beta < 1 - opchar$`P(tau)`[2])*
               ((1 - opchar$`P(tau)`[2]) - beta)/beta)
}

pbvnorm               <- function(lower, upper, mean, sigma, sqrt_sigma, rho) {
  if (missing(sqrt_sigma)) {
    sqrt_sigma <- sqrt(c(sigma[1, 1], sigma[2, 2]))
  }
  if (missing(rho)) {
    rho        <- sigma[1, 2]/(sqrt_sigma[1]*sqrt_sigma[2])
  }
  x            <- (lower - mean)/c(sqrt_sigma[1], sqrt_sigma[2])
  y            <- (upper - mean)/c(sqrt_sigma[1], sqrt_sigma[2])
  if (all(is.infinite(y))) {
    I          <- 1
  } else if (is.infinite(y[1])) {
    I          <- stats::pnorm(y[2])
  } else if (is.infinite(y[2])) {
    I          <- stats::pnorm(y[1])
  } else {
    I          <- pbv::pbv_rcpp_pbvnorm(y[1], y[2], rho)
  }
  if (all(is.finite(x))) {
    I          <- I + pbv::pbv_rcpp_pbvnorm(x[1], x[2], rho)
  }
  if (is.finite(x[2])) {
    if (is.infinite(y[1])) {
      I        <- I - stats::pnorm(x[2])
    } else {
      I        <- I - pbv::pbv_rcpp_pbvnorm(y[1], x[2], rho)
    }
  }
  if (is.finite(x[1])) {
    if (is.infinite(y[2])) {
      I        <- I - stats::pnorm(x[1])
    } else {
      I        <- I - pbv::pbv_rcpp_pbvnorm(x[1], y[2], rho)
    }
  }
  I
}

power                 <- function(tau, e, f, sqrt_I, CovZ) {
  means <- tau*sqrt_I
  P     <- 1 - stats::pnorm(e[1], means[1])
  for (j in 2:length(e)) {
    P   <- P + mvtnorm::pmvnorm(c(f[1:(j - 1)], e[j]), c(e[1:(j - 1)], Inf),
                                means[1:j], sigma = CovZ[1:j, 1:j])[1]
  }
  P
}

sim_internal          <- function(des, tau, replicates, summary) {
  seq_J                <- 1:des$J
  n0_vec               <- c(0L, seq_J*des$n0)
  n1_vec               <- c(0L, seq_J*des$n1)
  sqrt_I               <- sqrt(des$I)
  Zj                   <- E <- Fu <- numeric(des$J)
  x0                   <- numeric(n0_vec[des$J + 1])
  x1                   <- numeric(n1_vec[des$J + 1])
  N                    <- numeric(replicates)
  for (i in 1:replicates) {
    sum_x0             <- sum_x1 <- 0
    for (j in seq_J) {
      range0           <- (1 + n0_vec[j]):n0_vec[j + 1]
      range1           <- (1 + n1_vec[j]):n1_vec[j + 1]
      x0[range0]       <- stats::rnorm(des$n0, sd = des$sigma0)
      x1[range1]       <- stats::rnorm(des$n1, tau, des$sigma1)
      sum_x0           <- sum_x0 + sum(x0[range0])
      sum_x1           <- sum_x1 + sum(x1[range1])
      Zj[j]            <-
        (sum_x1/n1_vec[j + 1] - sum_x0/n0_vec[j + 1])*sqrt_I[j]
      if (any(Zj[j] > des$e[j], Zj[j] <= des$f[j])) {
        N[i]           <- des$n[j]
        if (Zj[j] > des$e[j]) {
          E[j]         <- E[j] + 1L
        } else {
          Fu[j]        <- Fu[j] + 1L
        }
        break
      }
    }
    if (all(i%%ceiling(replicates/10) == 0, summary)) {
      message("...", ceiling(i/replicates), "% of simulations for tau = ", tau,
              " completed...")
    }
  }
  E                    <- E/replicates
  Fu                   <- Fu/replicates
  cum_S                <- cumsum(S <- E + Fu)
  P                    <- sum(E)
  ESS                  <- mean(N)
  SDSS                 <- sqrt(sum((N - ESS)^2)/(replicates - 1))
  sort_N               <- sort(N)
  table_N              <- table(N)
  MeSS                 <-
    0.5*(sort_N[ceiling(0.5*replicates)] + sort_N[ceiling(0.5*replicates + 1)])
  MoSS                 <- mean(des$n[which(table_N == max(table_N))])
  c(tau, P, ESS, SDSS, MeSS, MoSS, E, Fu, S, cum_S, des$n[des$J])
}

theme_OptGS           <- function(base_size = 11, base_family = "") {
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) +
    ggplot2::theme(axis.ticks       = ggplot2::element_line(colour = "grey70",
                                                            size   = 0.25),
                   complete         = TRUE,
                   legend.key       = ggplot2::element_rect(fill   = "white",
                                                            colour = NA),
                   legend.position  = "bottom",
                   legend.title     = ggplot2::element_blank(),

                   panel.background = ggplot2::element_rect(fill   = "white",
                                                            colour = NA),
                   panel.border     = ggplot2::element_rect(fill   = NA,
                                                            colour = "grey70",
                                                            size   = 0.5),
                   panel.grid.major = ggplot2::element_line(colour = "grey87",
                                                            size   = 0.25),
                   plot.margin      = ggplot2::unit(c(0.3, 0.5, 0.3, 0.3),
                                                    "cm"),
                   plot.title = ggplot2::element_text(hjust = 0.5),
                   panel.grid.minor = ggplot2::element_line(colour = "grey87",
                                                            size   = 0.125),
                   strip.background = ggplot2::element_rect(fill   = "grey70",
                                                            colour = NA),
                   strip.text       =
                     ggplot2::element_text(colour = "white",
                                           size   = ggplot2::rel(0.8)))
}

umvue_2               <- function(z, sqrt_I2, f1, e1, sqrt_Iratio, fac1, fac3) {
  est   <- z/sqrt_I2 - fac3*(stats::dnorm(e1, z*sqrt_Iratio, fac1) -
                               stats::dnorm(f1, z*sqrt_Iratio, fac1))/
                               (stats::pnorm(e1, z*sqrt_Iratio, fac1) -
                                  stats::pnorm(f1, z*sqrt_Iratio, fac1))
  if (is.infinite(est)) {
    est <- z/sqrt_I2
  }
  est
}

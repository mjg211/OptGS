an_internal  <- function(des, Zj, alpha, summary) {
  nrow_Zj        <- nrow(Zj)
  sqrt_I         <- sqrt(des$I)
  an             <- matrix(0, nrow_Zj, 10)
  for (i in 1:nrow_Zj) {
    j            <- Zj[i, 2]
    est_naive    <- Zj[i, 1]/sqrt_I[j]
    p_naive      <- 1 - stats::pnorm(Zj[i, 1])[1]
    lci_naive    <- est_naive - stats::qnorm(1 - alpha)/sqrt_I[j]
    if (j == 1) {
      p_adj      <- p_naive
      est_adj    <- stats::uniroot(f        = p_value,
                                   interval = c(-10000, 10000),
                                   J        = j,
                                   e        = Zj[i, 1],
                                   fu       = des$f[1],
                                   sqrt_I   = sqrt_I[1],
                                   CovZ     = 1,
                                   h        = 0.5)$root
      lci_adj    <- stats::uniroot(f        = p_value,
                                   interval = c(-10000, 10000),
                                   J        = 1,
                                   e        = Zj[i, 1],
                                   fu       = des$f[1],
                                   sqrt_I   = sqrt_I[1],
                                   CovZ     = 1,
                                   h        = alpha)$root
    } else {
      p_adj      <- p_adj + p_value(0, j, c(des$e[1:(j - 1)], Zj[i, 1]),
                                    des$f[1:j], sqrt_I[1:j], des$CovZ[1:j, 1:j])
      est_adj    <- stats::uniroot(f        = p_value,
                                   interval = c(-10000, 10000),
                                   J        = j,
                                   e        = c(des$e[1:(j - 1)], Zj[i, 1]),
                                   fu       = des$f[1:j],
                                   sqrt_I   = sqrt_I[1:j],
                                   CovZ     = des$CovZ[1:j, 1:j],
                                   h        = 0.5)$root
      lci_adj    <- stats::uniroot(f        = p_value,
                                   interval = c(-10000, 10000),
                                   J        = j,
                                   e        = c(des$e[1:(j - 1)], Zj[i, 1]),
                                   fu       = des$f[1:j],
                                   sqrt_I   = sqrt_I[1:j],
                                   CovZ     = des$CovZ[1:j, 1:j],
                                   h        = alpha)$root
    }
    an[i, ]      <- c(Zj[i, ], (Zj[i, 1] > des$e[Zj[i, 2]]), des$n[Zj[i, 2]],
                      est_naive, est_adj, p_naive, p_adj, lci_naive, lci_adj)
  }
  an             <- tibble::as_tibble(an)
  colnames(an)   <- c("Zj", "j", "Reject H0", "N", "EST_naive(Zj,j)",
                      "EST_adj(Zj,j)", "p_naive(Zj,j)", "p_adj(Zj,j)",
                      "LCI_naive(Zj,j)", "LCI_adj(Zj,j)")
  an$j           <- factor(an$j, 1:des$J)
  an$`Reject H0` <- as.logical(an$`Reject H0`)
  an
}


covariance    <- function(sqrt_I) {
  CovZ            <- diag(length(sqrt_I))
  for (k1 in seq_len(length(sqrt_I) - 1) + 1L) {
    vec           <- 1:(k1 - 1)
    CovZ[k1, vec] <- CovZ[vec, k1] <- sqrt_I[vec]/sqrt_I[k1]
  }
  CovZ
}

minus_ess     <- function(tau, e, f, sqrt_I, CovZ, n) {
  J        <- length(e)
  means    <- tau*sqrt_I
  S        <- c(stats::pnorm(f[1], mean = means[1]) +
                  stats::pnorm(e[1], mean = means[1], lower.tail = F),
                numeric(J - 1))
  if (J > 2) {
    for (j in 2:(J - 1)) {
      S[j] <-
        mvtnorm::pmvnorm(c(f[1:(j - 1)], -Inf), c(e[1:(j - 1)], f[j]),
                         means[1:j], sigma = CovZ[1:j, 1:j])[1] +
        mvtnorm::pmvnorm(c(f[1:(j - 1)], e[j]), c(e[1:(j - 1)], Inf),
                         means[1:j], sigma = CovZ[1:j, 1:j])[1]
    }
  }
  S[J]     <- 1 - sum(S[1:(J - 1)])
  -sum(n*S)
}

eval_C_hp_wt  <- function(C, alpha, shape, Delta, CovZ) {
  J   <- nrow(CovZ)
  if (shape == "haybittle_peto") {
    e <- c(rep(3, J - 1), C)
  } else {
    e <- C*((1:J)/J)^(Delta - 0.5)
  }
  f   <- c(-e[1:(J - 1)], C)
  alpha - power(0, e, f, numeric(J), CovZ)
}

eval_C_pf     <- function(C, J, alpha, beta, delta, CovZ, e_fac, f_fac,
                          sqrt_I_fac, seq_j, seq_jm1) {
  means_H1  <- sqrt_I_fac*abs(sum(C))
  e         <- C[2]*e_fac
  f         <- means_H1 - C[1]*f_fac
  notP_H1   <- beta - stats::pnorm(f[1], mean = means_H1[1])[1]
  P_H0      <- alpha - stats::pnorm(e[1], lower.tail = F)[1]
  for (j in 2:J) {
    notP_H1 <- notP_H1 - mvtnorm::pmvnorm(c(f[seq_jm1[[j]]], -Inf),
                                          c(e[seq_jm1[[j]]], f[j]),
                                          means_H1[seq_j[[j]]],
                                          sigma = CovZ[seq_j[[j]],
                                                       seq_j[[j]]])[1]
    P_H0    <- P_H0 - mvtnorm::pmvnorm(c(f[seq_jm1[[j]]], e[j]),
                                       c(e[seq_jm1[[j]]], Inf),
                                       sigma = CovZ[seq_j[[j]], seq_j[[j]]])[1]
  }
  P_H0^2 + notP_H1^2
}

eval_Delta_pf <- function(Delta, J, alpha, beta, delta, sigma0, sigma1, ratio,
                          w, CovZ, b_fac, sqrt_I_fac, n_fac, seq_j, seq_jm1) {
  e_fac    <- b_fac^(Delta[1] - 0.5)
  f_fac    <- b_fac^(Delta[2] - 0.5)
  C        <- optim(par        = c(0.5, 0.5),
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
    score  <- score + w[4]*n[J]
  }
  score
}

eval_n0_hp_wt <- function(n0, delta, sigma0, sigma1, ratio, CovZ, e, fu,
                          power) {
  power - power(delta, e, fu,
                sqrt(information(n0, length(e), sigma0, sigma1, ratio)), CovZ)
}

information   <- function(n0, J, sigma0, sigma1, ratio) {
  ((1:J)/J)*(sigma0^2/(n0*J) + sigma1^2/(ratio*n0*J))^-1
}

opchar_int    <- function(tau, e, f, sqrt_I, CovZ, n) {
  J                <- length(e)
  len_tau          <- length(tau)
  means            <- matrix(tau, ncol = 1)%*%matrix(sqrt_I, 1)
  opchar           <- matrix(0, len_tau, 6 + 4*J)
  E                <- Fu <- numeric(J)
  for (t in 1:len_tau) {
    Fu[1]          <- stats::pnorm(f[1], mean = means[t, 1])
    E[1]           <- stats::pnorm(e[1], mean = means[t, 1], lower.tail = F)
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
    MSS            <- ifelse(any(cum_S == 0.5),
                             0.5*(n[which(cum_S == 0.5)] +
                                    n[which(cum_S == 0.5) + 1]),
                             n[which(cum_S > 0.5)[1]])
    opchar[t, ]    <- c(tau[t], sum(E), sum(n*S),
                        sqrt(sum(n^2*S) - sum(n*S)^2), MSS, E, Fu, S, cum_S,
                        n[J])
  }
  colnames(opchar) <- c("tau", "P(tau)", "ESS(tau)", "SDSS(tau)",
                        "MSS(tau)", paste(rep(c("E", "F", "S"), each = J),
                                          rep(1:J, 3), "(tau)", sep = ""),
                        paste("cum{S", 1:J, "(tau)}", sep = ""), "max(n)")
  tibble::as_tibble(opchar)
}

optimal       <- function(par, J, alpha, beta, delta, sigma0, sigma1, ratio, w,
                          penalty, CovZ) {
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

p_value  <- function(theta, J, e, fu, sqrt_I, CovZ, h = 0) {
  theta_sqrt_I <- theta*sqrt_I
  P            <- stats::pnorm(e[1], theta_sqrt_I[1], lower.tail = F)
  if (J > 1) {
    for (j in 2:J) {
      P        <- P + mvtnorm::pmvnorm(c(fu[1:(j - 1)], e[j]),
                                       c(e[1:(j - 1)], Inf),
                                       theta_sqrt_I[1:j],
                                       sigma = CovZ[1:j, 1:j])
    }
  }
  P - h
}

power         <- function(tau, e, f, sqrt_I, CovZ) {
  means <- tau*sqrt_I
  P     <- stats::pnorm(e[1], mean = means[1], lower.tail = F)
  for (j in 2:length(e)) {
    P   <- P + mvtnorm::pmvnorm(c(f[1:(j - 1)], e[j]), c(e[1:(j - 1)], Inf),
                                means[1:j], sigma = CovZ[1:j, 1:j])[1]
  }
  P
}

sim_internal  <- function(des, tau, adjusted, alpha, replicates,
                          summary) {
  seq_J                <- 1:des$J
  n0_vec               <- c(0L, seq_J*des$n0)
  n1_vec               <- c(0L, seq_J*des$n1)
  sqrt_I               <- sqrt(des$I)
  P                    <- cov_naive <- 0L
  p_naive              <- 0
  est_naive            <- N <- numeric(replicates)
  if (adjusted) {
    cov_adj            <- 0L
    p_adj              <- 0
    est_adj            <- est_naive
  } else {
    bias_adj           <- cov_adj <- est_adj <- p_adj <- rmse_adj <- NA
  }
  Zj                   <- numeric(des$J)
  x0                   <- numeric(n0_vec[des$J + 1])
  x1                   <- numeric(n1_vec[des$J + 1])
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
          P            <- P + 1L
        }
        est_naive[i]   <- Zj[j]/sqrt_I[j]
        p_naive        <- p_naive + (1 - stats::pnorm(Zj[j])[1])
        lci_naive      <- est_naive[i] - stats::qnorm(1 - alpha)/sqrt_I[j]
        cov_naive      <- cov_naive + (lci_naive <= tau)
        if (adjusted) {
          if (j == 1) {
            p_adj      <- p_adj + (1 - stats::pnorm(Zj[j])[1])
            est_adj[i] <- stats::uniroot(f        = p_value,
                                         interval = c(-10000, 10000),
                                         J        = j,
                                         e        = Zj[j],
                                         fu       = des$f[1],
                                         sqrt_I   = sqrt_I[1:j],
                                         CovZ     = des$CovZ[1:j, 1:j],
                                         h        = 0.5)$root
            lci_adj    <- stats::uniroot(f        = p_value,
                                         interval = c(-10000, 10000),
                                         J        = j,
                                         e        = Zj[j],
                                         fu       = des$f[1],
                                         sqrt_I   = sqrt_I[1:j],
                                         CovZ     = des$CovZ[1:j, 1:j],
                                         h        = alpha)$root
          } else {
            p_adj      <- p_adj + p_value(0, j, c(des$e[1:(j - 1)], Zj[j]),
                                          des$f[1:j], sqrt_I[1:j],
                                          des$CovZ[1:j, 1:j])
            est_adj[i] <- stats::uniroot(f        = p_value,
                                         interval = c(-10000, 10000),
                                         J        = j,
                                         e        = c(des$e[1:(j - 1)], Zj[j]),
                                         fu       = des$f[1:j],
                                         sqrt_I   = sqrt_I[1:j],
                                         CovZ     = des$CovZ[1:j, 1:j],
                                         h        = 0.5)$root
            lci_adj    <- stats::uniroot(f        = p_value,
                                         interval = c(-10000, 10000),
                                         J        = j,
                                         e        = c(des$e[1:(j - 1)], Zj[j]),
                                         fu       = des$f[1:j],
                                         sqrt_I   = sqrt_I[1:j],
                                         CovZ     = des$CovZ[1:j, 1:j],
                                         h        = alpha)$root
          }
          cov_adj      <- cov_adj + (lci_adj <= tau)
        }
        break
      }
    }
    if (all(adjusted, i%%ceiling(replicates/10) == 0, summary)) {
      message("...", ceiling(i/replicates), "% of simulations for tau = ", tau,
              " completed...")
    }
  }
  P                    <- P/replicates
  ESS                  <- mean(N)
  SDSS                 <- sqrt(sum((N - ESS)^2)/(replicates - 1))
  sort_N               <- sort(N)
  MSS                  <-
    0.5*(sort_N[ceiling(0.5*replicates)] + sort_N[ceiling(0.5*replicates + 1)])
  est_naive            <- mean(est_naive)
  bias_naive           <- est_naive - tau
  rmse_naive           <- sqrt(sum((est_naive - tau)^2)/replicates)
  cov_naive            <- cov_naive/replicates
  p_naive              <- p_naive/replicates
  if (adjusted) {
    est_adj            <- sum(est_adj)/replicates
    bias_adj           <- est_adj - tau
    rmse_adj           <- sqrt(sum((est_adj - tau)^2)/replicates)
    cov_adj            <- cov_adj/replicates
    p_adj              <- p_adj/replicates
  }
  c(tau, P, ESS, SDSS, MSS, est_naive, est_adj, bias_naive, bias_adj,
    rmse_naive, rmse_adj, p_naive, p_adj, cov_naive, cov_adj)
}

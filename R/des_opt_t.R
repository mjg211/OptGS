des_opt_t <- function(J = 2, alpha = 0.05, beta = 0.2, delta = 0.2, sigma0 = 1,
                      sigma1 = sigma0, ratio = 1, w = c(1, 0, 0),
                      replicates = 1e5, seed = Sys.time(), summary = F) {

  set.seed(seed)

  ##### Check input variables ##################################################



  ##### Perform main computations ##############################################

  penalty                 <-
    (1 + ratio)*((stats::qnorm(1 - alpha)*sigma0*sqrt(1 + 1/ratio) +
                    stats::qnorm(1 - beta)*
                    sqrt(sigma0^2 + sigma1^2/ratio))/delta)^2
  poss_n0                 <- 2:ceiling(J*penalty/(1 + ratio))
  poss_n0                 <- poss_n0[which(poss_n0*ratio%%1 == 0)]
  if (length(poss_n0) %in% 0:1) {

  }
  seq_J                   <- 1:J
  poss_n0_J               <- unique(as.vector(poss_n0%*%t(seq_J)))
  num_n0                  <- length(poss_n0)
  n0_max                  <- poss_n0[num_n0]
  n0_max_J                <- n0_max*J
  sim_data0               <- matrix(stats::rnorm(replicates*n0_max_J,
                                                 sd = sigma0),
                                    replicates, n0_max_J)
  sim_data1               <- matrix(stats::rnorm(replicates*n0_max_J*ratio,
                                                 sd = sigma1),
                                    replicates, n0_max_J*ratio)
  data_tau_hat            <- data_sqrt_I_hat <- matrix(0, replicates, n0_max_J)
  for (i in 1:replicates) {
    cumsum_data0             <- cumsum(sim_data0[i, ])
    cumsum_data1             <- cumsum(sim_data1[i, ])
    cumsum_data0_sq          <- cumsum(sim_data0[i, ]^2)
    cumsum_data1_sq          <- cumsum(sim_data1[i, ]^2)
    for (n0 in poss_n0_J) {
      n1                     <- n0*ratio
      data_tau_hat[i, n0]    <- cumsum_data1[n1]/n1 - cumsum_data0[n0]/n0
      data_sqrt_I_hat[i, n0] <-
        1/sqrt((cumsum_data0_sq[n0] - cumsum_data0[n0]^2/n0)/(n0*(n0 - 1)) +
                 (cumsum_data1_sq[n1] - cumsum_data1[n1]^2/n1)/(n1*(n1 - 1)))
    }
    print(i)
  }
  remove(sim_data0, sim_data1)
  twoJm1                     <- 2L*J - 1L
  contMean                   <- c(rep(-1L, J - 1), rep(2L, J))
  contSD                     <- rep(5, twoJm1)
  contConstVec               <- numeric(twoJm1)
  contConstMat               <- matrix(0L, twoJm1, twoJm1)
  for (j in (J + 1):twoJm1) {
    contConstMat[j, j]       <- -1L
  }
  discCat                    <- poss_n0
  discSmooth                 <- 0.5
  discProbs                  <- stats::dnorm(poss_n0,
                                             1.25*penalty/(1 + ratio), 5)
  discProbs                  <- list(discProbs/sum(discProbs))

  CEoptim                    <-
    CEoptim::CEoptim(f        = optimal_t,
                     f.arg    = list(J               = J,
                                     alpha           = alpha,
                                     beta            = beta,
                                     delta           = delta,
                                     ratio           = ratio,
                                     w               = w,
                                     replicates      = replicates,
                                     penalty         = penalty,
                                     data_tau_hat    = data_tau_hat,
                                     data_sqrt_I_hat = data_sqrt_I_hat,
                                     poss_n0         = poss_n0),
                     continuous = list(mean       = contMean,
                                       sd         = contSD,
                                       conMat     = contConstMat,
                                       conVec     = contConstVec,
                                       smoothMean = 0.5,
                                       smoothSd   = 0.5),
                     discrete   = list(categories = length(discCat),
                                       smoothProb = discSmooth,
                                       probs      = discProbs),
                     N          = as.integer(600*J),
                     rho        = 0.05,
                     verbose    = T)


  score <- optimal.design$optimum
  n     <- optimal.design$optimizer$discrete + 2
  if (stopping == "A&R"){
    a <- optimal.design$optimizer$continuous[1:L]
    r <- c(optimal.design$optimizer$continuous[(L + 1):(2*L - 1)],
           optimal.design$optimizer$continuous[L])
    return(list(n = n, score = score, a = a, r = r, sim.data.mean.diff = sim.data.mean.diff,
                sim.data.sum.sq = sim.data.sum.sq, optimal.design = optimal.design))
  } else if (stopping == "A"){
    a <- optimal.design$optimizer$continuous[1:L]
    return(list(n = n, score = score, a = a, sim.data.mean.diff = sim.data.mean.diff,
                sim.data.sum.sq = sim.data.sum.sq, optimal.design = optimal.design))
  } else if (stopping == "R"){
    r <- optimal.design$optimizer$continuous[1:L]
    return(list(n = n, score = score, r = r, sim.data.mean.diff = sim.data.mean.diff,
                sim.data.sum.sq = sim.data.sum.sq, optimal.design = optimal.design))
  }

}

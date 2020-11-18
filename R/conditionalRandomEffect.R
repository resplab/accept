
ExacHistAdj_Weib <- function(n_sample = 100,
                             RE_var_mat = matrix(c(0.6854, 0.08782, 0.08782, 2.2491), ncol = 2),
                             PRED_RATE_PLAC_RE = "PRED_RATE_ALL_RE",
                             PRED_SPROB_ALL_RE = "PRED_SPROB_ALL_RE",
                             Hist_obsExac = "Hist_obsExac",
                             Hist_obsExac_Sev = "Hist_obsExac_Sev",
                             Data) {
  RE_seq_1 <- seq(from = -2 * RE_var_mat[1, 1], to = 2 * RE_var_mat[1, 1], length.out = n_sample)
  RE_seq_2 <- seq(from = -2 * RE_var_mat[2, 2], to = 2 * RE_var_mat[2, 2], length.out = n_sample)
  RE_W_mat <- outer(X = RE_seq_1, Y = RE_seq_2, FUN = Vectorize(function(x, y) dmvnorm(c(x, y), sigma = RE_var_mat)))
  Lambda <- (matrix(Data[ , PRED_RATE_PLAC_RE] ^ 1 / 0.97, ncol = 1) %*% matrix(exp(RE_seq_1), nrow = 1)) ^ 0.97
  ProbSev <- matrix(Data[ , PRED_SPROB_ALL_RE] / (1 - Data[ , PRED_SPROB_ALL_RE]), ncol = 1) %*% matrix(exp(RE_seq_2), nrow = 1)
  ProbSev <- ProbSev / (1 + ProbSev)
  Lambda_Sev <- lapply(c(1 : nrow(Data)), function(x) matrix(Lambda[x, ], ncol = 1) %*% matrix(ProbSev[x, ], nrow = 1))
  Lambda_non_Sev <- lapply(c(1 : nrow(Data)), function(x) matrix(Lambda[x, ], ncol = 1) %*% matrix(1 - ProbSev[x, ], nrow = 1))
  Posterior_Sev_W <-
    lapply(c(1 : nrow(Data)), function(x) {
      t(apply(Lambda_Sev[[x]], 1, function(y) dpois(x = Data[x , Hist_obsExac_Sev], lambda = y))) * RE_W_mat
    })
  Posterior_non_Sev_W <-
    lapply(c(1 : nrow(Data)), function(x) {
      t(apply(Lambda_non_Sev[[x]], 1, function(y) dpois(x = Data[x , Hist_obsExac] - Data[x , Hist_obsExac_Sev], lambda = y))) * RE_W_mat
    })
  Rate_Sev_Adj <- sapply(c(1 : nrow(Data)), function(x) weighted.mean(x = Lambda_Sev[[x]], w = Posterior_Sev_W[[x]]))
  Rate_non_Sev_Adj <- sapply(c(1 : nrow(Data)), function(x) weighted.mean(x = Lambda_non_Sev[[x]], w = Posterior_non_Sev_W[[x]]))
  Rate_Sev_SD_Adj <- sqrt(Rate_Sev_Adj + Rate_Sev_Adj ^ 2 * (exp(0.97 * RE_var_mat[1, 1]) - 1))
  Rate_non_Sev_SD_Adj <- sqrt(Rate_non_Sev_Adj + Rate_non_Sev_Adj ^ 2 * (exp(0.97 * RE_var_mat[1, 1]) - 1))
  Rate_Adj <- Rate_Sev_Adj + Rate_non_Sev_Adj
  Rate_SD_Adj <- sqrt(Rate_Adj + Rate_Adj ^ 2 * (exp(0.97 * RE_var_mat[1, 1]) - 1))
  return(list(Rate_Adj = Rate_Adj, Rate_SD_Adj = Rate_SD_Adj,
              Rate_Sev_Adj = Rate_Sev_Adj, Rate_Sev_SD_Adj = Rate_Sev_SD_Adj,
              Rate_non_Sev_Adj = Rate_non_Sev_Adj, Rate_non_Sev_SD_Adj = Rate_non_Sev_SD_Adj))
}


densityLastYrExacNew <- function (patientData, random_distribution_iteration = 2e4, lastYrExacCol = "LastYrExacCount", lastYrSevExacCol = "LastYrSevExacCount") {

  conditionalRandEffect <- list()

  RE_seq_1 = seq(from = -2 * covMat[1, 1], to = 2 * covMat[1, 1], length.out = random_sampling_N)
  RE_seq_2 = seq(from = -2 * covMat[2, 2], to = 2 * covMat[2, 2], length.out = random_sampling_N)
  RE_W_mat <- outer(X = RE_seq_1, Y = RE_seq_2, FUN = Vectorize(function(x, y) mvtnorm::dmvnorm(c(x, y), sigma = covMat)))


  Lambda  <- as.matrix(patientData[, "log_alpha"], ncol = 1) %*% matrix(exp(RE_seq_1), nrow = 1)
  ProbSev <- as.matrix(patientData[ , "c_lin"], ncol = 1) %*% matrix(exp(RE_seq_2), nrow = 1)
  ProbSev <- ProbSev / (1 + ProbSev)

  Lambda_Sev <- lapply(c(1 : nrow(patientData)), function(x) matrix(Lambda[x, ], ncol = 1) %*% matrix(ProbSev[x, ], nrow = 1))
  Lambda_non_Sev <- lapply(c(1 : nrow(patientData)), function(x) matrix(Lambda[x, ], ncol = 1) %*% matrix(1 - ProbSev[x, ], nrow = 1))

  Posterior_Sev_W <-
    lapply(c(1 : nrow(patientData)), function(x) {
      t(apply(Lambda_Sev[[x]], 1, function(y) dpois(x = patientData[x, lastYrSevExacCol], lambda = y))) * RE_W_mat
    })

  Posterior_non_Sev_W <-
    lapply(c(1 : nrow(patientData)), function(x) {
      t(apply(Lambda_non_Sev[[x]], 1, function(y) dpois(x = patientData[x, lastYrExacCol] - patientData[x , lastYrSevExacCol], lambda = y))) * RE_W_mat
    })

  conditionalZ <- matrix(0, nrow = random_distribution_iteration, ncol = 3)
  colnames(conditionalZ) <- c("weight", "z1", "z2")

  conditionalZ[j, "z1"] <- z1
  conditionalZ[j, "z2"] <- z2
  conditionalZ[j, "weight"] <- lastYrExacProb*lastYrSevExacProb

  ID <- as.character(patientData[i, "ID"])
  conditionalRandEffect[[ID]] <- as.data.frame(conditionalZ)




#
#
#
#   for (i in 1:(nrow(patientData)))
#   {
#     log_alpha <-   b0 +
#       b_male * patientData[i, "male"] +
#       b_age * patientData[i, "age"] +
#       b_nowsmk * patientData[i, "smoker"] +
#       b_oxygen * patientData[i, "oxygen"] +
#       b_fev1 * patientData[i, "FEV1"] +
#       b_SGRQ * patientData[i, "SGRQ"] +
#       b_cardiovascular * patientData[i, "statin"] +
#       b_randomized_azithromycin * patientData[i, "randomized_azithromycin"] +
#       b_LAMA * patientData[i, "LAMA"] +
#       b_LABA * patientData[i, "LABA"] +
#       b_ICS * patientData[i, "ICS"] +
#       b_randomized_LAMA * patientData[i, "randomized_LAMA"] +
#       b_randomized_LABA * patientData[i, "randomized_LABA"] +
#       b_randomized_ICS * patientData[i, "randomized_ICS"] +
#       b_randomized_statin * patientData[i, "randomized_statin"] +
#       b_BMI * patientData[i, "BMI"]
#
#     c_lin <-   c0 +
#       c_male * patientData[i, "male"] +
#       c_age * patientData[i, "age"] +
#       c_nowsmk * patientData[i, "smoker"] +
#       c_oxygen * patientData[i, "oxygen"] +
#       c_fev1 * patientData[i, "FEV1"] +
#       c_SGRQ * patientData[i, "SGRQ"] +
#       c_cardiovascular * patientData[i, "statin"] +
#       c_randomized_azithromycin * patientData[i, "randomized_azithromycin"] +
#       c_LAMA * patientData[i, "LAMA"] +
#       c_LABA * patientData[i, "LABA"] +
#       c_ICS * patientData[i, "ICS"] +
#       c_randomized_LAMA * patientData[i, "randomized_LAMA"] +
#       c_randomized_LABA * patientData[i, "randomized_LABA"] +
#       c_randomized_ICS * patientData[i, "randomized_ICS"] +
#       c_randomized_statin * patientData[i, "randomized_statin"] +
#       c_BMI * patientData[i, "BMI"]
#
#     conditionalZ <- matrix(0, nrow = random_distribution_iteration, ncol = 3)
#     colnames(conditionalZ) <- c("weight", "z1", "z2")
#
#     for (j in 1:random_distribution_iteration){
#
#       z <- MASS::mvrnorm(1, c(0, 0), covMat)
#       z1 <- z[1]
#       z2 <- z[2]
#
#       alpha <- exp (as.numeric(log_alpha) + z1)
#       lambda <- as.numeric(alpha ^ gamma)
#
#       obsLastYrExacCount <- as.numeric(patientData[i, lastYrExacCol])
#       lastYrExacProb <-  dpois(obsLastYrExacCount, lambda)
#
#       OR <- exp (as.numeric(c_lin) + z2)
#       pSevere <- (OR/(1+OR))
#       rateSevere <- as.numeric(pSevere * lambda)
#
#       obsLastYrSevExacCount <- as.numeric(patientData[i, lastYrSevExacCol])
#       lastYrSevExacProb <-  dpois(obsLastYrSevExacCount, rateSevere)
#
#       conditionalZ[j, "z1"] <- z1
#       conditionalZ[j, "z2"] <- z2
#       conditionalZ[j, "weight"] <- lastYrExacProb*lastYrSevExacProb
#     }
#     ID <- as.character(patientData[i, "ID"])
#     conditionalRandEffect[[ID]] <- as.data.frame(conditionalZ)
#   }

  return(conditionalRandEffect)
}

densityLastYrExac <- function (patientData, random_distribution_iteration = 2e4, lastYrExacCol = "LastYrExacCount", lastYrSevExacCol = "LastYrSevExacCount") {

  conditionalRandEffect <- list()
  for (i in 1:(nrow(patientData)))
  {
    log_alpha <-   b0 +
      b_male * patientData[i, "male"] +
      b_age * patientData[i, "age"] +
      b_nowsmk * patientData[i, "smoker"] +
      b_oxygen * patientData[i, "oxygen"] +
      b_fev1 * patientData[i, "FEV1"] +
      b_SGRQ * patientData[i, "SGRQ"] +
      b_cardiovascular * patientData[i, "statin"] +
      b_randomized_azithromycin * patientData[i, "randomized_azithromycin"] +
      b_LAMA * patientData[i, "LAMA"] +
      b_LABA * patientData[i, "LABA"] +
      b_ICS * patientData[i, "ICS"] +
      b_randomized_LAMA * patientData[i, "randomized_LAMA"] +
      b_randomized_LABA * patientData[i, "randomized_LABA"] +
      b_randomized_ICS * patientData[i, "randomized_ICS"] +
      b_randomized_statin * patientData[i, "randomized_statin"] +
      b_BMI * patientData[i, "BMI"]

    c_lin <-   c0 +
      c_male * patientData[i, "male"] +
      c_age * patientData[i, "age"] +
      c_nowsmk * patientData[i, "smoker"] +
      c_oxygen * patientData[i, "oxygen"] +
      c_fev1 * patientData[i, "FEV1"] +
      c_SGRQ * patientData[i, "SGRQ"] +
      c_cardiovascular * patientData[i, "statin"] +
      c_randomized_azithromycin * patientData[i, "randomized_azithromycin"] +
      c_LAMA * patientData[i, "LAMA"] +
      c_LABA * patientData[i, "LABA"] +
      c_ICS * patientData[i, "ICS"] +
      c_randomized_LAMA * patientData[i, "randomized_LAMA"] +
      c_randomized_LABA * patientData[i, "randomized_LABA"] +
      c_randomized_ICS * patientData[i, "randomized_ICS"] +
      c_randomized_statin * patientData[i, "randomized_statin"] +
      c_BMI * patientData[i, "BMI"]

    conditionalZ <- matrix(0, nrow = random_distribution_iteration, ncol = 3)
    colnames(conditionalZ) <- c("weight", "z1", "z2")

    for (j in 1:random_distribution_iteration){

      z <- MASS::mvrnorm(1, c(0, 0), covMat)
      z1 <- z[1]
      z2 <- z[2]

      alpha <- exp (as.numeric(log_alpha) + z1)
      lambda <- as.numeric(alpha ^ gamma)

      obsLastYrExacCount <- as.numeric(patientData[i, lastYrExacCol])
      lastYrExacProb <-  dpois(obsLastYrExacCount, lambda)

      OR <- exp (as.numeric(c_lin) + z2)
      pSevere <- (OR/(1+OR))
      rateSevere <- as.numeric(pSevere * lambda)

      obsLastYrSevExacCount <- as.numeric(patientData[i, lastYrSevExacCol])
      lastYrSevExacProb <-  dpois(obsLastYrSevExacCount, rateSevere)

      conditionalZ[j, "z1"] <- z1
      conditionalZ[j, "z2"] <- z2
      conditionalZ[j, "weight"] <- lastYrExacProb*lastYrSevExacProb
    }
    ID <- as.character(patientData[i, "ID"])
    conditionalRandEffect[[ID]] <- as.data.frame(conditionalZ)
  }

  return(conditionalRandEffect)
}

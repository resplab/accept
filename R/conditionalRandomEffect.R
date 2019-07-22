
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

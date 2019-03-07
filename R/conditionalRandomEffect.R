densityLastYrExac <- function (patientData, random_sampling_N = 1000, lastYrExacCol = "LastYrExacCount", lastYrSevExacCol = "LastYrSevExacCount") {

  conditionalRandEffect <- list()
  for (i in 1:(nrow(patientData)))

  {
    log_alpha <-   b0 +
      b_male * patientData[i, "male"] +
      b_age10 * patientData[i, "age10"] +
      b_smoker * patientData[i, "smoker"] +
      b_oxygen * patientData[i, "oxygen"] +
      b_fev1 * patientData[i, "FEV1"] +
      b_sgrq10 * patientData[i, "sgrq10"] +
      b_cardiovascular * patientData[i, "statin"] +
      b_azithromycin * patientData[i, "azithromycin"] +
      b_LAMA * patientData[i, "LAMA"] +
      b_LABA * patientData[i, "LABA"] +
      b_ICS * patientData[i, "ICS"] +
      b_BMI10 * patientData[i, "BMI10"]


    c_lin <-   c0 +
      c_male * patientData[i, "male"] +
      c_age10 * patientData[i, "age10"] +
      c_smoker * patientData[i, "smoker"] +
      c_oxygen * patientData[i, "oxygen"] +
      c_fev1 * patientData[i, "FEV1"] +
      c_sgrq10 * patientData[i, "sgrq10"] +
      c_cardiovascular * patientData[i, "statin"] +
      c_azithromycin * patientData[i, "azithromycin"] +
      c_LAMA * patientData[i, "LAMA"] +
      c_LABA * patientData[i, "LABA"] +
      c_ICS * patientData[i, "ICS"] +
      c_BMI10 * patientData[i, "BMI10"]

    conditionalZ <- matrix(0, nrow = random_sampling_N, ncol = 3)
    colnames(conditionalZ) <- c("weight", "z1", "z2")


    for (j in 1:random_sampling_N){

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

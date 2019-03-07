
gamma	               <- 0.9694
b0                   <-	0.05689
b_male               <-	-0.08636
b_age10	             <- -0.02341
b_nowsmk             <-	-0.2002
b_oxygen             <-	0.03565
b_fev1               <-	-0.206
b_sgrq10             <-	0.1056
b_cardiovascular     <-	0.1135
b_azithromycin       <-	-0.1637
b_LAMA               <-	0.1546
b_LABA               <-	0.09876
b_ICS                <-	0.1486
b_BMI10              <-	-0.1107



c0                  <-   -3.4448
c_male              <- 0.5553
c_age10             <- 0.06462
c_nowsmk            <- 0.4077
c_oxygen            <- 0.5163
c_fev1              <- -0.5279
c_sgrq10            <- 0.2058
c_cardiovascular    <- 0.3316
c_azithromycin      <- -0.08741
c_LAMA              <- -0.2168
c_LABA              <- -0.03896
c_ICS               <- 0.1933
c_BMI10             <- -0.09053



v1  <-  0.6162
v2  <- 2.4016
cov <- 0.1558


covMat <- matrix(
  c(v1, cov, cov, v2),
  nrow = 2,
  ncol = 2
)

predictACCEPT <- function (patientData, conditionalZ, random_sampling_N = 1000){

  log_alpha <-   b0 +
    b_male * patientData["gender"] +
    b_age10 * patientData["age10"] +
    b_nowsmk * patientData["nowsmk"] +
    b_oxygen * patientData["oxygen"] +
    b_fev1 * patientData["FEV1"] +
    b_sgrq10 * patientData["sgrq10"] +
    b_cardiovascular * patientData["statin"] +
    b_azithromycin * patientData["azithromycin"] +
    b_LAMA * patientData["LAMA"] +
    b_LABA * patientData["LABA"] +
    b_ICS * patientData["ICS"] +
    b_BMI10 * patientData["BMI10"]

  z <- sample_n(conditionalZ, random_sampling_N, replace = TRUE, weight = weight)
  alpha <- exp (as.numeric(log_alpha) + z[, "z1"])
  lambda <- alpha ^ gamma

  predicted_exac_rate <- lambda
  mean_predicted_exac_rate <- mean (predicted_exac_rate)

  predicted_exac_probability <- 1 - exp(-lambda*(as.numeric(patientData[i, "follow_up"])^gamma))
  mean_predicted_exac_probability <- mean(predicted_exac_probability)

  predicted_exac_count <-  as.numeric(lapply(lambda, rpois, n=1))
  mean_predicted_exac_count <- mean(predicted_exac_count)


  #severity
  c_lin <-   c0 +
    c_male * patientData["gender"] +
    c_age10 * patientData["age10"] +
    c_nowsmk * patientData["nowsmk"] +
    c_oxygen * patientData["oxygen"] +
    c_fev1 * patientData["FEV1"] +
    c_sgrq10 * patientData["sgrq10"] +
    c_cardiovascular * patientData["statin"] +
    c_azithromycin * patientData["azithromycin"] +
    c_LAMA * patientData["LAMA"] +
    c_LABA * patientData["LABA"] +
    c_ICS * patientData["ICS"] +
    c_BMI10 * patientData["BMI10"]

  OR <- exp (as.numeric(c_lin) + z[, "z2"])
  predicted_severe_exac_probability <- (OR/(1+OR))
  mean_predicted_severe_exac_probability
  patientData [i, "predicted_severe_exac_probability"] <- mean(predicted_severe_exac_probability[,i])
  patientData [i, "predicted_severe_exac_rate"] <- patientData [i, "predicted_exac_rate"] * patientData [i, "predicted_severe_exac_probability"]

  predicted_severe_exac_count[, i] <-  as.numeric(lapply(patientData [i, "predicted_severe_exac_rate"], rpois, n=1))
  #print(predicted_severe_exac_count)
  patientData [i, "predicted_severe_exac_count"] <- mean(predicted_severe_exac_count[,i])






}


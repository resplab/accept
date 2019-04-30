
gamma	                    <-   0.9696
b0	                      <-   0.0528
b_male	                  <-   -0.09181
b_age10	                  <-   -0.03442
b_nowsmk	                <-   -0.1945
b_oxygen	                <-   0.08129
b_fev1	                  <-   -0.1693
b_sgrq10	                <-   0.104
b_cardiovascular	        <-   0.08814
b_randomized_azithromycin <- 	 -0.164
b_LAMA	                  <-   0.1415
b_LABA	                  <-   0.1204
b_ICS	                    <-   0.2177
b_randomized_LAMA	        <-   0.1589
b_randomized_LABA	        <-   0.1514
b_randomized_ICS	        <-   -0.2912
b_randomized_statin	      <-  -0.05562
b_BMI10                   <- 	-0.1201


c0	                      <-  -3.6145
c_male	                  <-  0.5516
c_age10	                  <-  0.06352
c_nowsmk                  <-  0.4086
c_oxygen                  <-  0.5254
c_fev1	                  <-  -0.502
c_sgrq10                  <-  0.2016
c_cardiovascular	        <-  0.3246
c_randomized_azithromycin <- 	-0.1178
c_LAMA	                  <-  -0.17
c_LABA            	      <-  0.01644
c_ICS	                    <-  0.386
c_randomized_LAMA	        <-  0.1081
c_randomized_LABA	        <-  -0.3354
c_randomized_ICS	        <-  -0.05231
c_randomized_statin	      <-  0.1106
c_BMI10           	      <-  -0.09662

b_age <- b_age10/10
b_SGRQ <- b_sgrq10/10
b_BMI <- b_BMI10/10

c_age <- c_age10/10
c_SGRQ <- c_sgrq10/10
c_BMI <- c_BMI10/10

v1 	<- 0.5971
v2	<- 2.3525
cov	<- 0.1365

covMat <- matrix(
  c(v1, cov, cov, v2),
  nrow = 2,
  ncol = 2
)


# predictACCEPT <- function (patientData, random_sampling_N = 1e4){
#
#   # no treatment
#   patientData <- patientData %>% mutate (randomized_azithromycin = 0)
#   noAzithroResults <- estimateACCEPT(patientData, random_sampling_N)
#
#   # with daily azithromycin
#   patientData <- patientData %>% mutate (randomized_azithromycin = 1)
#   azithroResults <- estimateACCEPT(patientData, random_sampling_N) %>% select (ID, starts_with("predict")) %>% rename_at (vars(starts_with("predict")),
#                                                                                   funs(str_replace(., "predict", "azithromycin_predict")))
#
#
#   result <- left_join(noAzithroResults, azithroResults, by ='ID')
#   return(result)
# }


#' Predicts COPD exacerbations within the next year
#' @param patientData patient data matrix. Can have one or many patients in it
#' @param random_sampling_N number of random sampling. Default is 1000.
#' @return patientData with prediction
#' @examples
#' results <- predictACCEPT(samplePatients)
#' @export
predictACCEPT <- function (patientData, random_sampling_N = 1e4, random_distribution_iteration = 2e4, calculate_CIs = TRUE){

  predicted_exac_rate <- matrix(0, random_sampling_N, nrow(patientData))
  predicted_exac_count <- matrix(0, random_sampling_N, nrow(patientData))
  predicted_severe_exac_count <- matrix(0, random_sampling_N, nrow(patientData))
  predicted_exac_probability <- matrix(0, random_sampling_N, nrow(patientData))
  predicted_severe_exac_rate <- matrix(0, random_sampling_N, nrow(patientData))
  predicted_severe_exac_probability <- matrix(0, random_sampling_N, nrow(patientData))

  azithro_predicted_exac_rate <- matrix(0, random_sampling_N, nrow(patientData))
  azithro_predicted_exac_count <- matrix(0, random_sampling_N, nrow(patientData))
  azithro_predicted_severe_exac_count <- matrix(0, random_sampling_N, nrow(patientData))
  azithro_predicted_exac_probability <- matrix(0, random_sampling_N, nrow(patientData))
  azithro_predicted_severe_exac_rate <- matrix(0, random_sampling_N, nrow(patientData))
  azithro_predicted_severe_exac_probability <- matrix(0, random_sampling_N, nrow(patientData))

  conditionalZ <- densityLastYrExac(patientData, random_distribution_iteration = random_distribution_iteration)

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
      b_LAMA * patientData[i, "LAMA"] +
      b_LABA * patientData[i, "LABA"] +
      b_ICS * patientData[i, "ICS"] +
      b_BMI * patientData[i, "BMI"]

    azithro_log_alpha <- log_alpha + b_randomized_azithromycin

    ID <- as.character(patientData[i, "ID"])
    z <- sample_n(conditionalZ[[ID]], random_sampling_N, replace = TRUE, weight = weight)

    # no treatment scenario results
    alpha <- exp (as.numeric(log_alpha) + z[, "z1"])
    lambda <- alpha ^ gamma
    predicted_exac_rate[, i] <- lambda
    predicted_exac_probability[, i] <- 1 - exp(-lambda)
    predicted_exac_count[, i] <-  as.numeric(lapply(lambda, rpois, n=1))


    patientData [i, "predicted_exac_probability"] <- mean(predicted_exac_probability[,i])
    patientData [i, "predicted_exac_rate"] <- mean(predicted_exac_rate[,i])


    # azithromycin scenario results
    azithro_alpha <- exp (as.numeric(azithro_log_alpha) + z[, "z1"])
    azithro_lambda <- azithro_alpha ^ gamma
    azithro_predicted_exac_rate[, i] <- azithro_lambda
    azithro_predicted_exac_probability[, i] <- 1 - exp(-azithro_lambda)
    azithro_predicted_exac_count[, i] <-  as.numeric(lapply(azithro_lambda, rpois, n=1))


    patientData [i, "azithromycin_predicted_exac_probability"] <-        mean(azithro_predicted_exac_probability[,i])
    patientData [i, "azithromycin_predicted_exac_rate"] <-               mean    (azithro_predicted_exac_rate[,i])

    #severity
    c_lin <-   c0 +
      c_male * patientData[i, "male"] +
      c_age * patientData[i, "age"] +
      c_nowsmk * patientData[i, "smoker"] +
      c_oxygen * patientData[i, "oxygen"] +
      c_fev1 * patientData[i, "FEV1"] +
      c_SGRQ * patientData[i, "SGRQ"] +
      c_cardiovascular * patientData[i, "statin"] +
      c_LAMA * patientData[i, "LAMA"] +
      c_LABA * patientData[i, "LABA"] +
      c_ICS * patientData[i, "ICS"] +
      c_BMI * patientData[i, "BMI"]

    azithro_c_lin <- c_lin + c_randomized_azithromycin

    # no treatment
    OR <- exp (as.numeric(c_lin) + z[, "z2"])
    predicted_severe_exac_probability[, i] <- (OR/(1+OR))
    predicted_severe_exac_rate[, i] <- predicted_exac_rate[, i] * predicted_severe_exac_probability[, i]

    patientData [i, "predicted_severe_exac_probability"] <- mean(predicted_severe_exac_probability[,i])
    patientData [i, "predicted_severe_exac_rate"] <- mean (predicted_severe_exac_rate[, i])

    # azithromycin treatment
    azithro_OR <- exp (as.numeric(azithro_c_lin) + z[, "z2"])
    azithro_predicted_severe_exac_probability[, i] <- (azithro_OR/(1+azithro_OR))
    azithro_predicted_severe_exac_rate[, i] <- azithro_predicted_exac_rate[, i] * azithro_predicted_severe_exac_probability[, i]

    patientData [i, "azithromycin_predicted_severe_exac_probability"]        <- mean    (azithro_predicted_severe_exac_probability[,i])
    patientData [i, "azithromycin_predicted_severe_exac_rate"]               <- mean    (azithro_predicted_severe_exac_rate[, i])

    ####bootstrapping for CI
    if (calculate_CIs) {
      bootProb <- numeric(random_sampling_N)
      bootRate <- numeric(random_sampling_N)

      azithro_bootProb <- numeric(random_sampling_N)
      azithro_bootRate <- numeric(random_sampling_N)

      severe_bootProb <- numeric(random_sampling_N)
      severe_bootRate <- numeric(random_sampling_N)

      azithro_severe_bootProb <- numeric(random_sampling_N)
      azithro_severe_bootRate <- numeric(random_sampling_N)

      for (j in 1:random_sampling_N) {

        bootProb [j] <- mean(sample(predicted_exac_probability[,i], replace = TRUE))
        bootRate [j] <- mean(sample(predicted_exac_rate[,i], replace = TRUE))

        azithro_bootProb [j] <- mean(sample(azithro_predicted_exac_probability[,i], replace = TRUE))
        azithro_bootRate [j] <- mean(sample(azithro_predicted_exac_rate[,i], replace = TRUE))

        severe_bootProb [j] <- mean(sample(predicted_severe_exac_probability[,i], replace = TRUE))
        severe_bootRate [j] <- mean(sample(predicted_severe_exac_rate[,i], replace = TRUE))

        azithro_severe_bootProb [j] <- mean(sample(azithro_predicted_severe_exac_probability[,i], replace = TRUE))
        azithro_severe_bootRate [j] <- mean(sample(azithro_predicted_severe_exac_rate[, i], replace = TRUE))
      }

      patientData [i, "predicted_exac_probability_lower"]  <- quantile(bootProb, 0.025)
      patientData [i, "predicted_exac_probability_upper"]  <- quantile(bootProb, 0.975)

      patientData [i, "predicted_exac_rate_lower"]  <- quantile(bootRate, 0.025)
      patientData [i, "predicted_exac_rate_upper"]  <- quantile(bootRate, 0.975)


      patientData [i, "azithromycin_predicted_exac_probability_lower"]  <- quantile(azithro_bootProb, 0.025)
      patientData [i, "azithromycin_predicted_exac_probability_upper"]  <- quantile(azithro_bootProb, 0.975)

      patientData [i, "azithromycin_predicted_exac_rate_lower"]  <-        quantile(azithro_bootRate, 0.025)
      patientData [i, "azithromycin_predicted_exac_rate_upper"]  <-        quantile(azithro_bootRate, 0.975)

      patientData [i, "predicted_severe_exac_probability_lower"]  <- quantile(severe_bootProb, 0.025)
      patientData [i, "predicted_severe_exac_probability_upper"]  <- quantile(severe_bootProb, 0.975)

      patientData [i, "predicted_severe_exac_rate_lower"]  <- quantile(severe_bootRate, 0.025)
      patientData [i, "predicted_severe_exac_rate_upper"]  <- quantile(severe_bootRate, 0.975)

      patientData [i, "azithromycin_predicted_severe_exac_probability_lower"]  <- quantile(azithro_severe_bootProb, 0.025)
      patientData [i, "azithromycin_predicted_severe_exac_probability_upper"]  <- quantile(azithro_severe_bootProb, 0.975)

      patientData [i, "azithromycin_predicted_severe_exac_rate_lower"]         <- quantile(azithro_severe_bootRate, 0.025)
      patientData [i, "azithromycin_predicted_severe_exac_rate_upper"]         <- quantile(azithro_severe_bootRate, 0.975)
    }

  }

  return(patientData)

}


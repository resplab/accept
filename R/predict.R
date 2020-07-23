

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


#' Predicts COPD exacerbation rate by severity level
#' @param patientData patient data matrix. Can have one or many patients in it
#' @param random_sampling_N number of random sampling. Default is 1000.
#' @param random_distribution_iteration default is 2*10^4
#' @param calculate_CIs whether to calculate confidence interval of the mean
#' @param betas betas provided by the user.
#' @param KeepSGRQ default is TRUE. If set to false, the value of SGRQ beta will be forced to be 0. Can be used for models without SGRQ.
#' @param KeepMeds default is TRUE. If set to false, beta values for LAMA, LABA, and ICS will be forced to be 0.Can be used for models without medications.
#' @return patientData with prediction
#' @examples
#' betas <- list()
#' betas$gamma	                    <- 0.9687
#' betas$b0	                      <- -0.00964
#' betas$b_male	                  <- -0.157
#' betas$b_age10	                  <- -0.01885
#' betas$b_nowsmk	                <- -0.2009
#' betas$b_oxygen	                <- 0.08781
#' betas$b_fev1pp100	              <- -0.4419
#' betas$b_sgrq10                  <- 0.103
#' betas$b_cardiovascular	        <- 0.09837
#' betas$b_randomized_azithromycin <- -0.1687
#' betas$b_LAMA	                  <- 0.1485
#' betas$b_LABA	                  <- 0.1216
#' betas$b_ICS	                    <- 0.2232
#' betas$b_randomized_LAMA	        <- 0.172
#' betas$b_randomized_LABA	        <- 0.1398
#' betas$b_randomized_ICS	        <- -0.2452
#' betas$b_randomized_statin	      <- -0.05617
#' betas$b_BMI10                   <- -0.1272
#'
#'
#' betas$c0	                      <- -3.973
#' betas$c_male	                  <- 0.3889
#' betas$c_age10	                  <- 0.1123
#' betas$c_nowsmk                  <- 0.4025
#' betas$c_oxygen                  <- 0.5558
#' betas$c_fev1pp100	              <- -1.1552
#' betas$c_sgrq10                  <- 0.205
#' betas$c_cardiovascular	        <- 0.3255
#' betas$c_randomized_azithromycin <- -0.1103
#' betas$c_LAMA	                  <- -0.1385
#' betas$c_LABA            	      <- 0.01246
#' betas$c_ICS	                    <- 0.3879
#' betas$c_randomized_LAMA	        <- 0.1074
#' betas$c_randomized_LABA	        <- -0.2253
#' betas$c_randomized_ICS	        <- -0.1211
#' betas$c_randomized_statin	      <- 0.109
#' betas$c_BMI10           	      <- -0.106
#'
#'
#' betas$v1 	<- 0.5968
#' betas$v2	<- 2.3847
#' betas$cov	<- 0.147
#'
#'
#' # More accurate azithromycin therapy estimates from AJE paper (https://doi.org/10.1093/aje/kww085), Table 2
#' betas$b_randomized_azithromycin <- 	 log(1/1.30)
#' betas$c_randomized_azithromycin <- 	log(0.93)
#'
#' results <- predictACCEPT(samplePatients, random_distribution_iteration = 5000, betas=betas)
#' @export
predictACCEPT <- function (patientData, random_sampling_N = 1e3,
                           random_distribution_iteration = 2e4, calculate_CIs = TRUE,  betas = NULL, KeepSGRQ = TRUE, KeepMeds = TRUE){

      gamma	                    <- betas$gamma
      b0	                      <- betas$b0
      b_male	                  <- betas$b_male
      b_age10	                  <- betas$b_age10
      b_nowsmk	                <- betas$b_nowsmk
      b_oxygen	                <- betas$b_oxygen
      b_fev1pp100	              <- betas$b_fev1pp100
      b_sgrq10                  <- KeepSGRQ*betas$b_sgrq10
      b_cardiovascular	        <- betas$b_cardiovascular
      b_randomized_azithromycin <- betas$b_randomized_azithromycin
      b_LAMA	                  <- KeepMeds*betas$b_LAMA
      b_LABA	                  <- KeepMeds*betas$b_LABA
      b_ICS	                    <- KeepMeds*betas$b_ICS
      b_randomized_LAMA	        <- betas$b_randomized_LAMA
      b_randomized_LABA	        <- betas$b_randomized_LABA
      b_randomized_ICS	        <- betas$b_randomized_ICS
      b_randomized_statin	      <- betas$b_randomized_statin
      b_BMI10                   <- betas$b_BMI10
      c0	                      <- betas$c0
      c_male	                  <- betas$c_male
      c_age10	                  <- betas$c_age10
      c_nowsmk                  <- betas$c_nowsmk
      c_oxygen                  <- betas$c_oxygen
      c_fev1pp100	              <- betas$c_fev1pp100
      c_sgrq10                  <- KeepSGRQ*betas$c_sgrq10
      c_cardiovascular	        <- betas$c_cardiovascular
      c_randomized_azithromycin <- betas$c_randomized_azithromycin
      c_LAMA	                  <- KeepMeds*betas$c_LAMA
      c_LABA            	      <- KeepMeds*betas$c_LABA
      c_ICS	                    <- KeepMeds*betas$c_ICS
      c_randomized_LAMA	        <- betas$c_randomized_LAMA
      c_randomized_LABA	        <- betas$c_randomized_LABA
      c_randomized_ICS	        <- betas$c_randomized_ICS
      c_randomized_statin	      <- betas$c_randomized_statin
      c_BMI10           	      <- betas$c_BMI10

      v1                       	<- betas$v1
      v2	                      <- betas$v2
      cov                      	<- betas$cov

      b_age <- b_age10/10
      b_SGRQ <- b_sgrq10/10
      b_BMI <- b_BMI10/10
      b_fev1 <- b_fev1pp100/100

      c_age <- c_age10/10
      c_SGRQ <- c_sgrq10/10
      c_BMI <- c_BMI10/10
      c_fev1 <- c_fev1pp100/100


      covMat <- matrix(
        c(v1, cov, cov, v2),
        nrow = 2,
        ncol = 2
      )

  predicted_exac_rate <- matrix(0, random_sampling_N, nrow(patientData))
  #predicted_exac_count <- matrix(0, random_sampling_N, nrow(patientData))
  predicted_severe_exac_count <- matrix(0, random_sampling_N, nrow(patientData))
  predicted_exac_probability <- matrix(0, random_sampling_N, nrow(patientData))
  predicted_severe_exac_rate <- matrix(0, random_sampling_N, nrow(patientData))
  predicted_severe_exac_probability <- matrix(0, random_sampling_N, nrow(patientData))

  azithro_predicted_exac_rate <- matrix(0, random_sampling_N, nrow(patientData))
  #azithro_predicted_exac_count <- matrix(0, random_sampling_N, nrow(patientData))
  azithro_predicted_severe_exac_count <- matrix(0, random_sampling_N, nrow(patientData))
  azithro_predicted_exac_probability <- matrix(0, random_sampling_N, nrow(patientData))
  azithro_predicted_severe_exac_rate <- matrix(0, random_sampling_N, nrow(patientData))
  azithro_predicted_severe_exac_probability <- matrix(0, random_sampling_N, nrow(patientData))

  conditionalZ <- densityLastYrExac(patientData, random_distribution_iteration = random_distribution_iteration, betas=betas)

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
    weight = NULL
    ID <- as.character(patientData[i, "ID"])
    z <- sample_n(conditionalZ[[ID]], random_sampling_N, replace = TRUE, weight = weight)

    # no treatment scenario results
    alpha <- exp (as.numeric(log_alpha) + z[, "z1"])
    lambda <- alpha ^ gamma
    predicted_exac_rate[, i] <- lambda
    predicted_exac_probability[, i] <- 1 - exp(-lambda)
    #predicted_exac_count[, i] <-  as.numeric(lapply(lambda, rpois, n=1))


    patientData [i, "predicted_exac_probability"] <- mean(predicted_exac_probability[,i])
    patientData [i, "predicted_exac_rate"] <- mean(predicted_exac_rate[,i])

    # azithromycin scenario results
    azithro_alpha <- exp (as.numeric(azithro_log_alpha) + z[, "z1"])
    azithro_lambda <- azithro_alpha ^ gamma
    azithro_predicted_exac_rate[, i] <- azithro_lambda
    azithro_predicted_exac_probability[, i] <- 1 - exp(-azithro_lambda)
    #azithro_predicted_exac_count[, i] <-  as.numeric(lapply(azithro_lambda, rpois, n=1))

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

    # severe no treatment
    OR <- exp (as.numeric(c_lin) + z[, "z2"])
    predicted_severe_exac_probability[, i] <- (OR/(1+OR))
    predicted_severe_exac_rate[, i] <- predicted_exac_rate[, i] * predicted_severe_exac_probability[, i]

    patientData [i, "predicted_severe_exac_probability"] <- mean(predicted_severe_exac_probability[,i])
    patientData [i, "predicted_severe_exac_rate"] <- mean (predicted_severe_exac_rate[, i])

    # severe  azithromycin treatment
    azithro_OR <- exp (as.numeric(azithro_c_lin) + z[, "z2"])
    azithro_predicted_severe_exac_probability[, i] <- (azithro_OR/(1+azithro_OR))
    azithro_predicted_severe_exac_rate[, i] <- azithro_predicted_exac_rate[, i] * azithro_predicted_severe_exac_probability[, i]

    patientData [i, "azithromycin_predicted_severe_exac_probability"]        <- mean    (azithro_predicted_severe_exac_probability[,i])
    patientData [i, "azithromycin_predicted_severe_exac_rate"]               <- mean    (azithro_predicted_severe_exac_rate[, i])

    # bootstrapping for PI

    patientData [i, "predicted_exac_probability_lower_PI"]                      <- quantile(predicted_exac_probability[,i], 0.025)
    patientData [i, "predicted_exac_probability_upper_PI"]                      <- quantile(predicted_exac_probability[,i], 0.975)

    patientData [i, "predicted_exac_rate_lower_PI"]                             <- quantile(predicted_exac_rate[,i], 0.025)
    patientData [i, "predicted_exac_rate_upper_PI"]                             <- quantile(predicted_exac_rate[,i], 0.975)

    patientData [i, "azithromycin_predicted_exac_probability_lower_PI"]         <- quantile(azithro_predicted_exac_probability[,i], 0.025)
    patientData [i, "azithromycin_predicted_exac_probability_upper_PI"]         <- quantile(azithro_predicted_exac_probability[,i], 0.975)

    patientData [i, "azithromycin_predicted_exac_rate_lower_PI"]                <- quantile(azithro_predicted_exac_rate[,i], 0.025)
    patientData [i, "azithromycin_predicted_exac_rate_upper_PI"]                <- quantile(azithro_predicted_exac_rate[,i], 0.975)

    patientData [i, "predicted_severe_exac_probability_lower_PI"]               <- quantile(predicted_severe_exac_probability[,i], 0.025)
    patientData [i, "predicted_severe_exac_probability_upper_PI"]               <- quantile(predicted_severe_exac_probability[,i], 0.975)

    patientData [i, "predicted_severe_exac_rate_lower_PI"]                      <- quantile(predicted_severe_exac_rate[,i], 0.025)
    patientData [i, "predicted_severe_exac_rate_upper_PI"]                      <- quantile(predicted_severe_exac_rate[,i], 0.975)

    patientData [i, "azithromycin_predicted_severe_exac_probability_lower_PI"]  <- quantile(azithro_predicted_severe_exac_probability[,i], 0.025)
    patientData [i, "azithromycin_predicted_severe_exac_probability_upper_PI"]  <- quantile(azithro_predicted_severe_exac_probability[,i], 0.975)

    patientData [i, "azithromycin_predicted_severe_exac_rate_lower_PI"]         <- quantile(azithro_predicted_severe_exac_rate[,i], 0.025)
    patientData [i, "azithromycin_predicted_severe_exac_rate_upper_PI"]         <- quantile(azithro_predicted_severe_exac_rate[,i], 0.975)


    # bootstrapping for CI
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

      patientData [i, "predicted_exac_probability_lower_CI"]                      <- quantile(bootProb, 0.025)
      patientData [i, "predicted_exac_probability_upper_CI"]                      <- quantile(bootProb, 0.975)

      patientData [i, "predicted_exac_rate_lower_CI"]                             <- quantile(bootRate, 0.025)
      patientData [i, "predicted_exac_rate_upper_CI"]                             <- quantile(bootRate, 0.975)

      patientData [i, "azithromycin_predicted_exac_probability_lower_CI"]         <- quantile(azithro_bootProb, 0.025)
      patientData [i, "azithromycin_predicted_exac_probability_upper_CI"]         <- quantile(azithro_bootProb, 0.975)

      patientData [i, "azithromycin_predicted_exac_rate_lower_CI"]                <- quantile(azithro_bootRate, 0.025)
      patientData [i, "azithromycin_predicted_exac_rate_upper_CI"]                <- quantile(azithro_bootRate, 0.975)

      patientData [i, "predicted_severe_exac_probability_lower_CI"]               <- quantile(severe_bootProb, 0.025)
      patientData [i, "predicted_severe_exac_probability_upper_CI"]               <- quantile(severe_bootProb, 0.975)

      patientData [i, "predicted_severe_exac_rate_lower_CI"]                      <- quantile(severe_bootRate, 0.025)
      patientData [i, "predicted_severe_exac_rate_upper_CI"]                      <- quantile(severe_bootRate, 0.975)

      patientData [i, "azithromycin_predicted_severe_exac_probability_lower_CI"]  <- quantile(azithro_severe_bootProb, 0.025)
      patientData [i, "azithromycin_predicted_severe_exac_probability_upper_CI"]  <- quantile(azithro_severe_bootProb, 0.975)

      patientData [i, "azithromycin_predicted_severe_exac_rate_lower_CI"]         <- quantile(azithro_severe_bootRate, 0.025)
      patientData [i, "azithromycin_predicted_severe_exac_rate_upper_CI"]         <- quantile(azithro_severe_bootRate, 0.975)
    }

  }

  return(patientData)

}


#' Predicts probability of observing n exacerbations in the next year
#' @param patientResults patient results vector, produced by predictAccept.
#' @param n how many exacerbations
#' @param shortened boolean: Shortened results groups into 0, 1, 2, and 3 or more exacerbations
#' @return a matrix of probabilities with the number of exacerbations as rows and number of severe exacerbations as columns
#' @examples
#' results <- predictACCEPT(samplePatients[1,], random_distribution_iteration = 5000)
#' predictCountProb (results)
#' @import plotly
#' @export
predictCountProb <- function (patientResults, n = 10, shortened = TRUE){
  results <- matrix (0, nrow = n, ncol = n)
  rowNames = 0:(n-1)
  rowNames = paste0(rowNames, " exacerbation(s)")
  colNames = 0:(n-1)
  colNames = paste0(colNames, " severe")
  colnames(results) = colNames
  rownames(results) = rowNames
  # i is all exacs, j of them severe
 for (i in 1:n) {
   for (j in 1:n) {
      if (i>=j) {
       results [i, j] <- dpois(i-1, patientResults$predicted_exac_rate) *
                         factorial(i-1) / (factorial(j-1) * factorial (i-j))  *
                         patientResults$predicted_severe_exac_probability ^ (j-1) *
                         (1 - patientResults$predicted_severe_exac_probability) ^ (i-j)
       }}
 }
 if (shortened) {
   shortResults <- results
   shortResults[,4] <- rowSums(results[, 4:10])
   shortResults[4,] <- colSums(results[4:10, ])
   shortResults <- shortResults[1:4, 1:4]
   colnames(shortResults) <- c("none severe", "1 severe", "2 severe", "3 or more severe")
   rownames(shortResults) <- c("no exacerbations", "1 exacerbation", "2 exacerbations", "3 or more exacerbations")

   results <- shortResults
 }
 return(results)
}

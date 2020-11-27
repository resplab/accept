#' Predicts COPD exacerbation rate by severity level
#' @param patientData patient data matrix. Can have one or many patients in it
#' @param random_sampling_N number of random sampling. Default is 100.
#' @param lastYrExacCol the column specifying last year all exacerbation count
#' @param lastYrSevExacCol the column specifying last year severe exacerbation count
#' @param calculate_CIs whether to calculate confidence interval of the mean
#' @return patientData with prediction
#' @examples
#' results <- predictACCEPT(samplePatients)
#' @export
predictACCEPT <- function (patientData, random_sampling_N = 1e2, lastYrExacCol="LastYrExacCount",
                           lastYrSevExacCol="LastYrSevExacCount", calculate_CIs = FALSE, ...){

  gamma	                    <- 0.9687
  b0	                      <- -0.00964
  b_male	                  <- -0.157
  b_age10	                  <- -0.01885
  b_nowsmk	                <- -0.2009
  b_oxygen	                <- 0.08781
  b_fev1pp100	              <- -0.4419
  b_sgrq10                  <- 0.103
  b_cardiovascular	        <- 0.09837
  b_randomized_azithromycin <- -0.1687
  b_LAMA	                  <- 0.1485
  b_LABA	                  <- 0.1216
  b_ICS	                    <- 0.2232
  b_randomized_LAMA	        <- 0.172
  b_randomized_LABA	        <- 0.1398
  b_randomized_ICS	        <- -0.2452
  b_randomized_statin	      <- -0.05617
  b_BMI10                   <- -0.1272

  c0	                      <- -3.973
  c_male	                  <- 0.3889
  c_age10	                  <- 0.1123
  c_nowsmk                  <- 0.4025
  c_oxygen                  <- 0.5558
  c_fev1pp100	              <- -1.1552
  c_sgrq10                  <- 0.205
  c_cardiovascular	        <- 0.3255
  c_randomized_azithromycin <- -0.1103
  c_LAMA	                  <- -0.1385
  c_LABA            	      <- 0.01246
  c_ICS	                    <- 0.3879
  c_randomized_LAMA	        <- 0.1074
  c_randomized_LABA	        <- -0.2253
  c_randomized_ICS	        <- -0.1211
  c_randomized_statin	      <- 0.109
  c_BMI10           	      <- -0.106

  b_age <- b_age10/10
  b_SGRQ <- b_sgrq10/10
  b_BMI <- b_BMI10/10
  b_fev1 <- b_fev1pp100/100

  c_age <- c_age10/10
  c_SGRQ <- c_sgrq10/10
  c_BMI <- c_BMI10/10
  c_fev1 <- c_fev1pp100/100

  v1 	<- 0.5968
  v2	<- 2.3847
  cov	<- 0.147

  covMat <- matrix(
    c(v1, cov, cov, v2),
    nrow = 2,
    ncol = 2
  )

  # More accurate azithromycin therapy estimates from AJE paper (https://doi.org/10.1093/aje/kww085), Table 2
  b_randomized_azithromycin <- 	 log(1/1.30)
  c_randomized_azithromycin <- 	 log(0.93)

  patientData <- patientData %>% mutate (log_alpha = b0 +
                                           b_male           * male   +
                                           b_age            * age    +
                                           b_nowsmk         * smoker +
                                           b_oxygen         * oxygen +
                                           b_fev1           * FEV1   +
                                           b_SGRQ           * SGRQ   +
                                           b_cardiovascular * statin +
                                           b_LAMA           * LAMA   +
                                           b_LABA           * LABA   +
                                           b_ICS            * ICS    +
                                           b_BMI            * BMI)       %>%
                                 mutate (c_lin = c0 +
                                           c_male           * male   +
                                           c_age            * age    +
                                           c_nowsmk         * smoker +
                                           c_oxygen         * oxygen +
                                           c_fev1           * FEV1   +
                                           c_SGRQ           * SGRQ   +
                                           c_cardiovascular * statin +
                                           c_LAMA           * LAMA   +
                                           c_LABA           * LABA   +
                                           c_ICS            * ICS    +
                                           c_BMI            * BMI)


   RE_seq_1 = seq(from = -2 * covMat[1, 1], to = 2 * covMat[1, 1], length.out = random_sampling_N)
   RE_seq_2 = seq(from = -2 * covMat[2, 2], to = 2 * covMat[2, 2], length.out = random_sampling_N)
   RE_W_mat <- outer(X = RE_seq_1, Y = RE_seq_2, FUN = Vectorize(function(x, y) mvtnorm::dmvnorm(c(x, y), sigma = covMat)))

  Lambda  <- exp(as.matrix(patientData[, "log_alpha"], ncol = 1)) %*% matrix(exp(RE_seq_1), nrow = 1)
  ProbSev <- exp(as.matrix(patientData[ , "c_lin"], ncol = 1)) %*% matrix(exp(RE_seq_2), nrow = 1)
  ProbSev <- ProbSev / (1 + ProbSev)

  Lambda_Sev <- lapply(c(1 : nrow(patientData)), function(x) matrix(Lambda[x, ], ncol = 1) %*% matrix(ProbSev[x, ], nrow = 1))
  Lambda_non_Sev <- lapply(c(1 : nrow(patientData)), function(x) matrix(Lambda[x, ], ncol = 1) %*% matrix(1 - ProbSev[x, ], nrow = 1))

  Posterior_Sev_W <-
    lapply(c(1 : nrow(patientData)), function(x) {
      t(apply(Lambda_Sev[[x]], 1, function(y) dpois(x = as.numeric(patientData[x, lastYrSevExacCol]), lambda = y))) * RE_W_mat
    })

  Posterior_non_Sev_W <-
    lapply(c(1 : nrow(patientData)), function(x) {
      t(apply(Lambda_non_Sev[[x]], 1, function(y) dpois(x = as.numeric(patientData[x, lastYrExacCol] - patientData[x , lastYrSevExacCol]), lambda = y))) * RE_W_mat
    })

  Posterior_all_W <-
    lapply(c(1 : nrow(patientData)), function(x) {
      t(apply(Lambda_non_Sev[[x]]+Lambda_Sev[[x]], 1, function(y) dpois(x = as.numeric(patientData[x, lastYrExacCol]), lambda = y))) * RE_W_mat
    })

  Rate_Sev_Adj <- sapply(c(1 : nrow(patientData)), function(x) weighted.mean(x = Lambda_Sev[[x]], w = Posterior_Sev_W[[x]]))
  Rate_Sev_Adj_lower_PI <- sapply(c(1 : nrow(patientData)), function(x) reldist::wtd.quantile(x = as.vector(Lambda_Sev[[x]]), weight = Posterior_Sev_W[[x]], q = 0.025))
  Rate_Sev_Adj_upper_PI <- sapply(c(1 : nrow(patientData)), function(x) reldist::wtd.quantile(x = as.vector(Lambda_Sev[[x]]), weight = Posterior_Sev_W[[x]], q = 0.975))


  Rate_non_Sev_Adj <- sapply(c(1 : nrow(patientData)), function(x) weighted.mean(x = Lambda_non_Sev[[x]], w = Posterior_non_Sev_W[[x]]))
  Rate_non_Sev_Adj_lower_PI <- sapply(c(1 : nrow(patientData)), function(x) reldist::wtd.quantile(x = as.vector(Lambda_non_Sev[[x]]), weight = Posterior_non_Sev_W[[x]], q = 0.025))
  Rate_non_Sev_Adj_upper_PI <- sapply(c(1 : nrow(patientData)), function(x) reldist::wtd.quantile(x = as.vector(Lambda_non_Sev[[x]]), weight = Posterior_non_Sev_W[[x]], q = 0.975))


  Rate_Sev_SD_Adj <- sqrt(Rate_Sev_Adj + Rate_Sev_Adj ^ 2 * (exp(0.97 * covMat[1, 1]) - 1))
  Rate_non_Sev_SD_Adj <- sqrt(Rate_non_Sev_Adj + Rate_non_Sev_Adj ^ 2 * (exp(0.97 * covMat[1, 1]) - 1))

  Rate_Adj <- Rate_Sev_Adj + Rate_non_Sev_Adj
  Rate_Adj_lower_PI <- sapply(c(1 : nrow(patientData)), function(x) reldist::wtd.quantile(x = as.vector(Lambda_non_Sev[[x]]+Lambda_Sev[[x]]), weight = Posterior_all_W[[x]], q = 0.025))
  Rate_Adj_upper_PI <- sapply(c(1 : nrow(patientData)), function(x) reldist::wtd.quantile(x = as.vector(Lambda_non_Sev[[x]]+Lambda_Sev[[x]]), weight = Posterior_all_W[[x]], q = 0.975))


  Rate_SD_Adj <- sqrt(Rate_Adj + Rate_Adj ^ 2 * (exp(0.97 * covMat[1, 1]) - 1))

  risk_at_least_one_exac <- 1 - exp(-Rate_Adj)
  risk_at_least_one_exac_lower_PI <- 1 - exp(-Rate_Adj_lower_PI)
  risk_at_least_one_exac_upper_PI <- 1 - exp(-Rate_Adj_upper_PI)

  risk_at_least_one_Sev_exac <- 1 - exp(-Rate_Sev_Adj)
  risk_at_least_one_Sev_exac_lower_PI <- 1 - exp(-Rate_Sev_Adj_lower_PI)
  risk_at_least_one_Sev_exac_upper_PI <- 1 - exp(-Rate_Sev_Adj_upper_PI)

  patientData <- patientData %>% mutate(predicted_exac_probability                 = risk_at_least_one_exac,
                                        predicted_exac_probability_lower_PI        = risk_at_least_one_exac_lower_PI,
                                        predicted_exac_probability_upper_PI        = risk_at_least_one_exac_upper_PI,
                                        # predicted_exac_probability_lower_CI        = risk_at_least_one_exac_lower_CI,
                                        # predicted_exac_probability_upper_CI        = risk_at_least_one_exac_upper_CI,

                                        predicted_exac_rate                        = Rate_Adj,
                                        predicted_exac_rate_lower_PI               = Rate_Adj_lower_PI,
                                        predicted_exac_rate_upper_PI               = Rate_Adj_upper_PI,
                                        # predicted_exac_rate_lower_CI               = Rate_Adj_lower_CI,
                                        # predicted_exac_rate_upper_CI               = Rate_Adj_upper_CI,

                                        predicted_severe_exac_probability          = risk_at_least_one_Sev_exac,
                                        predicted_severe_exac_probability_lower_PI = risk_at_least_one_Sev_exac_lower_PI,
                                        predicted_severe_exac_probability_upper_PI = risk_at_least_one_Sev_exac_upper_PI,
                                        # predicted_severe_exac_probability_lower_CI = risk_at_least_one_Sev_exac_lower_CI,
                                        # predicted_severe_exac_probability_upper_CI = risk_at_least_one_Sev_exac_upper_CI,

                                        predicted_severe_exac_rate                 = Rate_non_Sev_Adj,
                                        predicted_severe_exac_rate_lower_PI        = Rate_non_Sev_Adj_lower_PI,
                                        predicted_severe_exac_rate_upper_PI        = Rate_non_Sev_Adj_upper_PI
                                        # predicted_severe_exac_rate_lower_CI        = Rate_non_Sev_Adj_lower_CI,
                                        # predicted_severe_exac_rate_upper_CI        = Rate_non_Sev_Adj_upper_CI,

                                        # azithromycin_predicted_exac_probability                 = azithromycin_risk_at_least_one_exac,
                                        # azithromycin_predicted_exac_probability_lower_PI        = azithromycin_risk_at_least_one_exac_lower_PI,
                                        # azithromycin_predicted_exac_probability_upper_PI        = azithromycin_risk_at_least_one_exac_upper_PI,
                                        # azithromycin_predicted_exac_probability_lower_CI        = azithromycin_risk_at_least_one_exac_lower_CI,
                                        # azithromycin_predicted_exac_probability_upper_CI        = azithromycin_risk_at_least_one_exac_upper_CI,
                                        #
                                        # azithromycin_predicted_exac_rate                        = azithromycin_Rate_Adj,
                                        # azithromycin_predicted_exac_rate_lower_PI               = azithromycin_Rate_Adj_lower_PI,
                                        # azithromycin_predicted_exac_rate_upper_PI               = azithromycin_Rate_Adj_upper_PI,
                                        # azithromycin_predicted_exac_rate_lower_CI               = azithromycin_Rate_Adj_lower_CI,
                                        # azithromycin_predicted_exac_rate_upper_CI               = azithromycin_Rate_Adj_upper_CI,
                                        #
                                        # azithromycin_predicted_severe_exac_probability          = azithromycin_risk_at_least_one_Sev_exac,
                                        # azithromycin_predicted_severe_exac_probability_lower_PI = azithromycin_risk_at_least_one_Sev_exac_lower_PI,
                                        # azithromycin_predicted_severe_exac_probability_upper_PI = azithromycin_risk_at_least_one_Sev_exac_upper_PI,
                                        # azithromycin_predicted_severe_exac_probability_lower_CI = azithromycin_risk_at_least_one_Sev_exac_lower_CI,
                                        # azithromycin_predicted_severe_exac_probability_upper_CI = azithromycin_risk_at_least_one_Sev_exac_upper_CI,
                                        #
                                        # azithromycin_predicted_severe_exac_rate                 = azithromycin_Rate_non_Sev_Adj,
                                        # azithromycin_predicted_severe_exac_rate_lower_PI        = azithromycin_Rate_non_Sev_Adj_lower_PI,
                                        # azithromycin_predicted_severe_exac_rate_upper_PI        = azithromycin_Rate_non_Sev_Adj_upper_PI,
                                        # azithromycin_predicted_severe_exac_rate_lower_CI        = azithromycin_Rate_non_Sev_Adj_lower_CI,
                                        # azithromycin_predicted_severe_exac_rate_upper_CI        = azithromycin_Rate_non_Sev_Adj_upper_CI,


  )


  return(patientData)

}


#' Predicts probability of observing n exacerbations in the next year
#' @param patientResults patient results vector, produced by predictAccept.
#' @param n how many exacerbations
#' @param shortened boolean: Shortened results groups into 0, 1, 2, and 3 or more exacerbations
#' @return a matrix of probabilities with the number of exacerbations as rows and number of severe exacerbations as columns
#' @examples
#' results <- predictACCEPT(samplePatients[1,])
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
                         dbinom(x= j-1, size = (i-1), prob = patientResults$predicted_severe_exac_probability)
                         # factorial(i-1) / (factorial(j-1) * factorial (i-j))  *
                         # patientResults$predicted_severe_exac_probability ^ (j-1) *
                         # (1 - patientResults$predicted_severe_exac_probability) ^ (i-j)
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

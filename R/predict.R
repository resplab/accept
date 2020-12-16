binorm_pdf <- function(x, sigma) {
  s1 <- sqrt(sigma[1, 1])
  s2 <- sqrt(sigma[2, 2])
  rho <- sigma[1, 2] / (s1 * s2)
  Z <- x[1] ^ 2 / sigma[1, 1] - 2 * rho * x[1] * x[2] / (s1 * s2) + x[2] ^ 2 / sigma[2, 2]
  densRes <- 1 / (2 * pi * s1 * s2 * sqrt(1 - rho ^ 2)) * exp(- Z / (2 * (1 - rho ^ 2)))
  return(densRes)
}

Sp_Manual_Pred <- function(Predictor, CoefEst, Boundary_knots) {
  bs_obj <- bs(Predictor, knots = 0, Boundary.knots = Boundary_knots)
  if (length(Predictor) == 1) BasisFuncs <- t(c(1, as.numeric(bs_obj)[-1]))
  else BasisFuncs <- as.matrix(cbind(1, bs_obj[ , -1]), ncol=4)
  Preds <- BasisFuncs %*% matrix(CoefEst, ncol = 1)
  return(Preds)
}


# Predicts COPD exacerbation rate by severity level
# @param patientData patient data matrix. Can have one or many patients in it
# @param random_sampling_N number of random sampling. Default is 1000.
# @param lastYrExacCol the column specifying last year all exacerbation count
# @param lastYrSevExacCol the column specifying last year severe exacerbation count
# @param calculate_CIs whether to calculate confidence interval of the mean
# @param betas betas provided by the user.
# @param KeepSGRQ default is TRUE. If set to false, the value of SGRQ beta will be forced to be 0. Can be used for models without SGRQ.
# @param KeepMeds default is TRUE. If set to false, beta values for LAMA, LABA, and ICS will be forced to be 0.Can be used for models without medications.
# @return patientData with prediction
# @examples
# betas <- list()
# betas$gamma	                    <- 0.9687
# betas$b0	                      <- -0.00964
# betas$b_male	                  <- -0.157
# betas$b_age10	                  <- -0.01885
# betas$b_nowsmk	                <- -0.2009
# betas$b_oxygen	                <- 0.08781
# betas$b_fev1pp100	              <- -0.4419
# betas$b_sgrq10                  <- 0.103
# betas$b_cardiovascular	        <- 0.09837
# betas$b_randomized_azithromycin <- -0.1687
# betas$b_LAMA	                  <- 0.1485
# betas$b_LABA	                  <- 0.1216
# betas$b_ICS	                    <- 0.2232
# betas$b_randomized_LAMA	        <- 0.172
# betas$b_randomized_LABA	        <- 0.1398
# betas$b_randomized_ICS	        <- -0.2452
# betas$b_randomized_statin	      <- -0.05617
# betas$b_BMI10                   <- -0.1272
#
#
# betas$c0	                      <- -3.973
# betas$c_male	                  <- 0.3889
# betas$c_age10	                  <- 0.1123
# betas$c_nowsmk                  <- 0.4025
# betas$c_oxygen                  <- 0.5558
# betas$c_fev1pp100	              <- -1.1552
# betas$c_sgrq10                  <- 0.205
# betas$c_cardiovascular	        <- 0.3255
# betas$c_randomized_azithromycin <- -0.1103
# betas$c_LAMA	                  <- -0.1385
# betas$c_LABA            	      <- 0.01246
# betas$c_ICS	                    <- 0.3879
# betas$c_randomized_LAMA	        <- 0.1074
# betas$c_randomized_LABA	        <- -0.2253
# betas$c_randomized_ICS	        <- -0.1211
# betas$c_randomized_statin	      <- 0.109
# betas$c_BMI10           	      <- -0.106
#
#
# betas$v1 	<- 0.5968
# betas$v2	<- 2.3847
# betas$cov	<- 0.147
#
#
# betas$b_randomized_azithromycin <- 	 log(1/1.30)
# betas$c_randomized_azithromycin <- 	log(0.93)
#
# results <- acceptEngine(samplePatients, betas=betas)
acceptEngine <- function (patientData, random_sampling_N = 1e2,lastYrExacCol="LastYrExacCount",
                          lastYrSevExacCol="LastYrSevExacCount", calculate_CIs = TRUE,  betas = NULL, KeepSGRQ = TRUE, KeepMeds = TRUE){

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
  RE_W_mat <- outer(X = RE_seq_1, Y = RE_seq_2, FUN = Vectorize(function(x, y) binorm_pdf(c(x, y), sigma = covMat)))

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


  patientData <- patientData %>% select(-log_alpha, -c_lin) %>%
    mutate(predicted_exac_probability                 = risk_at_least_one_exac,
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
    )

  ## Now for Azithromycin

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
                                           b_BMI            * BMI    +
                                           b_randomized_azithromycin)       %>%
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
              c_BMI            * BMI    +
              c_randomized_azithromycin)



  azithromycin_Lambda  <- exp(as.matrix(patientData[, "log_alpha"], ncol = 1)) %*% matrix(exp(RE_seq_1), nrow = 1)
  azithromycin_ProbSev <- exp(as.matrix(patientData[ , "c_lin"], ncol = 1)) %*% matrix(exp(RE_seq_2), nrow = 1)
  azithromycin_ProbSev <- azithromycin_ProbSev / (1 + azithromycin_ProbSev)

  azithromycin_Lambda_Sev <- lapply(c(1 : nrow(patientData)), function(x) matrix(azithromycin_Lambda[x, ], ncol = 1) %*% matrix(azithromycin_ProbSev[x, ], nrow = 1))
  azithromycin_Lambda_non_Sev <- lapply(c(1 : nrow(patientData)), function(x) matrix(azithromycin_Lambda[x, ], ncol = 1) %*% matrix(1 - azithromycin_ProbSev[x, ], nrow = 1))

  azithromycin_Posterior_Sev_W <-
    lapply(c(1 : nrow(patientData)), function(x) {
      t(apply(azithromycin_Lambda_Sev[[x]], 1, function(y) dpois(x = as.numeric(patientData[x, lastYrSevExacCol]), lambda = y))) * RE_W_mat
    })

  azithromycin_Posterior_non_Sev_W <-
    lapply(c(1 : nrow(patientData)), function(x) {
      t(apply(azithromycin_Lambda_non_Sev[[x]], 1, function(y) dpois(x = as.numeric(patientData[x, lastYrExacCol] - patientData[x , lastYrSevExacCol]), lambda = y))) * RE_W_mat
    })

  azithromycin_Posterior_all_W <-
    lapply(c(1 : nrow(patientData)), function(x) {
      t(apply(azithromycin_Lambda_non_Sev[[x]]+azithromycin_Lambda_Sev[[x]], 1, function(y) dpois(x = as.numeric(patientData[x, lastYrExacCol]), lambda = y))) * RE_W_mat
    })

  azithromycin_Rate_Sev_Adj <- sapply(c(1 : nrow(patientData)), function(x) weighted.mean(x = azithromycin_Lambda_Sev[[x]], w = azithromycin_Posterior_Sev_W[[x]]))
  azithromycin_Rate_Sev_Adj_lower_PI <- sapply(c(1 : nrow(patientData)), function(x) reldist::wtd.quantile(x = as.vector(azithromycin_Lambda_Sev[[x]]), weight = azithromycin_Posterior_Sev_W[[x]], q = 0.025))
  azithromycin_Rate_Sev_Adj_upper_PI <- sapply(c(1 : nrow(patientData)), function(x) reldist::wtd.quantile(x = as.vector(azithromycin_Lambda_Sev[[x]]), weight = azithromycin_Posterior_Sev_W[[x]], q = 0.975))


  azithromycin_Rate_non_Sev_Adj <- sapply(c(1 : nrow(patientData)), function(x) weighted.mean(x = azithromycin_Lambda_non_Sev[[x]], w = azithromycin_Posterior_non_Sev_W[[x]]))
  azithromycin_Rate_non_Sev_Adj_lower_PI <- sapply(c(1 : nrow(patientData)), function(x) reldist::wtd.quantile(x = as.vector(azithromycin_Lambda_non_Sev[[x]]), weight = azithromycin_Posterior_non_Sev_W[[x]], q = 0.025))
  azithromycin_Rate_non_Sev_Adj_upper_PI <- sapply(c(1 : nrow(patientData)), function(x) reldist::wtd.quantile(x = as.vector(azithromycin_Lambda_non_Sev[[x]]), weight = azithromycin_Posterior_non_Sev_W[[x]], q = 0.975))


  azithromycin_Rate_Sev_SD_Adj <- sqrt(azithromycin_Rate_Sev_Adj + azithromycin_Rate_Sev_Adj ^ 2 * (exp(0.97 * covMat[1, 1]) - 1))
  azithromycin_Rate_non_Sev_SD_Adj <- sqrt(azithromycin_Rate_non_Sev_Adj + azithromycin_Rate_non_Sev_Adj ^ 2 * (exp(0.97 * covMat[1, 1]) - 1))

  azithromycin_Rate_Adj <- azithromycin_Rate_Sev_Adj + azithromycin_Rate_non_Sev_Adj
  azithromycin_Rate_Adj_lower_PI <- sapply(c(1 : nrow(patientData)), function(x) reldist::wtd.quantile(x = as.vector(azithromycin_Lambda_non_Sev[[x]]+azithromycin_Lambda_Sev[[x]]), weight = azithromycin_Posterior_all_W[[x]], q = 0.025))
  azithromycin_Rate_Adj_upper_PI <- sapply(c(1 : nrow(patientData)), function(x) reldist::wtd.quantile(x = as.vector(azithromycin_Lambda_non_Sev[[x]]+azithromycin_Lambda_Sev[[x]]), weight = azithromycin_Posterior_all_W[[x]], q = 0.975))


  azithromycin_Rate_SD_Adj <- sqrt(azithromycin_Rate_Adj + azithromycin_Rate_Adj ^ 2 * (exp(0.97 * covMat[1, 1]) - 1))

  azithromycin_risk_at_least_one_exac <- 1 - exp(-azithromycin_Rate_Adj)
  risk_at_least_one_exac_lower_PI <- 1 - exp(-Rate_Adj_lower_PI)
  azithromycin_risk_at_least_one_exac_lower_PI <- 1 - exp(-azithromycin_Rate_Adj_lower_PI)
  azithromycin_risk_at_least_one_exac_upper_PI <- 1 - exp(-azithromycin_Rate_Adj_upper_PI)

  azithromycin_risk_at_least_one_Sev_exac <- 1 - exp(-azithromycin_Rate_Sev_Adj)
  azithromycin_risk_at_least_one_Sev_exac_lower_PI <- 1 - exp(-azithromycin_Rate_Sev_Adj_lower_PI)
  azithromycin_risk_at_least_one_Sev_exac_upper_PI <- 1 - exp(-azithromycin_Rate_Sev_Adj_upper_PI)


  ## Putting it all together
  patientData <- patientData %>% select(-log_alpha, -c_lin) %>%
                                 mutate(azithromycin_predicted_exac_probability                 = azithromycin_risk_at_least_one_exac,
                                        azithromycin_predicted_exac_probability_lower_PI        = azithromycin_risk_at_least_one_exac_lower_PI,
                                        azithromycin_predicted_exac_probability_upper_PI        = azithromycin_risk_at_least_one_exac_upper_PI,
                                        # azithromycin_predicted_exac_probability_lower_CI        = azithromycin_risk_at_least_one_exac_lower_CI,
                                        # azithromycin_predicted_exac_probability_upper_CI        = azithromycin_risk_at_least_one_exac_upper_CI,
                                        #
                                        azithromycin_predicted_exac_rate                        = azithromycin_Rate_Adj,
                                        azithromycin_predicted_exac_rate_lower_PI               = azithromycin_Rate_Adj_lower_PI,
                                        azithromycin_predicted_exac_rate_upper_PI               = azithromycin_Rate_Adj_upper_PI,
                                        #azithromycin_predicted_exac_rate_lower_CI               = azithromycin_Rate_Adj_lower_CI,
                                        #azithromycin_predicted_exac_rate_upper_CI               = azithromycin_Rate_Adj_upper_CI,
                                        #
                                        azithromycin_predicted_severe_exac_probability          = azithromycin_risk_at_least_one_Sev_exac,
                                        azithromycin_predicted_severe_exac_probability_lower_PI = azithromycin_risk_at_least_one_Sev_exac_lower_PI,
                                        azithromycin_predicted_severe_exac_probability_upper_PI = azithromycin_risk_at_least_one_Sev_exac_upper_PI,
                                        # azithromycin_predicted_severe_exac_probability_lower_CI = azithromycin_risk_at_least_one_Sev_exac_lower_CI,
                                        # azithromycin_predicted_severe_exac_probability_upper_CI = azithromycin_risk_at_least_one_Sev_exac_upper_CI,
                                        #
                                        azithromycin_predicted_severe_exac_rate                 = azithromycin_Rate_non_Sev_Adj,
                                        azithromycin_predicted_severe_exac_rate_lower_PI        = azithromycin_Rate_non_Sev_Adj_lower_PI,
                                        azithromycin_predicted_severe_exac_rate_upper_PI        = azithromycin_Rate_non_Sev_Adj_upper_PI
                                        # azithromycin_predicted_severe_exac_rate_lower_CI        = azithromycin_Rate_non_Sev_Adj_lower_CI,
                                        # azithromycin_predicted_severe_exac_rate_upper_CI        = azithromycin_Rate_non_Sev_Adj_upper_CI,

  )

  return(patientData)

}



#' Predicts COPD exacerbation rate by severity level
#' @param patientData patient data matrix. Can have one or many patients in it
#' @param random_sampling_N number of random sampling. Default is 100.
#' @param lastYrExacCol the column specifying last year all exacerbation count
#' @param lastYrSevExacCol the column specifying last year severe exacerbation count
#' @param calculate_CIs whether to calculate confidence interval of the mean
#' @param ... for backward compatibility
#' @return patientData with prediction
#' @examples
#' results <- accept(samplePatients)
#' @export
accept <- function (patientData, random_sampling_N = 1e2, lastYrExacCol="LastYrExacCount",
                           lastYrSevExacCol="LastYrSevExacCount", calculate_CIs = FALSE, ...){

  betas <- list()
  betas$gamma	                    <- 0.9687
  betas$b0	                      <- -0.00964
  betas$b_male	                  <- -0.157
  betas$b_age10	                  <- -0.01885
  betas$b_nowsmk	                <- -0.2009
  betas$b_oxygen	                <- 0.08781
  betas$b_fev1pp100	              <- -0.4419
  betas$b_sgrq10                  <- 0.103
  betas$b_cardiovascular	        <- 0.09837
  betas$b_randomized_azithromycin <- -0.1687
  betas$b_LAMA	                  <- 0.1485
  betas$b_LABA	                  <- 0.1216
  betas$b_ICS	                    <- 0.2232
  betas$b_randomized_LAMA	        <- 0.172
  betas$b_randomized_LABA	        <- 0.1398
  betas$b_randomized_ICS	        <- -0.2452
  betas$b_randomized_statin	      <- -0.05617
  betas$b_BMI10                   <- -0.1272

  betas$c0	                      <- -3.973
  betas$c_male	                  <- 0.3889
  betas$c_age10	                  <- 0.1123
  betas$c_nowsmk                  <- 0.4025
  betas$c_oxygen                  <- 0.5558
  betas$c_fev1pp100	              <- -1.1552
  betas$c_sgrq10                  <- 0.205
  betas$c_cardiovascular	        <- 0.3255
  betas$c_randomized_azithromycin <- -0.1103
  betas$c_LAMA	                  <- -0.1385
  betas$c_LABA            	      <- 0.01246
  betas$c_ICS	                    <- 0.3879
  betas$c_randomized_LAMA	        <- 0.1074
  betas$c_randomized_LABA	        <- -0.2253
  betas$c_randomized_ICS	        <- -0.1211
  betas$c_randomized_statin	      <- 0.109
  betas$c_BMI10           	      <- -0.106

  betas$v1 	<- 0.5968
  betas$v2	<- 2.3847
  betas$cov	<- 0.147

  # More accurate azithromycin therapy estimates from AJE paper (https://doi.org/10.1093/aje/kww085), Table 2
  betas$b_randomized_azithromycin <- 	 log(1/1.30)
  betas$c_randomized_azithromycin <- 	 log(0.93)

  results <- acceptEngine(patientData = patientData, betas = betas)

  return(results)


}


#' Predicts COPD exacerbation rate by severity level based on accept2 model
#' @param patientData patient data matrix. Can have one or many patients in it
#' @param random_sampling_N number of random sampling. Default is 100.
#' @param lastYrExacCol the column specifying last year all exacerbation count
#' @param lastYrSevExacCol the column specifying last year severe exacerbation count
#' @param calculate_CIs whether to calculate confidence interval of the mean
#' @param ... for backward compatibility
#' @return patientData with prediction
#' @examples
#' results <- accept(samplePatients)
#' @export
accept2 <- function (patientData, random_sampling_N = 1e2, lastYrExacCol="LastYrExacCount",
                    lastYrSevExacCol="LastYrSevExacCount", calculate_CIs = FALSE, ...){

  betas <- list()
  betas$gamma	                    <- 0.9706
  betas$b0	                      <- -0.2014
  betas$b_male	                  <- -0.1855
  betas$b_age10	                  <- -0.00823
  betas$b_nowsmk	                <- -0.1867
  betas$b_oxygen	                <- 0.1209
  betas$b_fev1pp100	              <- -0.5584
  betas$b_sgrq10                  <- 0.1064
  betas$b_cardiovascular	        <- 0.1359
  betas$b_randomized_azithromycin <- -0.1287
  betas$b_LAMA	                  <- 0.1678
  betas$b_LABA	                  <- 0.1137
  betas$b_ICS	                    <- 0.279
  betas$b_randomized_LAMA	        <- 0.2202
  betas$b_randomized_LABA	        <- 0.1321
  betas$b_randomized_ICS	        <- -0.2359
  betas$b_randomized_statin	      <- -0.1573
  betas$b_BMI10                   <- -0.1333

  betas$c0	                      <- -3.6901
  betas$c_male	                  <- 0.4255
  betas$c_age10	                  <- 0.09545
  betas$c_nowsmk                  <- 0.4211
  betas$c_oxygen                  <- 0.546
  betas$c_fev1pp100	              <- -0.8095
  betas$c_sgrq10                  <- 0.1781
  betas$c_cardiovascular	        <- 0.2326
  betas$c_randomized_azithromycin <- -0.1305
  betas$c_LAMA	                  <- -0.1638
  betas$c_LABA            	      <- 0.05466
  betas$c_ICS	                    <- 0.2677
  betas$c_randomized_LAMA	        <- 0.2193
  betas$c_randomized_LABA	        <- -0.4085
  betas$c_randomized_ICS	        <- -0.1755
  betas$c_randomized_statin	      <- 0.2169
  betas$c_BMI10           	      <- -0.09666

  betas$v1 	<- 0.6855
  betas$v2	<- 2.2494
  betas$cov	<- 0.08772

  # More accurate azithromycin therapy estimates from AJE paper (https://doi.org/10.1093/aje/kww085), Table 2
  betas$b_randomized_azithromycin <- 	 log(1/1.30)
  betas$c_randomized_azithromycin <- 	 log(0.93)

  results_before_adj <- acceptEngine(patientData = patientData, betas = betas)


  rate_boundary_knots = c(0.3484506, 4.1066510)
  sev_boundary_knots = c(0.02767713, 1.74609740)

  rate_coeff <- c(0.031, 1.554, 3.514, 5.235 )
  sev_coeff <- c(1.167, 0.326, 0.028, 0.003)

  adj_predicted_exac_rate         <- Sp_Manual_Pred(results_before_adj$predicted_exac_rate, rate_coeff, rate_boundary_knots)
  adj_predicted_severe_exac_rate <- Sp_Manual_Pred(results_before_adj$predicted_severe_exac_rate, sev_coeff, sev_boundary_knots)


  results <- c(adj_predicted_exac_rate, adj_predicted_severe_exac_rate)
  return(results)

}


#' Predicts probability of observing n exacerbations in the next year
#' @param patientResults patient results vector, produced by accept.
#' @param n how many exacerbations
#' @param shortened boolean: Shortened results groups into 0, 1, 2, and 3 or more exacerbations
#' @return a matrix of probabilities with the number of exacerbations as rows and number of severe exacerbations as columns
#' @examples
#' results <- accept(samplePatients[1,])
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

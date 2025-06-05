binorm_pdf <- function(x, sigma) {
  s1 <- sqrt(sigma[1, 1])
  s2 <- sqrt(sigma[2, 2])
  rho <- sigma[1, 2] / (s1 * s2)
  Z <- x[1] ^ 2 / sigma[1, 1] - 2 * rho * x[1] * x[2] / (s1 * s2) + x[2] ^ 2 / sigma[2, 2]
  densRes <- 1 / (2 * pi * s1 * s2 * sqrt(1 - rho ^ 2)) * exp(- Z / (2 * (1 - rho ^ 2)))
  return(densRes)
}

Sp_Manual_Pred <- function(Predictor, CoefEst, knots, Boundary_knots) {
  ns_obj <- ns(Predictor, knots = knots, Boundary.knots = Boundary_knots)
  if (length(Predictor) == 1) BasisFuncs <- t(c(1, as.numeric(ns_obj)))
  else BasisFuncs <- as.matrix(cbind(1, ns_obj), ncol = 4)
  Preds <- as.vector(BasisFuncs %*% matrix(CoefEst, ncol = 1))
  return(Preds)
}

Sp_Manual_Vec <- function(data, CoefEst, CoefEst_sev, knots, knots_sev,
                          Boundary_knots, Boundary_knots_sev) {

  data$predicted_exac_rate <- ifelse(Sp_Manual_Pred(data$predicted_exac_rate, CoefEst, knots, Boundary_knots) < 0, 0,
                                     Sp_Manual_Pred(data$predicted_exac_rate, CoefEst, knots, Boundary_knots))
  data$predicted_exac_rate_lower_PI <- ifelse(Sp_Manual_Pred(data$predicted_exac_rate_lower_PI, CoefEst, knots, Boundary_knots) < 0, 0,
                                              Sp_Manual_Pred(data$predicted_exac_rate_lower_PI, CoefEst, knots, Boundary_knots))
  data$predicted_exac_rate_upper_PI <- ifelse(Sp_Manual_Pred(data$predicted_exac_rate_upper_PI, CoefEst, knots, Boundary_knots) < 0, 0,
                                              Sp_Manual_Pred(data$predicted_exac_rate_upper_PI, CoefEst, knots, Boundary_knots))
  data$predicted_severe_exac_rate <- ifelse(Sp_Manual_Pred(data$predicted_severe_exac_rate, CoefEst_sev, knots_sev, Boundary_knots_sev) < 0, 0,
                                            Sp_Manual_Pred(data$predicted_severe_exac_rate, CoefEst_sev, knots_sev, Boundary_knots_sev))
  data$predicted_severe_exac_rate_lower_PI <- ifelse(Sp_Manual_Pred(data$predicted_severe_exac_rate_lower_PI, CoefEst_sev, knots_sev, Boundary_knots_sev) < 0, 0,
                                                     Sp_Manual_Pred(data$predicted_severe_exac_rate_lower_PI, CoefEst_sev, knots_sev, Boundary_knots_sev))
  data$predicted_severe_exac_rate_upper_PI <- ifelse(Sp_Manual_Pred(data$predicted_severe_exac_rate_upper_PI, CoefEst_sev, knots_sev, Boundary_knots_sev) < 0, 0,
                                                     Sp_Manual_Pred(data$predicted_severe_exac_rate_upper_PI, CoefEst_sev, knots_sev, Boundary_knots_sev))

  data$predicted_exac_probability <- 1 - exp(-data$predicted_exac_rate)
  data$predicted_exac_probability_lower_PI <- 1 - exp(-data$predicted_exac_rate_lower_PI)
  data$predicted_exac_probability_upper_PI <- 1 - exp(-data$predicted_exac_rate_upper_PI)
  data$predicted_severe_exac_probability <- 1 - exp(-data$predicted_severe_exac_rate)
  data$predicted_severe_exac_probability_lower_PI <- 1 - exp(-data$predicted_severe_exac_rate_lower_PI)
  data$predicted_severe_exac_probability_upper_PI <- 1 - exp(-data$predicted_severe_exac_rate_upper_PI)

  # For azithromycin
  data$azithromycin_predicted_exac_rate <-                ifelse(Sp_Manual_Pred(data$azithromycin_predicted_exac_rate, CoefEst, knots, Boundary_knots) < 0, 0,
                                                                 Sp_Manual_Pred(data$azithromycin_predicted_exac_rate, CoefEst, knots, Boundary_knots))
  data$azithromycin_predicted_exac_rate_lower_PI <-       ifelse(Sp_Manual_Pred(data$azithromycin_predicted_exac_rate_lower_PI, CoefEst, knots, Boundary_knots) < 0, 0,
                                                                 Sp_Manual_Pred(data$azithromycin_predicted_exac_rate_lower_PI, CoefEst, knots, Boundary_knots))
  data$azithromycin_predicted_exac_rate_upper_PI <-       ifelse(Sp_Manual_Pred(data$azithromycin_predicted_exac_rate_upper_PI, CoefEst, knots, Boundary_knots) < 0, 0,
                                                                 Sp_Manual_Pred(data$azithromycin_predicted_exac_rate_upper_PI, CoefEst, knots, Boundary_knots))
  data$azithromycin_predicted_severe_exac_rate <-         ifelse(Sp_Manual_Pred(data$azithromycin_predicted_severe_exac_rate, CoefEst_sev, knots_sev, Boundary_knots_sev) < 0, 0,
                                                                 Sp_Manual_Pred(data$azithromycin_predicted_severe_exac_rate, CoefEst_sev, knots_sev, Boundary_knots_sev))
  data$azithromycin_predicted_severe_exac_rate_lower_PI <- ifelse(Sp_Manual_Pred(data$azithromycin_predicted_severe_exac_rate_lower_PI, CoefEst_sev, knots_sev, Boundary_knots_sev) < 0, 0,
                                                                  Sp_Manual_Pred(data$azithromycin_predicted_severe_exac_rate_lower_PI, CoefEst_sev, knots_sev, Boundary_knots_sev))
  data$azithromycin_predicted_severe_exac_rate_upper_PI <- ifelse(Sp_Manual_Pred(data$azithromycin_predicted_severe_exac_rate_upper_PI, CoefEst_sev, knots_sev, Boundary_knots_sev) < 0, 0,
                                                                  Sp_Manual_Pred(data$azithromycin_predicted_severe_exac_rate_upper_PI, CoefEst_sev, knots_sev, Boundary_knots_sev))

  data$azithromycin_predicted_exac_probability <- 1 - exp(-data$azithromycin_predicted_exac_rate)
  data$azithromycin_predicted_exac_probability_lower_PI <- 1 - exp(-data$azithromycin_predicted_exac_rate_lower_PI)
  data$azithromycin_predicted_exac_probability_upper_PI <- 1 - exp(-data$azithromycin_predicted_exac_rate_upper_PI)
  data$azithromycin_predicted_severe_exac_probability <- 1 - exp(-data$azithromycin_predicted_severe_exac_rate)
  data$azithromycin_predicted_severe_exac_probability_lower_PI <- 1 - exp(-data$azithromycin_predicted_severe_exac_rate_lower_PI)
  data$azithromycin_predicted_severe_exac_probability_upper_PI <- 1 - exp(-data$azithromycin_predicted_severe_exac_rate_upper_PI)

  return(data)
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
                          lastYrSevExacCol="LastYrSevExacCount", betas = NULL, KeepSGRQ = TRUE, KeepMeds = TRUE){

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

  # Add azithromycin indicator
  patientData$azithromycin_ind <- 0
  patientData_temp <- patientData
  patientData_temp$azithromycin_ind <- 1
  patientData <- rbind(patientData, patientData_temp)
  rm(patientData_temp)

  patientData <- patientData %>% mutate (log_alpha = b0 +
                                           b_male                    * .data$male   +
                                           b_age                     * .data$age    +
                                           b_nowsmk                  * .data$smoker +
                                           b_oxygen                  * .data$oxygen +
                                           b_fev1                    * .data$FEV1   +
                                           b_SGRQ                    * .data$SGRQ   +
                                           b_cardiovascular          * .data$statin +
                                           b_LAMA                    * .data$LAMA   +
                                           b_LABA                    * .data$LABA   +
                                           b_ICS                     * .data$ICS    +
                                           b_BMI                     * .data$BMI    +
                                           b_randomized_azithromycin * .data$azithromycin_ind)       %>%
    mutate (c_lin = c0 +
              c_male                    * .data$male   +
              c_age                     * .data$age    +
              c_nowsmk                  * .data$smoker +
              c_oxygen                  * .data$oxygen +
              c_fev1                    * .data$FEV1   +
              c_SGRQ                    * .data$SGRQ   +
              c_cardiovascular          * .data$statin +
              c_LAMA                    * .data$LAMA   +
              c_LABA                    * .data$LABA   +
              c_ICS                     * .data$ICS    +
              c_BMI                     * .data$BMI +
              b_randomized_azithromycin * .data$azithromycin_ind)


  RE_seq_1 = seq(from = -2 * covMat[1, 1], to = 2 * covMat[1, 1], length.out = random_sampling_N)
  RE_seq_2 = seq(from = -2 * covMat[2, 2], to = 2 * covMat[2, 2], length.out = random_sampling_N)
  RE_W_mat <- outer(X = RE_seq_1, Y = RE_seq_2, FUN = Vectorize(function(x, y) binorm_pdf(c(x, y), sigma = covMat)))

  Lambda  <- (exp(as.matrix(patientData[, "log_alpha"], ncol = 1)) %*% matrix(exp(RE_seq_1), nrow = 1)) ^ gamma
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


  patientData <- patientData %>%
    select(-"log_alpha", -"c_lin") %>%
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

           predicted_severe_exac_rate                 = Rate_Sev_Adj,
           predicted_severe_exac_rate_lower_PI        = Rate_Sev_Adj_lower_PI,
           predicted_severe_exac_rate_upper_PI        = Rate_Sev_Adj_upper_PI
           # predicted_severe_exac_rate_lower_CI        = Rate_Sev_Adj_lower_CI,
           # predicted_severe_exac_rate_upper_CI        = Rate_Sev_Adj_upper_CI,
    )

  patientData <-
    reshape(as.data.frame(patientData),
            timevar = "azithromycin_ind", idvar = "ID", direction = "wide",
            v.names = c("predicted_exac_probability", "predicted_exac_probability_lower_PI",
                        "predicted_exac_probability_upper_PI", "predicted_exac_rate",
                        "predicted_exac_rate_lower_PI", "predicted_exac_rate_upper_PI",
                        "predicted_severe_exac_probability", "predicted_severe_exac_probability_lower_PI",
                        "predicted_severe_exac_probability_upper_PI", "predicted_severe_exac_rate",
                        "predicted_severe_exac_rate_lower_PI", "predicted_severe_exac_rate_upper_PI"))

  colnames(patientData)[grep(".0", colnames(patientData), fixed = T)] <-
    unlist(strsplit(colnames(patientData)[grep(".0", colnames(patientData), fixed = T)], ".0"))
  colnames(patientData)[grep(".1", colnames(patientData), fixed = T)] <-
    paste0("azithromycin_",
           unlist(strsplit(colnames(patientData)[grep(".1", colnames(patientData), fixed = T)], ".1")))

  return(patientData)

}




#' Predicts COPD exacerbation rate by severity level based on Acute COPD Exacerbation Tool (ACCEPT)
#' @param patientData patient data matrix. Can have one or many patients in it
#' @param random_sampling_N number of random sampling. Default is 100.
#' @param lastYrExacCol the column specifying last year all exacerbation count
#' @param lastYrSevExacCol the column specifying last year severe exacerbation count
#' @param ... for backward compatibility
#' @return patientData with prediction
#' @examples
#' results <- accept1(samplePatients)
#' @export
accept1 <- function (patientData, random_sampling_N = 1e2, lastYrExacCol="LastYrExacCount",
                     lastYrSevExacCol="LastYrSevExacCount", ...){

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

  results <- acceptEngine(patientData = patientData, betas = betas)

  return(results)
}

#' Predicts COPD exacerbation rate by severity level based on the updated accept2 model, which improves accuracy in patients without an exacerbation history.
#' @param patientData patient data matrix. Can have one or many patients in it
#' @param random_sampling_N number of random sampling. Default is 100.
#' @param lastYrExacCol the column specifying last year all exacerbation count
#' @param lastYrSevExacCol the column specifying last year severe exacerbation count
#' @param KeepSGRQ default is TRUE. If set to false, the reduced model without SGRQ will be used.
#' @param KeepMeds default is TRUE. If set to false, the reduced model without medication predictors will be used.
#' @param ... for backward compatibility
#' @return patientData with prediction
#'
#' @importFrom splines ns
#' @importFrom stats reshape
#'
#' @examples
#' results <- accept2(samplePatients)
#' @export
accept2 <- function (patientData, random_sampling_N = 1e2, lastYrExacCol = "LastYrExacCount",
                     lastYrSevExacCol = "LastYrSevExacCount", KeepSGRQ = TRUE, KeepMeds = TRUE, ...){

  betas <- list()

  if (KeepSGRQ & KeepMeds){
    # model coefficients
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

    # Spline coefficients: Rates
    rate_knots = c(0.7145958, 0.9912697, 1.4652412)
    rate_boundary_knots = c(0.329367, 4.068048)
    sev_knots = c(0.1104125, 0.1553361, 0.2440743)
    sev_boundary_knots = c(0.02618769, 1.72861911)

    rate_coeff <- c(0.06518199, 0.90307795, 2.31804966, 4.23148175, 5.03274433)
    sev_coeff <- c(0.05010488, 0.10449992, 0.82837734, 1.76211800, 2.45999937)

  } else if (!KeepMeds & KeepSGRQ)
  {
    message ("Warning: You are using a simplified version of the model that does not include medications. See the manuscript for more details.")
    # model coefficients
    betas$gamma	                    <- 0.9707
    betas$b0	                      <- 0.1371
    betas$b_male	                  <- -0.1841
    betas$b_age10	                  <- -0.00501
    betas$b_nowsmk	                <- -0.2136
    betas$b_oxygen	                <- 0.1381
    betas$b_fev1pp100	              <- -0.6955
    betas$b_sgrq10                  <- 0.1111
    betas$b_cardiovascular	        <- 0.1835
    betas$b_randomized_azithromycin <- -0.06597
    betas$b_LAMA	                  <- 0
    betas$b_LABA	                  <- 0
    betas$b_ICS	                    <- 0
    betas$b_randomized_LAMA	        <- 0.2385
    betas$b_randomized_LABA	        <- 0.1318
    betas$b_randomized_ICS	        <- -0.2669
    betas$b_randomized_statin	      <- -0.2646
    betas$b_BMI10                   <- -0.1352

    betas$c0	                      <- -3.6522
    betas$c_male	                  <- 0.4285
    betas$c_age10	                  <- 0.09704
    betas$c_nowsmk                  <- 0.4338
    betas$c_oxygen                  <- 0.539
    betas$c_fev1pp100	              <- -0.7998
    betas$c_sgrq10                  <- 0.1778
    betas$c_cardiovascular	        <- 0.2554
    betas$c_randomized_azithromycin <- -0.09003
    betas$c_LAMA	                  <- 0
    betas$c_LABA	                  <- 0
    betas$c_ICS	                    <- 0
    betas$c_randomized_LAMA	        <- 0.2093
    betas$c_randomized_LABA	        <- -0.4087
    betas$c_randomized_ICS	        <- -0.1739
    betas$c_randomized_statin	      <- 0.08586
    betas$c_BMI10           	      <- -0.08287

    betas$v1 	<- 0.7083
    betas$v2	<- 2.2753
    betas$cov	<- 0.09212

    # Spline coefficients: Rates
    rate_knots = c(0.1018911, 0.1420318, 0.2163109)
    rate_boundary_knots = c(0.03407203, 1.56146110)
    sev_knots = c(0.6866579, 0.9167824, 1.3328795)
    sev_boundary_knots = c(0.3993395, 3.7012484)

    rate_coeff <- c(0.1197930, 0.9126978, 2.2951754, 4.0110488, 4.9058539)
    sev_coeff <- c(0.05871868, 0.10250238, 0.82620839, 1.68624119, 2.38272234)

    # set excluded covariates to zero
    patientData$LAMA <- 0
    patientData$LABA <- 0
    patientData$ICS <- 0

  } else if (KeepMeds & !KeepSGRQ)
  {
    message ("Warning: You are using a simplified version of the model that does not include St. George Respiratory Questionnaire. See the manuscript for more details.")
    # model coefficients
    betas$gamma	                    <- 0.97
    betas$b0	                      <- 0.5072
    betas$b_male	                  <- -0.1791
    betas$b_age10	                  <- -0.040983
    betas$b_nowsmk	                <- -0.1353
    betas$b_oxygen	                <- 0.1561
    betas$b_fev1pp100	              <- -0.7846
    betas$b_sgrq10                  <- 0
    betas$b_cardiovascular	        <- 0.1642
    betas$b_randomized_azithromycin <- -0.1259
    betas$b_LAMA	                  <- 0.1808
    betas$b_LABA	                  <- 0.0933
    betas$b_ICS	                    <- 0.3052
    betas$b_randomized_LAMA	        <- 0.2451
    betas$b_randomized_LABA	        <- 0.1034
    betas$b_randomized_ICS	        <- -0.2339
    betas$b_randomized_statin	      <- -0.1575
    betas$b_BMI10                   <- -0.1088

    betas$c0	                      <- -2.4146
    betas$c_male	                  <- 0.4525
    betas$c_age10	                  <- 0.03839
    betas$c_nowsmk                  <- 0.51
    betas$c_oxygen                  <- 0.6046
    betas$c_fev1pp100	              <- -1.2895
    betas$c_sgrq10                  <- 0
    betas$c_cardiovascular	        <- 0.2882
    betas$c_randomized_azithromycin <- -0.1343
    betas$c_LAMA	                  <- -0.1531
    betas$c_LABA            	      <- 0.01885
    betas$c_ICS	                    <- 0.3003
    betas$c_randomized_LAMA	        <- 0.2378
    betas$c_randomized_LABA	        <- -0.4569
    betas$c_randomized_ICS	        <- -0.171
    betas$c_randomized_statin	      <- 0.1928
    betas$c_BMI10           	      <- -0.06825

    betas$v1 	<- 0.7161
    betas$v2	<- 2.3299
    betas$cov	<- 0.1465

    # Spline coefficients: Rates
    rate_knots = c(0.7456274, 0.9866093, 1.4210972)
    rate_boundary_knots = c(0.4203808, 3.7239562)
    sev_knots = c(0.1137685, 0.1493893, 0.2106265)
    sev_boundary_knots = c(0.04448195, 1.56042213)

    rate_coeff <- c(0.1650515, 0.8529724, 2.0886867, 3.9422245, 4.9643087)
    sev_coeff <- c(0.05590748, 0.09148651, 0.86588143, 1.69546615, 2.33351171)

    # set excluded covariates to zero
    patientData$SGRQ <- 0

  } else if (!KeepMeds & !KeepSGRQ)
  {
    message ("Warning: You are using a simplified version of the model that does includes neither medications nor St. George Respiratory Questionnaire. See the manuscript for more details.")
    # model coefficients
    betas$gamma	                    <- 0.9701
    betas$b0	                      <- 0.8911
    betas$b_male	                  <- -0.1791
    betas$b_age10	                  <- -0.0397
    betas$b_nowsmk	                <- -0.1616
    betas$b_oxygen	                <- 0.1777
    betas$b_fev1pp100	              <- -0.9282
    betas$b_sgrq10                  <- 0
    betas$b_cardiovascular	        <- 0.216
    betas$b_randomized_azithromycin <- -0.05931
    betas$b_LAMA	                  <- 0
    betas$b_LABA	                  <- 0
    betas$b_ICS	                    <- 0
    betas$b_randomized_LAMA	        <- 0.2871
    betas$b_randomized_LABA	        <- 0.09825
    betas$b_randomized_ICS	        <- -0.2612
    betas$b_randomized_statin	      <- -0.2646
    betas$b_BMI10                   <- -0.1112

    betas$c0	                      <- -2.3819
    betas$c_male	                  <- 0.45
    betas$c_age10	                  <- 0.0423
    betas$c_nowsmk                  <- 0.5255
    betas$c_oxygen                  <- 0.6051
    betas$c_fev1pp100	              <- -1.3091
    betas$c_sgrq10                  <- 0
    betas$c_cardiovascular	        <- 0.3234
    betas$c_randomized_azithromycin <- -0.08298
    betas$c_LAMA	                  <- 0
    betas$c_LABA	                  <- 0
    betas$c_ICS	                    <- 0
    betas$c_randomized_LAMA	        <- 0.2853
    betas$c_randomized_LABA	        <- -0.475
    betas$c_randomized_ICS	        <- -0.1957
    betas$c_randomized_statin	      <- 0.07671
    betas$c_BMI10           	      <- -0.06022

    betas$v1 	<- 0.7424
    betas$v2	<- 2.3784
    betas$cov	<- 0.1564

    # Spline coefficients: Rates
    rate_knots = c(0.7028951, 0.9131159, 1.3158333)
    rate_boundary_knots = c(0.4924904, 3.3967642)
    sev_knots = c(0.1020546, 0.1304339, 0.1826366)
    sev_boundary_knots = c(0.05212108, 1.39483400)

    rate_coeff <- c(0.1513950, 0.9446998, 2.2107619, 3.9384999, 4.8656656)
    sev_coeff <- c(0.05768919, 0.12920786, 0.63391906, 1.49899418, 2.24064269)

    # set excluded covariates to zero
    patientData$LAMA <- 0
    patientData$LABA <- 0
    patientData$ICS <- 0
    patientData$SGRQ <- 0
  }
  # More accurate azithromycin therapy estimates from AJE paper (https://doi.org/10.1093/aje/kww085), Table 2
  betas$b_randomized_azithromycin <- 	 log(1/1.30)
  betas$c_randomized_azithromycin <- 	 log(0.93)

  results_before_adj <- acceptEngine(patientData = patientData, betas = betas, KeepMeds = KeepMeds, KeepSGRQ = KeepSGRQ)

  results_after_adj <- Sp_Manual_Vec(data = results_before_adj,
                                     CoefEst = rate_coeff, CoefEst_sev = sev_coeff,
                                     knots = rate_knots, knots_sev = sev_knots,
                                     Boundary_knots = rate_boundary_knots,
                                     Boundary_knots_sev = sev_boundary_knots)

  if (! KeepSGRQ) results_after_adj$SGRQ <- NULL
  if (! KeepMeds) {
    results_after_adj$ICS <- NULL
    results_after_adj$LAMA <- NULL
    results_after_adj$LABA <- NULL
  }

  return(results_after_adj)
}



#' A flexible version of ACCEPT 2.0 model, which imputes predictors using MICE approach.
#'
#' @param newdata new patient data with missing values to be imputed before prediction with the same format as accept samplePatients.
#' @param format default is "tibble". Can also be set to "json".
#' @param version indicates which version of ACCEPT needs to be called. Options include "accept1", "accept2", and "flexccept"
#' @param prediction_interval default is FALSE. If set to TRUE, returns prediction intervals of the predictions.
#' @param return_predictors default is FALSE. IF set to TRUE, returns the predictors along with prediction results.
#' @param ... for other versions of accept.
#' @return patientData with prediction.
#'
#' @importFrom splines ns
#' @importFrom stats reshape
#'
#' @examples
#' results <- accept(newdata = samplePatients)
#' @export
accept <- function(newdata, format="tibble",  version = "accept2", prediction_interval = FALSE, return_predictors = FALSE, ...) {

  if (format=="json") {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("Package \"jsonlite\" needed for this function to work. Please install it.",
           call. = FALSE)
    }
    newdata<-as_tibble(jsonlite::fromJSON(newdata))
  }

  if (!is_tibble(newdata)) {stop("Wrong input format. Only `tibble` and `json` formats are supported. Make sure format is set to 'json' if the input data is in json.")}
  if (any(newdata$FEV1>=120) | any(newdata$FEV1<10)) warning("Unusually high or low FEV1 values detected. Ensure you are passing percent predicted values between 10 to 110 ")

  if (version == "accept1") {
    return(accept1(newdata, ...))
  }
  if (version == "accept2") {
    return(accept2(newdata, ...))
  }

  samplePatients_colNames <- c("ID", "male", "age", "smoker", "oxygen",
                               "statin", "LAMA", "LABA", "ICS", "FEV1",
                               "BMI", "SGRQ", "LastYrExacCount",
                               "LastYrSevExacCount")

  samplePatients_colNames <- samplePatients_colNames[! grepl("randomized_|Last", samplePatients_colNames)]

  if (! "SGRQ" %in% colnames(newdata)) {
    if ("CAT" %in% colnames(newdata)) {
      warning("SGRQ score not found. Using CAT score instead of SGRQ")
      newdata$SGRQ <- 18.87 + 1.53 * newdata$CAT
    }
    else {
      if("mMRC" %in% colnames(newdata)) {
        message("SGRQ score not found. Using mMRC score instead of SGRQ")
        newdata$SGRQ <- 20.43 + 14.77 * newdata$mMRC
      }
    }
  }
  if (all(! is.na(newdata$SGRQ))) {
    if (! all(c("CAT", "mMRC") %in% colnames(newdata))) newdata$CAT <- newdata$SGRQ / 1.53 - 18.87 / 1.53
  }

  newdata_temp <- newdata[samplePatients_colNames]

  newdata_colNames <- colnames(newdata)
  KeepSGRQ_flag <- TRUE
  KeepMeds_flag <- TRUE
  if (any(is.na(newdata[ , c("LAMA", "LABA", "ICS")]))) {
    KeepMeds_flag <- FALSE
    newdata_colNames <- newdata_colNames[! newdata_colNames %in% c("LAMA", "LABA", "ICS")]
  }
  if (any(is.na(newdata$SGRQ))) {
    KeepSGRQ_flag <- FALSE
    newdata_colNames <- newdata_colNames[newdata_colNames != "SGRQ"]
  }


  colNames_missing <- sort(colnames(newdata_temp)[apply(newdata_temp, 2, function(x) any(is.na(x)))])
  colNames_complete <- sort(samplePatients_colNames[! samplePatients_colNames %in% c("ID", colNames_missing)])
  if (any(c("LAMA", "LABA", "ICS", "SGRQ") %in% colNames_missing)) {
    colNames_missing <- colNames_missing[! colNames_missing %in% c("LAMA", "LABA", "ICS", "SGRQ")]
  }

  if (length(colNames_missing) > 0) {
    for (i in 1 : length(colNames_missing)) {
      res_temp <- colNames_missing[i]
      model_temp <-
        model_list[model_list$response == res_temp &
                     model_list$predictors == paste(colNames_complete, collapse = ",") , ]
      if (res_temp %in% c("male", "smoker", "oxygen", "statin")) {
        pred_temp <-
          cbind(1, as.matrix(data.frame(newdata_temp[ , unlist(strsplit(model_temp$predictors, split = ","))]))) %*%
          matrix(unlist(model_temp$coef), ncol = 1)
        pred_temp <- as.vector(round(exp(pred_temp) / (1 + exp(pred_temp))))
      }
      else {
        pred_temp <-
          cbind(1, as.matrix(data.frame(newdata_temp[ , unlist(strsplit(model_temp$predictors, split = ","))]))) %*%
          matrix(unlist(model_temp$coef), ncol = 1)
        pred_temp <- as.vector(pred_temp)
      }
      newdata_temp[ , res_temp] <- pred_temp
      colNames_complete <- sort(c(colNames_complete, res_temp))
    }
  }

  ## Obtain ACCEPT 2 predictions for each set of imputed dataset
  if (any(colnames(newdata) %in% colNames_missing)) {
    acceptPreds <- newdata[ , ! (colnames(newdata) %in% colNames_missing)]
  }
  else {
    acceptPreds <- newdata
  }
  if (length(colNames_missing) > 0) {
    warning(paste0("Missing column(s) detected. Imputing the following columns: ", colNames_missing))
    acceptPreds <- merge(acceptPreds, newdata_temp[ , c("ID", colNames_missing)],
                         by = "ID")
  }
  acceptPreds <- accept2(patientData = acceptPreds,
                         KeepSGRQ = KeepSGRQ_flag,
                         KeepMeds = KeepMeds_flag)
  if (prediction_interval) {
    acceptPreds <- acceptPreds[ , c(newdata_colNames,
                                    "predicted_exac_probability",
                                    "predicted_exac_probability_lower_PI", "predicted_exac_probability_upper_PI",
                                    "predicted_exac_rate",
                                    "predicted_exac_rate_lower_PI", "predicted_exac_rate_upper_PI",
                                    "predicted_severe_exac_probability",
                                    "predicted_severe_exac_probability_lower_PI", "predicted_severe_exac_probability_upper_PI",
                                    "predicted_severe_exac_rate",
                                    "predicted_severe_exac_rate_lower_PI", "predicted_severe_exac_rate_upper_PI")]
  }
  else {
    acceptPreds <- acceptPreds[ , c(newdata_colNames,
                                    "predicted_exac_probability", "predicted_exac_rate",
                                    "predicted_severe_exac_probability", "predicted_severe_exac_rate")]
  }

  if (!return_predictors) {
    acceptPreds <- acceptPreds %>% select(starts_with("predicted_"))
  }

  return(acceptPreds)
}



#' Predicts probability of observing n exacerbations in the next year
#' @param patientResults patient results vector, produced by accept.
#' @param n how many exacerbations
#' @param shortened boolean: Shortened results groups into 0, 1, 2, and 3 or more exacerbations
#' @return a matrix of probabilities with the number of exacerbations as rows and number of severe exacerbations as columns
#'
#' @examples
#' results <- accept2(samplePatients[1,])
#' predictCountProb (results)
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



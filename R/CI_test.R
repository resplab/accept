# simulatePatient <- function(n=50){
#   patient <- accept::samplePatients
#
#   for (i in 1 : n) {
#
#     patient <- rbind(patient, NA)
#     patient$ID[i] <- i
#
#     if (runif(1, min = 0, max = 1) >= 0.5) {
#       patient$male[i] <- 0
#     } else {patient$male[i] <- 1}
#
#     if (runif(1, min = 0, max = 1) >= 0.258) {
#       patient$smoker[i] <- 0
#     } else {patient$smoker[i] <- 1}
#
#     if (runif(1, min = 0, max = 1) >= 0.4685) {
#       patient$oxygen[i] <- 0
#     } else {patient$oxygen[i] <- 1}
#
#     if (runif(1, min = 0, max = 1) >= 0.2265) {
#       patient$statin[i] <- 0
#     } else {patient$statin[i] <- 1}
#
#     if (runif(1, min = 0, max = 1) >= 0.6504) {
#       patient$LAMA[i] <- 0
#     } else {patient$LAMA[i] <- 1}
#
#     if (runif(1, min = 0, max = 1) >= 0.5206) {
#       patient$LABA[i]<- 0
#     } else {patient$LABA[i] <- 1}
#
#     if (runif(1, min = 0, max = 1) >= 0.5723) {
#       patient$ICS = 0
#     } else {patient$ICS[i] <- 1}
#
#     patient$age[i] <- round(rnorm(1, mean = 64.68, sd = 8.75))
#     patient$FEV1[i] <- round(rnorm(1, mean = 40.60, sd = 15.93))
#     patient$BMI[i] <- round(rnorm(1, mean = 27.53, sd = 6.43))
#     patient$SGRQ[i] <- round(rnorm(1, mean = 49.95, sd = 16.72))
#
#     #patient$LastYrExacCount[i] <- rpois (n = 1, lambda = 1.42)
#     #patient$LastYrSevExacCount[i] <- rpois (n = 1, lambda = 0.29)
#
#     covar <- cbind(c(2.55, 0.87), c(0.57, 0.44))
#     exacSample <- rmvpois(10, c(1.42, 0.29), covar)[1,]
#     patient$LastYrExacCount[i] <- exacSample[1]
#     patient$LastYrSevExacCount[i] <- exacSample[2]
#
#   }
#
#   patient[ , grep("randomized", colnames(patient))] <- 0
#
# return(patient[-c(n+1,n+2),])
# }
#
# patient <- simulatePatient()
#
#
#
# res <- accept::predictACCEPT(patient)
#
#
# S_E <- (res$predicted_exac_rate_upper_CI - res$predicted_exac_rate_lower_CI)/4
#
# hist(S_E)
#
# azi_rate <- res$azithromycin_predicted_severe_exac_rate / res$predicted_severe_exac_rate
# summary(azi_rate)
#
#
#
#
# azi_prob <- res$azithromycin_predicted_exac_probability / res$predicted_exac_probability
# summary(azi_prob)

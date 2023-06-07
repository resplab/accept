#' @import dplyr
#' @importFrom stats dpois quantile rpois dbinom weighted.mean
#' @importFrom reldist wtd.quantile
#' @importFrom splines bs
NULL

#' Sample Patient Characteristics Inputs
#'
#' A dataset containing sample patient characteristics to run the prediction model
#'  variables are as follows:
#'
#' \itemize{
#'   \item ID. A unique character string identifying a patients
#'   \item male. whether the patient is male
#'   \item age. the age of the patient (40--90)
#'   \item smoker. whether the patient is currently a smoker
#'   \item oxygen. whether the patient has had supplemental oxygen therapy within the past year (0,1)
#'   \item FEV1. forced expiratory volume in 1 second in percent predicted
#'   \item BMI. body mass index (10--60)
#'   \item SGRQ. St. George’s Respiratory Questionnaire score (0--100)
#'   \item statin. whether the patient is taking statins due to cardiovascular conditions
#'   \item LAMA. whether the patient is on long acting muscarinic antagonist
#'   \item LABA. whether the patient is on long acting beta agonist
#'   \item ICS. whether the patient is on inhaled corticosteroids
#'   \item LastYrExacCount. total number of exacerbations in the previous year
#'   \item LastYrSevExacCount. number of severe exacerbations in the previous year
#' }
#'
#' @docType data
#' @keywords datasets
#' @name samplePatients
#' @format A data frame with 2 rows and 19 variables
NULL

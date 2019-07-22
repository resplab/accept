#' Creates heatmap of number of exacerbations
#' @param patientResults patient results vector, produced by predictAccept.
#' @param n how many exacerbations to consider
#' @param shortened boolean
#' @return a heatmap
#' @examples
#' results <- predictACCEPT(samplePatients[1,])
#' plotHeatMap(results)
#' @export

plotHeatMap = function(patientResults, n = 10, shortened = TRUE) {

  results = predictCountProb(patientResults, n=n, shortened = shortened)
  heatPlotly <- t(results)

  plot_ly(x = colnames(heatPlotly),
          y = rownames(heatPlotly),
          z = heatPlotly, type = "heatmap", colors = colorRamp(c("steelblue4", "tomato")))  %>%
    layout(
      title = "Predicted Probability of Experiencing Certain Number of Exacerbations",
      yaxis = list(title = "Number of Severe Exacerbations"),
      xaxis = list(title = "Number of All Exacerbations")
    )
}
#' Creates bar graph comparing no treatment with azithromycin treatment
#' @param patientResults patient results vector, produced by predictAccept.
#' @param type string: either "probability" or "rate"
#' @param interval string: either "CI" or "PI"
#' PI = Predicted Interval
#' CI = Confidence Interval
#' @return a bar graph
#' @examples
#' results <- predictACCEPT(samplePatients[1,])
#' plotExacerbations(results)
#' @export

plotExacerbations = function(patientResults, type = "probability", interval = "CI") {
  themeColors = c("#330033", "#8cf2f2", "#c51672", "#007bff")
  exacerbations = c("Overall", "Severe")
  base_text = "predicted_exac_"
  severe_text = "predicted_severe_exac_"
  text_type = paste0(base_text, type)
  text_severe_type = paste0(severe_text, type)
  intervals = c(paste0(text_type, "_lower_", interval),
                 paste0(text_type, "_upper_", interval))
  azIntervals = paste0("azithromycin_", intervals)
  az_text_type = paste0("azithromycin_", text_type)
  az_text_severe_type = paste0("azithromycin", text_severe_type)
  noTreatment = c(patientResults[[text_type]], patientResults[[text_severe_type]])
  azithromycin = c(patientResults[[az_text_type]], patientResults[[az_text_severe_type]])
  data <- data.frame(exacerbations, noTreatment, azithromycin)
  title = paste0("Comparison of No Treatment vs Azithromycin")
  if(type == "probability") {
    yAxisTitle = "Probability of Exacerbation in Next Year"
  } else if(type == "rate") {
    yAxisTitle = "Predicted Exacerbation Rate (# exacerbations/year)"
  }

  p <- plot_ly(data, x = ~exacerbations, y = ~noTreatment, type = 'bar', name = 'No Treatment', marker = list(color = themeColors[2])) %>%
    add_trace(y = ~azithromycin, name = 'Azithromycin Treatment', marker = list(color = themeColors[3])) %>%
    layout(title = title,
           titlefont = list(
             size = 16,
             color = 'white'),
         xaxis = list(
           title = "Exacerbation Type",
           titlefont = list(
             size = 16,
             color = 'white'),
           tickfont = list(
             size = 14,
             color = 'white')),
         yaxis = list(
           title = yAxisTitle,
           titlefont = list(
             size = 16,
             color = 'white'),
           tickfont = list(
             size = 14,
             color = 'white')),
         legend = list(font=list(
                         size=12,
                         color='white'
                       )),
         barmode = 'group',
         bargap = 0.15,
          paper_bgcolor='#4C444B',
                plot_bgcolor='#4C444B')
  p
}

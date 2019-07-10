#' Creates heatmap of number of exacerbations
#' @param patientResults patient results vector, produced by predictAccept.
#' @param n how many exacerbations to consider
#' @param shortened boolean
#' @return a matrix of probabilities with the number of exacerbations as rows and number of severe exacerbations as columns
#' @import plotly
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

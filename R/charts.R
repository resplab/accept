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

#' Creates bar graph
#' @param patientResults patient results vector, produced by predictAccept.
#' @param type string: either "rate" or "probability"
#' @param interval string: either "CI" or "PI"
#' CI = confidence interval
#' PI = predicted interval
#' @param colors vector: vector of color strings, rbg or hexadecimal, for colors on bar graph
#' @return a bar graph
#' @import plotly
#' @examples
#' results <- predictACCEPT(samplePatients[1,])
#' plotExacerbations(results)
#' @export
plotExacerbations = function(patientResults, type="rate", interval = "CI",
                             colors = c("#007bff", 'rgb(204,204,204)')) {

  base_strings = c("predicted_exac_",
    "predicted_severe_exac_")
  full_strings = paste0(base_strings, type)
  az_strings = paste0("azithromycin_", full_strings)

  x = c('Overall', 'Severe')
  y1 = c(patientResults[[full_strings[1]]], patientResults[[full_strings[2]]])
  y2 = c(patientResults[[az_strings[1]]], patientResults[[az_strings[2]]])
  data <- data.frame(x, y1, y2)

  #The default order will be alphabetized unless specified as below:
  data$x <- factor(data$x, levels = data[["x"]])
  if(type == "rate") {
    yAxisTitle = paste0("Predicted Exacerbation Rate in Next Year")
  } else if (type == "probability") {
    yAxisTitle = "Probability of an Exacerbation in Next Year"
  }
  p <- plot_ly(data, x = ~x, y = ~y1, type = 'bar',
               name = 'No Treatment',
               marker = list(color = colors[1])) %>%
    add_trace(y = ~y2,
              name = 'Azithromycin Treatment',
              marker = list(color = colors[2])) %>%
    layout(xaxis = list(title = "Exacerbation Type",
                        tickangle = -45),
           yaxis = list(title = yAxisTitle),
           margin = list(b = 100),
           barmode = 'group')
  p
}

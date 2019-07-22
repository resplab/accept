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
          z = heatPlotly, type = "heatmap")  %>%
    layout(
      title = "Predicted Probability of Experiencing Certain Number of Exacerbations",
      yaxis = list(title = "Number of Severe Exacerbations"),
      xaxis = list(title = "Number of All Exacerbations")
    )
}
<<<<<<< HEAD
#' Creates bar graph comparing no treatment with azithromycin treatment
#' @param patientResults patient results vector, produced by predictAccept.
#' @param type string: either "probability" or "rate"
#' @param interval string: either "CI" or "PI"
#' PI = Predicted Interval
#' CI = Confidence Interval
#' @return a bar graph
=======

#' Creates bar graph
#' @param patientResults patient results vector, produced by predictAccept.
#' @param type string: either "rate" or "probability"
#' @param interval string: either "CI" or "PI"
#' CI = confidence interval
#' PI = predicted interval
#' @param colors vector: vector of color strings, rbg or hexadecimal, for colors on bar graph
#' @return a bar graph
#' @import plotly
>>>>>>> 637f1cddf5094488f08464221258468484c89d9d
#' @examples
#' results <- predictACCEPT(samplePatients[1,])
#' plotExacerbations(results)
#' @export
<<<<<<< HEAD

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
=======
plotExacerbations = function(patientResults, type="rate", interval = "CI",
                             colors = c("#007bff", 'rgb(204,204,204)')) {

  base_strings = c("predicted_exac_",
    "predicted_severe_exac_")
  full_strings = paste0(base_strings, type)
  az_strings = paste0("azithromycin_", full_strings)
  lower = "_lower_"
  upper = "_upper_"
  az_intervals = c(paste0(az_strings, lower, interval),
                     paste0(az_strings, upper, interval))
  no_t_intervals = c(paste0(full_strings, lower, interval),
                             paste0(full_strings, upper, interval))

  x = c('Overall', 'Severe')
  y1 = c(patientResults[[full_strings[1]]], patientResults[[full_strings[2]]])
  y2 = c(patientResults[[az_strings[1]]], patientResults[[az_strings[2]]])
  error_y1 = c(patientResults[[no_t_intervals[1]]],
               patientResults[[no_t_intervals[3]]],
               patientResults[[no_t_intervals[2]]],
               patientResults[[no_t_intervals[4]]])
  error_y2 = c(patientResults[[az_intervals[1]]],
               patientResults[[az_intervals[3]]],
               patientResults[[az_intervals[2]]],
               patientResults[[az_intervals[4]]])
  error_y1 = c(abs(error_y1[c(1,2)]-y1[1]),
               abs(error_y1[c(3,4)]-y1[2]))
  error_y2 = c(abs(error_y2[c(1,2)]-y2[1]),
               abs(error_y2[c(3,4)]-y2[2]))
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
               marker = list(color = colors[1]),
               error_y = list(
                 type='data',
                 symmetric=FALSE,
                 color='#85144B',
                 thickness=1.5,
                 width=3,
                 array= error_y1[c(2,4)],
                 arrayminus = error_y1[c(1,3)]
               )) %>%
    add_trace(y = ~y2,
              name = 'Azithromycin Treatment',
              marker = list(color = colors[2]),
              error_y = list(
                type='data',
                color='#85144B',
                thickness=1.5,
                width=3,
                symmetric=FALSE,
                array= error_y2[c(2,4)],
                arrayminus = error_y2[c(1,3)]
              )) %>%
    layout(xaxis = list(title = "Exacerbation Type",
                        tickangle = -45),
           yaxis = list(title = yAxisTitle),
           margin = list(b = 100),
           barmode = 'group')
>>>>>>> 637f1cddf5094488f08464221258468484c89d9d
  p
}

#' Creates heatmap of number of exacerbations
#' @param patientResults patient results vector, produced by accept.
#' @param n how many exacerbations to consider
#' @param shortened boolean
#' @return a heatmap
#' @examples
#' \dontrun{
#' results <- accept1(samplePatients[1,])
#' plotHeatMap(results)
#' }
#' @export

plotHeatMap <- function(patientResults, n = 10, shortened = TRUE) {

  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package \"plotly\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  results <- predictCountProb(patientResults, n=n, shortened = shortened)
  heatPlotly <- t(results)

  plotly::plot_ly(x = colnames(heatPlotly),
          y = rownames(heatPlotly),
          z = heatPlotly, type = "heatmap")  %>%
    plotly::layout(
      title = "Predicted Probability of Experiencing Certain Number of Exacerbations",
      yaxis = list(title = "Number of Severe Exacerbations"),
      xaxis = list(title = "Number of All Exacerbations")
    )
}
#' Creates bar graph comparing no treatment with azithromycin treatment
#' @param patientResults patient results vector, produced by accept.
#' @param type string: either "probability" or "rate"
#' @param interval string: either "CI" or "PI"
#' PI = Predicted Interval
#' CI = Confidence Interval
#' @param colors vector: a vector of colors to be used in the graph
#' must be length 2
#' can use hexadecimal, rgb, or R color codes
#' @return a bar graph
#' @examples
#' \dontrun{
#' results <- accept1(samplePatients[1,])
#' plotExacerbations(results)
#' }
#' @export
plotExacerbations <- function(patientResults, type="rate", interval = "PI",
                             colors = c("#007bff", 'rgb(204,204,204)')) {

  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package \"plotly\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

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
  if (is.null(y2)) y2 <- c(NA, NA)
  data <- data.frame(x, y1, y2)

  #The default order will be alphabetized unless specified as below:
  data$x <- factor(data$x, levels = data[["x"]])
  if(type == "rate") {
    yAxisTitle = paste0("Predicted Exacerbation Rate in Next Year")
  } else if (type == "probability") {
    yAxisTitle = "Probability of an Exacerbation in Next Year"
  }
  p <- plotly::plot_ly(data, x = ~x, y = ~y1, type = 'bar',
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
    plotly::add_trace(y = ~y2,
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
    plotly::layout(xaxis = list(title = "Exacerbation Type",
                        tickangle = -45),
           yaxis = list(title = yAxisTitle),
           margin = list(b = 100),
           barmode = 'group')
  p
}

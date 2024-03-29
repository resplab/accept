% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vetiver-compatibility.R
\name{handler_predict.accept_model}
\alias{handler_predict.accept_model}
\title{Model handler functions for API endpoint}
\usage{
\method{handler_predict}{accept_model}(vetiver_model, ...)
}
\arguments{
\item{vetiver_model}{A deployable [vetiver_model()] object}

\item{...}{any other inputs}
}
\value{
A `handler_startup` function should return invisibly, while a
`handler_predict` function should return a function with the signature
`function(req)`. The request body (`req$body`) consists of the new data
at prediction time; this function should return predictions either as a
tibble or as a list coercable to a tibble via [tibble::as_tibble()].
}
\description{
Useful for supporting accept_model class in vetiver
}

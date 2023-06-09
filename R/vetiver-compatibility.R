class(accept) <- c("accept_model", class(accept))


#' Model predictions
#'
#' @param object a model object
#' @param ... new data for which the prediction is needed
#' @return prediction results from ACCEPT model
#' @export
predict.accept_model <- function(object, ...) {
  accept(...)
}

#' Create a vetiver model object description
#'
#' @param model model name
#' @return the description of the model
#' @export
vetiver_create_description.accept_model <- function(model) {
  "The Acute COPD Exacerbation Prediction Tool (ACCEPT)"
}

#' Create a vetiver input data prototype
#'
#' @param model a vetiver model object
#' @param ... any other inputs
#' @return A `vetiver_ptype` method returns a zero-row dataframe, and
#' `vetiver_create_ptype()` returns either such a zero-row dataframe, `NULL`,
#' or the dataframe passed to `save_prototype`.
#' @export
vetiver_ptype.accept_model <- function(model, ...) {
  vctrs::vec_ptype(tibble::tibble(ID     = character(),
                                  male   = logical(),
                                  age    = integer(),
                                  smoker = logical(),
                                  oxygen = logical(),
                                  statin = logical(),
                                  LAMA   = logical(),
                                  LABA   = logical(),
                                  ICS    = logical(),
                                  FEV1   = double(),
                                  BMI    = double(),
                                  SGRQ   = integer(),
                                  LastYrExacCount = integer(),
                                  LastYrSevExacCount = integer()
  ))
}


#' Model handler functions for API endpoint
#'
#' Useful for supporting accept_model class in vetiver
#' @param vetiver_model A deployable [vetiver_model()] object
#' @param ... any other inputs
#' @return A `handler_startup` function should return invisibly, while a
#' `handler_predict` function should return a function with the signature
#' `function(req)`. The request body (`req$body`) consists of the new data
#' at prediction time; this function should return predictions either as a
#' tibble or as a list coercable to a tibble via [tibble::as_tibble()].
#' @export
handler_predict.accept_model <- function(vetiver_model, ...) {

  ptype <- vetiver_model$prototype

  function(req) {
    newdata <- req$body
    newdata <- vetiver_type_convert(newdata, ptype)
    newdata <- hardhat::scream(newdata, ptype)
    ret <- predict(vetiver_model$model, newdata = newdata, ...)
    list(.pred = ret)
  }

}

#' Metadata constructors for `vetiver_model()` object
#'
#' Useful for supporting accept_model class in vetiver
#' @param model a trained model object
#' @param metadata  list containing additional metadata to store with the pin
#' @return The vetiver_create_meta function returns a `vetiver_meta()` list.
#' @export
vetiver_create_meta.accept_model <- function(model, metadata) {
  vetiver_meta(metadata, required_pkgs = "accept")
}

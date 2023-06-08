#class(accept) <- c("accept_model", class(accept))

#' @export
vetiver_create_description <- function(model) {
  UseMethod("vetiver_create_description")
}

#' @export
vetiver_create_description.accept_model <- function(model) {
  "The Acute COPD Exacerbation Prediction Tool (ACCEPT)"
}

#' @export
predict<- function(object, newdata) {
  UseMethod("predict")
}

#' @export
predict.accept_model <- function(object, newdata) {
  accept(newdata)
}

#' @export
vetiver_ptype<- function(model,...) {
  UseMethod("vetiver_ptype")
}

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


#' @export
vetiver_create_meta<- function(model, metadata) {
  UseMethod("vetiver_create_meta")
}

vetiver_create_meta.accept_model <- function(model, metadata) {
  vetiver_meta(metadata, required_pkgs = "accept")
}


#' @export
handler_predict <- function(vetiver_model) {
  UseMethod("handler_predict")
}


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


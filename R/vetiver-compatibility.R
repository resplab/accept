#' @export
predict<- function(object, newdata) {
  UseMethod("predict")
}

#' @export
predict.accept_model <- function(object, newdata) {
  accept(newdata)
}

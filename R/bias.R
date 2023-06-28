#' @title Bias estimation function
#'
#' @description Function calculating bias
#'
#' @param actual vector of measured values
#' @param predicted vector of modeled values
#' @return return bias
#' @export

bias <- function(actual, predicted) {
  if (length(actual) != length(predicted)) {
    stop("vector with actual and prediction have different sizes")
  }

  actual <- as.numeric(actual)
  res <- sum(predicted - actual) / length(predicted)
  return(res)
}

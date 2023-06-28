#' @title Simulations function
#'
#' @description Function to run simulations with the calibration factors and test data
#'
#' @param params vector of calibration coeficients
#' @param test_data data frame containing the data to be run in test
#' @return return mse
#' @export

simulations <- function(params, test_data) {
  simulated_data <- rep(NA, nrow(test_data))
  names(simulated_data) <- test_data$Field_ID
  for (i in 1:nrow(test_data)) {
    simulated_data[i] <- sum(tail(
      AgreenaRothC2::ag_rothC(
        test_data[i, ],
        cf3 = params[1],
        cf4 = params[2],
        cf5 = params[3]
      ),
      1 # tail arg
    ))
  }
  return(simulated_data)
}

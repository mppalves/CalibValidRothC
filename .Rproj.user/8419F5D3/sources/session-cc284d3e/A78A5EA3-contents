#' @title Optimization function
#'
#' @description Function describing the opmization goal
#'
#' @param params vector of calibration coeficients
#' @param x_train input data.frame
#' @param y_train validation input
#' @return return mse
#' @export

optm_fun <- function(params, train_data) {
  x_train <- train_data[["x_train"]]
  y_train <- train_data[["y_train"]]

  simulated_data <- rep(NA, nrow(x_train))

  tryCatch(
    expr = {
      for (i in 1:nrow(x_train)) {
        simulated_data[i] <- sum(tail(ag_rothC(x_train[i, ]
                                               ,cf3 = params[1]
                                               ,cf4 = params[2]
                                               ,cf5 = params[3]
                                               ,write = FALSE), 1))
      }
    },
    error = function(e) {
      message(paste("Field_ID:", x_train[i, "Field_ID"]))
      print(e)
    }
  )
  res <- sum((simulated_data - y_train)^2) / length(simulated_data)
  return(res)
}

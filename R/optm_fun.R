#' @title Optimization function
#'
#' @description Function describing the opmization goal
#'
#' @param params vector of calibration coeficients
#' @param x_train input data.frame
#' @param y_train validation input
#' @return return mse
#' @importFrom AgreenaRothC2 ag_rothC
#' @export

optm_fun <- function(params, train_data) {
  x_train <- train_data[["x_train"]]
  y_train <- train_data[["y_train"]]
  simulated_data <- rep(NA, nrow(x_train))
  bad_field_ids <- list()
  #TODO: Make sure actual error msgs get pushed to warnings
  # put do not compromise perf.
  #err_msgs <- list()

  for (ii in seq_len(nrow(x_train))) {
    tryCatch(
      expr = {
        simulated_data[ii] <- sum(tail(
          AgreenaRothC2::ag_rothC(
            x_train[ii, ],
            cf3 = params[1],
            cf4 = params[2],
            cf5 = params[3],
            write = FALSE
          ),
          1 # tail
        ))
      },
      error = function(e) {
        bad_field_ids[[ii]] <<- as.data.frame(x_train)[ii, "Field_ID"]
        #err_msgs[[ii]] <- e
      }
    )
  }

  if (length(bad_field_ids) != 0) {
    out_warn_msg <- paste0(
      "Errors from rows with Field_ID: ",
      paste0(unlist(bad_field_ids), collapse = ", ")
    )

    warning(out_warn_msg)
  }

  not_na_indices <- !is.na(simulated_data)
  n_not_na <- sum(not_na_indices == TRUE)
  res <- sum((simulated_data[not_na_indices] - y_train[not_na_indices])^2) / n_not_na
  return(res)
}

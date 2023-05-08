#' @title Test function
#'
#' @description Function to test if all fields are working. It runs every field once to check if the pre processing is working
#'
#' @param x data source pro processed via \link{expand_data_set}
#' @return return bias
#' @export

test_fields <- function(x){
  test_input <- NULL
  tryCatch(
    expr = {
      for (i in 1:nrow(x)) {
        test_out[i] <- sum(tail(ag_rothC(x[i, ], write = FALSE), 1))
      }
    },
    error = function(e) {
      message(paste("Field ID:", x[i, "Field ID"]))
      print(e)
    }
  )
}

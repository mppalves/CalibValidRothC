#' @title Test function
#'
#' @description Function to test if all fields are working. It runs every field once to check if the pre processing is working
#'
#' @param x data source pro processed via \link{expand_data_set}
#' @return return bias and rmse
#' @export

bias_rmse <- function(x_test, y_test, y_pred) {
  x <- cbind(x_test, y_test, y_pred)
  metrics_per_study <- x %>%
    dplyr::group_by(Publication_ID, Practice_Category, CFGs) %>%
    dplyr::summarise(bias = bias(y_test, y_pred), rmse = Metrics::rmse(y_test, y_pred))

  return(metrics_per_study)
}

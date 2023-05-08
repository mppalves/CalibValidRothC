#' @title Test function
#'
#' @description Function to test if all fields are working. It runs every field once to check if the pre processing is working
#'
#' @param x data source pro processed via \link{expand_data_set}
#' @return return bias ans rmse
#' @export

bias_rmse <- function(x_test, y_test, y_pred){
  x <- cbind(x_test, y_test, y_pred)
  metrics_per_study <- x %>%
    group_by(Publication_ID, Practice_Category, CFGs) %>%
    summarise(bias = bias(y_test, y_pred), rmse = rmse(y_test, y_pred))
  # metrics_average <- metrics_per_study %>% select(bias, rmse) %>%
  #   summarise(bias = mean(bias), rmse = mean(rmse))
  return(metrics_per_study)
}

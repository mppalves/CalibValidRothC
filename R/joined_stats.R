#' Join Bias, RMSE, Confidence Interval, and PMU values
#'
#' This function performs left joins on several dataframes to calculate Bias, RMSE, Confidence Interval, and PMU values.
#'
#' @param bias_pooled_comb The dataframe containing bias values.
#' @param rmse_pooled_comb The dataframe containing RMSE values.
#' @param studycount The dataframe containing study counts.
#' @param Cis The dataframe containing confidence interval values.
#' @param PMU The dataframe containing PMU values.
#' @param join_cols Character vector of column names to join the dataframes on.
#'
#' @return A dataframe containing the merged data from all the left joins.
#'
#' @examples
#' bias_rmse_ci_pmu <- joined_stats(bias_pooled_comb, rmse_pooled_comb, studycount, Cis, PMU, c("Practice_Category", "Climate_zones_IPCC", "CFGs"))
#'@export
joined_stats <- function(bias_pooled_comb, rmse_pooled_comb, studycount, Cis, PMU, join_cols) {
  library(dplyr)
  
  result <- bias_pooled_comb %>%
    left_join(rmse_pooled_comb, by = join_cols) %>%
    left_join(studycount, by = join_cols) %>%
    left_join(Cis$Ci_percent, by = join_cols) %>%
    left_join(PMU, by = join_cols[!grepl("CFGs",join_cols)])
  
  return(result)
}
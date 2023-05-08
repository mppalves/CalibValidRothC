#' @title Test function
#'
#' @description Function to test if all fields are working. It runs every field once to check if the pre processing is working
#'
#' @param x data source pro processed via \link{expand_data_set}
#' @return return bias
#' @export

conf_int <- function(data_out){
  diffs <- data_out %>%
    mutate(observed_diff = SOC_End_Converted - SOC_Start_Converted,
           predicted_diff = soc_end_predicted - SOC_Start_Converted,
           model_error = predicted_diff - observed_diff)
  sds <- diffs %>%
    group_by(Practice_Category, CFGs, Climate_zones_IPCC) %>%
    summarise(sd = sd(model_error), n = n(),.groups ="drop")

  Ci <- left_join(diffs, sds, by = c("Practice_Category", "CFGs", "Climate_zones_IPCC")) %>%
  mutate(cilower = observed_diff - 1.64 * sd, ciupper = observed_diff + 1.64 * sd,
         fill = ifelse(predicted_diff < cilower | predicted_diff > ciupper,"Outside C.I.","Inside C.I."))

  Ci_percent <- Ci %>%
    group_by(Practice_Category, CFGs, Climate_zones_IPCC) %>%
    summarise(ci_percent = mean(fill == "Inside C.I.") * 100, .groups = "drop")

  return(list("Ci_percent" = Ci_percent, "Ci" = Ci))
}

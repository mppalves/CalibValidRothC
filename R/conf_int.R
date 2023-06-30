#' @title Test function
#'
#' @description Function to test if all fields are working. It runs every field once to check if the pre processing is working
#'
#' @param x data source pro processed via \link{expand_data_set}
#' @return return bias
#' @importFrom dplyr mutate group_by summarise left_join n
#' @export

conf_int <- function(data_out) {
  diffs <- data_out %>%
    dplyr::mutate(
      observed_diff = SOC_End_Converted - SOC_Start_Converted,
      predicted_diff = soc_end_predicted - SOC_Start_Converted,
      model_error = predicted_diff - observed_diff
    )

  sds <- diffs %>%
    dplyr::group_by(Practice_Category, CFGs, Climate_zones_IPCC) %>%
    dplyr::summarise(sd = sd(model_error), n = dplyr::n(), .groups = "drop")

  # 90% prediction interval, 5% either side, use t-dist as n is low and
  # sd is estimated
  Ci <- dplyr::left_join(diffs, sds, by = c("Practice_Category", "CFGs", "Climate_zones_IPCC")) %>%
    dplyr::mutate(
      cilower = observed_diff + qt(p = 0.05, df = n) * sd,
      ciupper = observed_diff + qt(p = 0.95, df = n) * sd,
      fill = ifelse(predicted_diff < cilower | predicted_diff > ciupper, "Outside C.I.", "Inside C.I.")
    )

  Ci_percent <- Ci %>%
    dplyr::group_by(Practice_Category, CFGs, Climate_zones_IPCC) %>%
    dplyr::summarise(
      ci_percent = mean(fill == "Inside C.I.") * 100,
      fields_inside = sum(fill == "Inside C.I."),
      total_fields = dplyr::n(),
      addtional_fields_to_90 = max(0, ceiling((0.9 * total_fields) - fields_inside)),
      .groups = "drop"
    )

  return(list("Ci_percent" = Ci_percent, "Ci" = Ci))
}

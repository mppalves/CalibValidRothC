#' Calculate Confidence Intervals
#'
#' This function calculates confidence intervals for a given dataset.
#'
#' @param data_out A data frame containing the necessary data.
#'   It should include the following columns:
#'   - SOC_End_Converted: Numeric vector representing the end converted SOC.
#'   - SOC_Start_Converted: Numeric vector representing the start converted SOC.
#'   - soc_end_predicted: Numeric vector representing the predicted SOC at the end.
#'   - Practice_Category: Categorical vector representing practice categories.
#'   - CFGs: Categorical vector representing CFGs.
#'   - Climate_zones_IPCC: Categorical vector representing climate zones (IPCC).
#'
#' @return A list containing two data frames: Ci_percent and Ci.
#'   - Ci_percent: A data frame summarizing the percentage of data points
#'     inside the confidence interval for each group.
#'   - Ci: A data frame containing the calculated confidence intervals
#'     and additional information for each data point.
#'
#' @details The function calculates the confidence intervals using the t-distribution
#' with 90% prediction interval (5% on either side) due to low sample size (n) and estimated standard deviation (sd).
#'
#' @import dplyr
#' @usage conf_int(data_out)
#' @examples
#' data_out <- data.frame(
#'   SOC_End_Converted = c(10, 12, 15, 8),
#'   SOC_Start_Converted = c(5, 9, 10, 7),
#'   soc_end_predicted = c(9, 11, 13, 6),
#'   Practice_Category = c("A", "B", "A", "B"),
#'   CFGs = c("CFG1", "CFG2", "CFG1", "CFG2"),
#'   Climate_zones_IPCC = c("Zone1", "Zone2", "Zone1", "Zone2")
#' )
#' conf_int(data_out)
#'
#' @seealso See \code{\link{qt}} for details on the t-distribution.
#' @importFrom stats qt
#'@export


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

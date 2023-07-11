#' Calculate PMU (Practice Mean Uncertainty) for a given dataset.
#'
#' This function calculates the Practice Mean Uncertainty (PMU) based on the input dataset.
#'
#' @param input_file Path to the CSV file containing the input data.
#' @return A numeric value representing the PMU.
#' @import tidyverse
#' @import readr
#' @import stringr
#'
#' @examples
#' pmu_result <- calculate_pmu(data_source)
#'
#' @export

calculate_pmu <- function(data_source) {

  # Select relevant columns
  studies_subset <- data_source %>%
    select(
      Publication_ID,
      Field_ID,
      Practice_Category,
      Climate_zones_IPCC,
      Measument_technique,
      CFGs,
      SOC_Start_Converted,
      SOC_End_Converted
    )
  
  # Calculate pmu_inputs
  pmu_inputs <- studies_subset %>%
    group_by(Practice_Category, Climate_zones_IPCC, CFGs) %>%
    summarise(
      practice_change_mean = mean(
        SOC_End_Converted - SOC_Start_Converted
      ),
      practice_change_variance = var(
        SOC_End_Converted - SOC_Start_Converted
      ),
      studies_per_practice_change = n()
    )
  
  # Calculate pmu
  pmu_result <- pmu_inputs %>%
    mutate(
      top = practice_change_variance * (studies_per_practice_change - 1),
      bot = (studies_per_practice_change - 1)
    ) %>%
    summarise(
      pmu = sqrt(sum(top, na.rm = TRUE) / sum(bot, na.rm = TRUE))
    )
  
  return(pmu_result)
}
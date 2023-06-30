#' Convert Values to tC/ha
#'
#' This function takes a unit measure and a corresponding value and converts
#' the value to tC/ha (metric tons of carbon per hectare) based on the provided
#' unit measure and optional bulk density.
#'
#' @param unit_measure The unit of measure for the value.
#' @param value The numeric value to be converted.
#' @param sampling_depth The depth in which the soil was sampled.
#' @param bulk_density Optional. The bulk density in kg/m^2. Required for certain unit measures.
#'
#' @return The converted value in tC/ha (metric tons of carbon per hectare).
#'
#' @examples
#' # Example usage
#' unit_measure <- "kg/ha"
#' value <- 5000
#' bulk_density <- 1500 # kg/m^2
#'
#' result <- convert_to_tc_per_ha(unit_measure, value, bulk_density)
#' print(result)
#'
#' @export
#'

convert_to_tc_per_ha <- function(unit_measure, value, sampling_depth, bulk_density = NULL) {
  # Define the lookup table
  conversion_table <- c(
    "%" = bulk_density * sampling_depth,
    "mgC/g" = bulk_density * sampling_depth / 10,
    "t C/ha" = 1,
    "g C/kg" = bulk_density * sampling_depth / 10,
    "g C/m^2" = 1 / 100,
    "kg/ha" = 1 / 1000,
    "kg/layer" = 10000 / 100
  )

  # Check if the unit measure exists in the conversion table
  if (!(unit_measure %in% names(conversion_table))) {
    stop("Invalid unit measure.")
  }

  # Perform the conversion using the lookup table
  tC_per_ha <- unlist(value) * conversion_table[[unit_measure]]

  return(tC_per_ha)
}

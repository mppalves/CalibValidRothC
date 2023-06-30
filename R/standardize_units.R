#' Convert Units to Standardized Units
#'
#' This function converts units to their standardized equivalents based on the unit_conversion dataset.
#'
#' @param unit A character string specifying the original unit.
#'
#' @return The conversion factor to convert the given unit to its standardized equivalent.
#'
#' @importFrom utils read.csv
#' @keywords conversion unit standardized
#' @export

standardize_units <- function(unit) {
  unit_conversion <- read.csv(file.path(getOption("calib_inputs"),"units.csv"), stringsAsFactors = FALSE)
  # Look up the conversion factor based on the unit
  conversion_factor <- unit_conversion$Standardized_Unit[unit_conversion$Original_Unit == unit]

  if (length(conversion_factor) == 0) {
    warning(paste0("No conversion factor found for unit '", unit, "'."))
  }
  return(conversion_factor)
}

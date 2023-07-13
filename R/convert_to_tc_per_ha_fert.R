#' Convert Original Values to tC/ha for Specific Fertilizer Types
#'
#' This function takes a dataframe \code{data} as input and converts the values
#' in the "values" column from the "original_unit" and "fert_id"
#' to tC/ha. It considers specific fertilizer types and their corresponding conversion
#' factors and carbon contents.
#'
#' @param data A dataframe containing the original values, original unit measures, and fertilizer types.
#'
#' @return A modified dataframe with the converted values in tC/ha.
#'
#' @examples
#' data <- data.frame(
#'   values = c("2240, 2358", "2240, 2358, 2035"),
#'   original_unit = c("kg ha-1 yr-1", "kg ha-1 yr-1"),
#'   fert_id = c("manure", "compost")
#' )
#' converted_data <- convert_to_tc_per_ha_fert(data)
#' print(converted_data)
#'
#' @export

convert_to_tc_per_ha_fert <- function(data) {
  names <- colnames(data)
  data <- fix_column_type(data)
  colnames(data) <- c("values", "original_unit", "fert_id")
  converted_data <- data
  converted_data$fert_c_content <- NA
  converted_data$conversion_factor <- NA
  for (i in seq_len(nrow(data))) {
    values <- strsplit(as.character(unlist(data[i, "values"])), ", ")[[1]]
    unit_measure <- as.character(unlist(data[i, "original_unit"]))
    fert_id <- as.character(unlist(data[i, "fert_id"]))
    conversion_factor <- get_conversion_factor(unit_measure)
    carbon_content <- get_carbon_content(fert_id)
    converted_values <- suppressWarnings(as.numeric(values) * conversion_factor * carbon_content)
    converted_data[i, "values"] <- toString(converted_values)
    converted_data[i, "fert_c_content"] <- carbon_content
    converted_data[i, "fert_conversion_factor"] <- conversion_factor
  }
  return(converted_data)
}

#' Get Conversion Factor for Unit Measure
#'
#' This function retrieves the conversion factor for a specific unit measure from the unit_conversion table.
#'
#' @param unit_measure The original unit measure.
#'
#' @return The conversion factor for the specified unit measure.
#'

get_conversion_factor <- function(unit_measure) {
  if (is.na(unit_measure) | unit_measure == "NA" | is.null(unit_measure) | unit_measure == "NULL") {
    return(0)
  }
  conversion_table <- read.csv(file.path(getOption("param_inputs"), "unit_conversion_fert.csv"), stringsAsFactors = FALSE)

  subset_table <- subset(conversion_table, original_unit == unit_measure)

  if (nrow(subset_table) == 0) {
    stop("Invalid unit_measure.")
  }

  return(as.numeric(subset_table$conversion_factor))
}

#' Get Carbon Content for Fertilizer Type
#'
#' This function retrieves the carbon content for a specific fertilizer type from the organic_fert_composition table.
#'
#' @param fert_id The type of organic fertilizer.
#'
#' @return The carbon content for the specified fertilizer type.
#'

get_carbon_content <- function(fert_id) {
  if (is.na(fert_id) | fert_id == "NA" | is.null(fert_id) | fert_id == "NULL") {
    return(0)
  }

  fert_table <- read.csv(file.path(getOption("param_inputs"), "organic_fert_composition.csv"), stringsAsFactors = FALSE)

  subset_table <- subset(fert_table, id == fert_id)

  if (nrow(subset_table) == 0) {
    stop("Invalid fert_id.")
  }

  return(as.numeric(subset_table$carbon_content) / 100) # values in %
}

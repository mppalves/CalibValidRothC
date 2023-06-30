#' Expand Data Set
#'
#' This function expands a data set by performing various transformations and calculations on the input data.
#'
#' @param x A data frame containing the input data.
#' @return A modified data frame with expanded and transformed data.
#'
#' @details
#' The `expand_data_set` function performs the following operations on the input data:
#' \itemize{
#'   \item Filters out rows with empty or null SOC End Original Value values.
#'   \item Splits cells in the "Crops Rotations" column by ",".
#'   \item Fills empty cells in "NCY converted value," "Manure C value," and "BI Converted Value" with character strings containing NAs.
#'   \item Performs calculations and assignments for fertilizer-related columns.
#'   \item Checks and compares lengths of certain columns row-wise, generating a warning if lengths are different.
#'   \item Prepares columns for processing by unlisting and converting null values to NA.
#'   \item Processes each row by splitting and assigning values to multiple rows based on certain conditions.
#'   \item Replaces NA values in the Bulk Density column with values based on Latitude, Longitude, and temp_soil_depth.
#'   \item Drops rows with NA values in specific columns.
#'   \item Applies the unit conversion function to each row of the data.
#' }
#'
#' @references
#' For more information on the functions and packages used in this implementation, please refer to the documentation of those functions and packages.
#'
#' @examples
#' # Create a sample data frame
#' df <- data.frame(
#'   `SOC Start Original Value` = c("1.5,2.0", "2.5", "1.0,1.5,2.0", NA),
#'   `SOC End Original Value` = c("3.5,4.0", "4.5", "3.0,3.5,4.0", NA),
#'   `Crops Rotations` = c("Crop A,Crop B", "Crop C", "Crop D,Crop E,Crop F", "Crop G"),
#'   `NCY converted value` = c("10.0", "", "20.0,25.0", NA),
#'   `Manure C value` = c("", "30.0", "40.0,45.0,50.0", NA),
#'   `BI Converted Value` = c("", "", "60.0,65.0,70.0", NA)
#' )
#'
#' # Expand the data set
#' expanded_df <- expand_data_set(df)
#'
#' @import tidyr dplyr
#' @export



expand_data_set <- function(x) {

  # Function to recursively unlist null values in a nested list
  unlist_null <- function(x) {
    if (is.null(x)) return(NA)
    if (is.list(x)) return(sapply(x, unlist_null))
    return(x)
  }

  # Filter out rows with empty or null SOC End Original Value values
  x <- x %>% dplyr::filter(!is.na(`SOC End Original Value`) & !is.null(`SOC End Original Value`) & `SOC End Original Value` != "" & `SOC End Original Value` != "NULL")

  # Split cells in "Crops Rotations" column by ","
  split_rotations <- strsplit(as.character(x$`Crops Rotations`), ",")

  # Check if "NCY converted value" and "Manure C value" are empty (NA)
  empty_ncy <- is.na(x$`NCY converted value`) | x$`NCY converted value` == "" | x$`NCY converted value` == "NULL" | is.null(x$`NCY converted value`)
  empty_manure_c <- is.na(x$`Manure C value`) | x$`Manure C value` == "" | x$`Manure C value` == "NULL" | is.null(x$`Manure C value`)
  empty_bi_c <- is.na(x$`BI Converted Value`) | x$`BI Converted Value` == "" | x$`BI Converted Value` == "NULL" | is.null(x$`BI Converted Value`)

  # Fill empty cells in "NCY converted value" and "Manure C value" with character strings containing NAs
  x$`NCY converted value`[empty_ncy] <- lapply(split_rotations[empty_ncy], function(split) paste(rep("NA", length(split)), collapse = ","))
  x$`Manure C value`[empty_manure_c] <- lapply(split_rotations[empty_manure_c], function(split) paste(rep("NA", length(split)), collapse = ","))
  x$`BI Converted Value`[empty_bi_c] <- lapply(split_rotations[empty_bi_c], function(split) paste(rep("NA", length(split)), collapse = ","))

  # Perform some calculations and assignments
  fert_conversion <- x[,c("Manure application (MA) original value", "MA original unit", "fert_id")]
  fert_converted <- convert_to_tc_per_ha_fert(fert_conversion)
  x$org_fert_carb <- fert_converted$values
  x$fert_c_content <- fert_converted$fert_c_content
  x$fert_conversion_factor <- fert_converted$fert_conversion_factor

  # Split cells and compare lengths row-wise
  lengths_check <- apply(x[, c("Crops Rotations", "NCY converted value", "Manure C value")], 1, function(row) {
    lengths <- sapply(strsplit(as.character(row), ","), length)
    !all(lengths == lengths[1])
  })

  # Get the Field ID where lengths_check is true
  field_ids <- x$`Field ID`[lengths_check]

  # Generate warning if lengths are different
  if (length(field_ids) > 0) {
    warning(paste("Columns 'Crops Rotations', 'NCY converted value', and 'Manure C value' have different lengths in Field ID(s):", paste(field_ids, collapse = ", ")))
  }

  new_rows <- data.frame()

  # Preparing columns for processing
  x$`SOC Start Original Value` <- x$`SOC Start Original Value` %>% unlist_null() %>% as.character()
  x$`SOC End Original Value` <- x$`SOC End Original Value` %>% unlist_null() %>% as.character()
  x$`Sampling depth increment` <- x$`Sampling depth increment` %>% unlist_null() %>% as.character()
  x$temp_soil_depth <- x$`Sampling depth increment` %>% unlist_null() %>% as.character()
  x$`Bulk Density`<- x$`Bulk Density` %>% unlist_null() %>% as.character()
  x$bd_back <- x$`Bulk Density` %>% unlist_null() %>% as.character()

  # Column indices
  col_depth <- which(colnames(x) == "Sampling depth increment")
  col_end <- which(colnames(x) == "SOC End Original Value")
  col_start <- which(colnames(x) == "SOC Start Original Value")
  col_bd <- which(colnames(x) == "Bulk Density")
  col_temp <- which(colnames(x) == "temp_soil_depth")

  toremove <- rep_len(0, length.out = nrow(x))

  # Loop through each row
  for (i in 1:nrow(x)) {
    split_start <- strsplit(as.character(x$`SOC Start Original Value`[i]), ",")[[1]]
    split_end <- strsplit(as.character(x$`SOC End Original Value`[i]), ",")[[1]]
    split_depth <- as.numeric(strsplit(as.character(x$`Sampling depth increment`[i]), ",")[[1]])
    split_bd <- as.numeric(strsplit(as.character(x$`Bulk Density`[i]), ",")[[1]])
    profile_depth <- split_depth

    # Checking if all splits have the same inputs
    if (!(length(split_start) == length(split_end) & length(split_end) == length(split_depth))) {
      # If not, stop execution and display an error message
      stop(paste("Sampling depth increment, SOC End Original Value, and SOC Start Original Value don't have the same length in F.id:", x$`Field ID`[i]))
    }

    y <- data.frame()
    if (length(split_start) > 1) {
      # Calculate the soil profile depth and not the size
      for (j in 2:length(split_depth)) {
        profile_depth[j] <- split_depth[j] - split_depth[j - 1]
      }

      # Create multiple rows for each split
      for (j in 1:length(split_start)) {
        y <- rbind(y, x[i, ])
      }

      # Assign values to each split row
      for (j in 1:length(split_start)) {
        y[j, col_depth] <- as.character(profile_depth[j])
        y[j, col_temp] <- as.character(split_depth[j])
        y[j, col_end] <- as.character(split_end[j])
        y[j, col_start] <- as.character(split_start[j])
        y[j, col_bd] <- as.character(split_bd[j])
      }

      new_rows <- rbind(new_rows, y)
      toremove[i] <- 1
    }
  }

  toremove <- as.logical(toremove)
  x <- rbind(x[!toremove,], new_rows)
  x <- x[order(x$`Field ID`), ]
  x[,col_depth] <- as.numeric(unlist(x[,col_depth]))
  x[,col_end] <- as.numeric(unlist(x[,col_end]))
  x[,col_start] <- as.numeric(unlist(x[,col_start]))
  x[,col_temp] <- as.numeric(unlist(x[,col_temp]))
  x[,col_bd] <- as.numeric(unlist(x[,col_bd]))

  # Replace NA values in Bulk Density column with values based on Latitude, Longitude, and temp_soil_depth
  bd <- apply(x, 1, function(x) ifelse(is.na(x$`Bulk Density`), environmental_variables(x$Latitude, x$Longitude, x$temp_soil_depth)[1,"BD"], x$`Bulk Density`))
  x[,col_bd] <- as.numeric(unlist(bd))

  # Drop rows with NA values in specific columns
  x <- x %>% tidyr::drop_na(`SOC Start Original Value`, `SOC End Original Value`, `Sampling depth increment`, CFGs)

  # Apply the unit conversion function to each row of the data
  x$`Standardized Unit` <- sapply(x$`SOC Original Unit`, standardize_units)

  # convert initial and final SOC from original units to tC/ha
  x$`SOC Start Converted` <-  apply(x, 1, function(x) {
    convert_to_tc_per_ha(value = x$`SOC Start Original Value`,
                         sampling_depth = x$temp_soil_depth,
                         bulk_density = x$`Bulk Density`,
                         unit_measure = x$`Standardized Unit`)
  })

  x$`SOC End Converted` <-  apply(x, 1, function(x) {
    convert_to_tc_per_ha(value = x$`SOC End Original Value`,
                         sampling_depth = x$temp_soil_depth,
                         bulk_density = x$`Bulk Density`,
                         unit_measure = x$`Standardized Unit`)
  })

  # Removing spaces and special characters from column names

  names(x) <- names(x) %>% gsub(" ", "_", .)
  names(x) <- names(x) %>% gsub("\\(", "", .)
  names(x) <- names(x) %>% gsub("\\)", "", .)
  names(x) <- names(x) %>% gsub("\\.", "", .)

  return(x)
}

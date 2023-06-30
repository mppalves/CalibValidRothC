#' Fix Column Type
#'
#' Converts columns of unknown type containing characters, NULL, NA, and list values
#' into columns of type character in a dataframe.
#'
#' @param data The input dataframe.
#' @return The dataframe with columns converted to type character.
#' @examples
#' # Example dataframe
#' data <- data.frame(
#'   id = 1:5,
#'   values = list(letters[1:3], NULL, NA, letters[4:5], character()),
#'   other_column = c("text", NULL, "NA", "more text", "")
#' )
#'
#' # Convert all columns in the dataframe
#' converted_data <- fix_column_type(data)
#'
#' # Output the converted dataframe
#' print(converted_data)
#'
#' @importFrom purrr map flatten
#' @export


fix_column_type <- function(data) {
  converted_data <- data

  # Iterate over all columns in the dataframe
  for (col_name in names(converted_data)) {
    column <- converted_data[[col_name]]

    # Convert NULL and NA values to character "NA"
    column <- purrr::map(column, function(x) {
      if (is.null(x) || is.na(x)) {
        "NA"
      } else if (is.list(x)) {
        paste(unlist(purrr::flatten(x)), collapse = ",")
      } else {
        x
      }
    })

    # Convert the column to character type
    column <- as.character(column)

    # Replace empty strings with "NA"
    column[column == ""] <- "NA"

    # Update the copied dataframe with the converted column
    converted_data[[col_name]] <- column
  }

  return(converted_data)
}

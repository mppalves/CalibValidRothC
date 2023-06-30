#' Test each row in data_source using ag_rothc()
#'
#' This function tests each row in data_source by feeding it into ag_rothc().
#' When an error occurs, the corresponding value from the "Field Id" column is stored in a vector and returned at the end.
#'
#' @param data_source A data frame containing the rows to be tested.
#'
#' @return A vector containing the "Field Id" values for rows that caused errors when passed to ag_rothc().
#'
#' @importFrom AgreenaRothC2 ag_rothC
#' @export
test_data_source <- function(data_source) {
  # names(data_source) <- names(data_source) %>% gsub(" ", "_", .)
  # names(data_source) <- names(data_source) %>% gsub("\\(", "", .)
  # names(data_source) <- names(data_source) %>% gsub("\\)", "", .)
  # names(data_source) <- names(data_source) %>% gsub("\\.", "", .)
  error_fields <- NULL # Vector to store "Field Id" values for rows that cause errors
  error_rows <- NULL
  out_rows <- NULL

  # for (i in 1:nrow(data_source)) {
  #   tryCatch({
  #     out_rows[i] <- sum(tail(ag_rothC(data_source[i, ]),1))  # Feed row i into ag_rothc()
  #   }, error = function(e) {
  #     error_rows <- c(error_rows, data_source[i, "Field_ID"])  # Store "Field Id" value
  #   })
  # }

  for (i in 1:nrow(data_source)) {
    result <- try(AgreenaRothC2::ag_rothC(data_source[i, ]), silent = TRUE)
    if (inherits(result, "try-error")) {
      error_fields <- c(error_fields, as.character(data_source[i, "Field_ID"])) # Store "Field Id" value
      error_rows <- c(error_rows, i) # Store "Field Id" value
    }
  }

  if (!is.null(error_rows)) {
    warning(paste(length(error_rows), "rows failed."))
  }

  return(list("row" = error_rows, "fields" = error_fields))
}

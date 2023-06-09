% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/expand_data_set.R
\name{expand_data_set}
\alias{expand_data_set}
\title{Expand Data Set}
\usage{
expand_data_set(x)
}
\arguments{
\item{x}{A data frame containing the input data.}
}
\value{
A modified data frame with expanded and transformed data.
}
\description{
This function expands a data set by performing various transformations and calculations on the input data.
}
\details{
The \code{expand_data_set} function performs the following operations on the input data:
\itemize{
\item Filters out rows with empty or null SOC End Original Value values.
\item Splits cells in the "Crops Rotations" column by ",".
\item Fills empty cells in "NCY converted value," "Manure C value," and "BI Converted Value" with character strings containing NAs.
\item Performs calculations and assignments for fertilizer-related columns.
\item Checks and compares lengths of certain columns row-wise, generating a warning if lengths are different.
\item Prepares columns for processing by unlisting and converting null values to NA.
\item Processes each row by splitting and assigning values to multiple rows based on certain conditions.
\item Replaces NA values in the Bulk Density column with values based on Latitude, Longitude, and temp_soil_depth.
\item Drops rows with NA values in specific columns.
\item Applies the unit conversion function to each row of the data.
}
}
\examples{
# Create a sample data frame
df <- data.frame(
  `SOC Start Original Value` = c("1.5,2.0", "2.5", "1.0,1.5,2.0", NA),
  `SOC End Original Value` = c("3.5,4.0", "4.5", "3.0,3.5,4.0", NA),
  `Crops Rotations` = c("Crop A,Crop B", "Crop C", "Crop D,Crop E,Crop F", "Crop G"),
  `NCY converted value` = c("10.0", "", "20.0,25.0", NA),
  `Manure C value` = c("", "30.0", "40.0,45.0,50.0", NA),
  `BI Converted Value` = c("", "", "60.0,65.0,70.0", NA)
)

# Expand the data set
expanded_df <- expand_data_set(df)

}
\references{
For more information on the functions and packages used in this implementation, please refer to the documentation of those functions and packages.
}

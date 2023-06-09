% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_pmu.R
\name{calculate_pmu}
\alias{calculate_pmu}
\title{Calculate PMU (Practice Mean Uncertainty) for a given dataset.}
\usage{
calculate_pmu(data_source)
}
\arguments{
\item{input_file}{Path to the CSV file containing the input data.}
}
\value{
A numeric value representing the PMU.
}
\description{
This function calculates the Practice Mean Uncertainty (PMU) based on the input dataset.
}
\examples{
pmu_result <- calculate_pmu(data_source)

}

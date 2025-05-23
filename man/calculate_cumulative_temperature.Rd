% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{calculate_cumulative_temperature}
\alias{calculate_cumulative_temperature}
\title{Calculate Cumulative Temperature}
\usage{
calculate_cumulative_temperature(T, Tbase)
}
\arguments{
\item{T}{A numeric value representing the daily mean temperature.}

\item{Tbase}{A numeric value representing the base temperature.}
}
\value{
A numeric value representing the cumulative temperature.
}
\description{
This function calculates the cumulative temperature for a given day.
If the daily mean temperature (T) is greater than the base temperature (Tbase), it returns the difference.
Otherwise, it returns 0.
}
\examples{
calculate_cumulative_temperature(25, 10)
}

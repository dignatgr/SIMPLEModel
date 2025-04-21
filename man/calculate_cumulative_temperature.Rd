
\name{calculate_cumulative_temperature}
\alias{calculate_cumulative_temperature}
\title{Calculate Cumulative Temperature}
\description{
  This function calculates the cumulative temperature for a given day.
  If the daily mean temperature (T) is greater than the base temperature (Tbase), it returns the difference.
  Otherwise, it returns 0.
}
\usage{
calculate_cumulative_temperature(T, Tbase)
}
\arguments{
  \item{T}{Daily mean temperature.}
  \item{Tbase}{Base temperature.}
}
\value{
  Cumulative temperature for the day.
}
\examples{
  calculate_cumulative_temperature(25, 10)
}

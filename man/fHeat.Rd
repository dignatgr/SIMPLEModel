
\name{fHeat}
\alias{fHeat}
\title{Calculate Impact of Heat Stress on Biomass Growth}
\description{
  This function calculates the impact of daily maximum temperature (Tmax) on biomass growth due to heat stress.
  If Tmax is less than or equal to Theat, it returns 1.
  If Tmax is between Theat and Textreme, it returns a value between 1 and 0 based on the ratio.
  If Tmax is greater than Textreme, it returns 0.
}
\usage{
fHeat(Tmax, Theat, Textreme)
}
\arguments{
  \item{Tmax}{Daily maximum temperature.}
  \item{Theat}{Heat stress threshold temperature.}
  \item{Textreme}{Extreme temperature threshold.}
}
\value{
  Impact of heat stress on biomass growth.
}
\examples{
  fHeat(35, 30, 40)
}

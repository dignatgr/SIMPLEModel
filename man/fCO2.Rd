
\name{fCO2}
\alias{fCO2}
\title{Calculate Impact of CO2 Concentration on Radiation Use Efficiency (RUE)}
\description{
  This function calculates the impact of atmospheric CO2 concentration on RUE.
  If CO2 concentration is less than 700 ppm, it returns a value based on the sensitivity (SCO2).
  If CO2 concentration is greater than or equal to 700 ppm, it returns a constant value based on SCO2.
}
\usage{
fCO2(CO2, SCO2)
}
\arguments{
  \item{CO2}{Atmospheric CO2 concentration in ppm.}
  \item{SCO2}{Sensitivity of RUE to CO2 concentration.}
}
\value{
  Impact of CO2 concentration on RUE.
}
\examples{
  fCO2(600, 0.001)
}

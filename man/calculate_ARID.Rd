
\name{calculate_ARID}
\alias{calculate_ARID}
\title{Calculate ARID Index for Drought Stress}
\description{
  This function calculates the ARID index based on reference evapotranspiration (ETo) and plant available water (PAW).
}
\usage{
calculate_ARID(ETo, PAW)
}
\arguments{
  \item{ETo}{Reference evapotranspiration.}
  \item{PAW}{Plant available water.}
}
\value{
  ARID index for drought stress.
}
\examples{
  calculate_ARID(5, 100)
}

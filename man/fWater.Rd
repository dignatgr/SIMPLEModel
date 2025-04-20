
\name{fWater}
\alias{fWater}
\title{Calculate Impact of Drought Stress on RUE}
\description{
  This function calculates the impact of drought stress on RUE based on the ARID index and sensitivity (Swater).
}
\usage{
fWater(ARID, Swater)
}
\arguments{
  \item{ARID}{ARID index for drought stress.}
  \item{Swater}{Sensitivity of RUE to drought stress.}
}
\value{
  Impact of drought stress on RUE.
}
\examples{
  fWater(0.5, 0.1)
}

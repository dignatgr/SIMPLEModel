
\name{fTemp}
\alias{fTemp}
\title{Calculate Impact of Temperature on Biomass Growth}
\description{
  This function calculates the impact of daily mean temperature (T) on biomass growth.
  If T is less than Tbase, it returns 0.
  If T is between Tbase and Topt, it returns a value between 0 and 1 based on the ratio.
  If T is greater than or equal to Topt, it returns 1.
}
\usage{
fTemp(T, Tbase, Topt)
}
\arguments{
  \item{T}{Daily mean temperature.}
  \item{Tbase}{Base temperature.}
  \item{Topt}{Optimal temperature.}
}
\value{
  Impact of temperature on biomass growth.
}
\examples{
  fTemp(25, 10, 30)
}

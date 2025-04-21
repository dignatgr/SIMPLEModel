
\name{run_simple_model}
\alias{run_simple_model}
\title{Run SIMPLE Model}
\description{
  This function runs the SIMPLE model to simulate crop growth, development, and yield.
  It takes various parameters related to temperature, radiation, CO2 concentration, and water availability.
}
\usage{
run_simple_model(days, daily_temperatures, daily_max_temperatures, daily_radiation, daily_CO2, daily_ETo, Tbase, Topt, Theat, Textreme, SCO2, PAW, Swater, RUE, I50A, I50B, Tsum, HI, Imax_water)
}
\arguments{
  \itemize{
    \item{days}{Number of days for the simulation.}
    \item{daily_temperatures}{A numeric vector of daily mean temperatures.}
    \item{daily_max_temperatures}{A numeric vector of daily maximum temperatures.}
    \item{daily_radiation}{A numeric vector of daily radiation values.}
    \item{daily_CO2}{A numeric vector of daily CO2 concentrations.}
    \item{daily_ETo}{A numeric vector of daily reference evapotranspiration values.}
    \item{Tbase}{Base temperature.}
    \item{Topt}{Optimal temperature.}
    \item{Theat}{Heat stress threshold temperature.}
    \item{Textreme}{Extreme temperature threshold.}
    \item{SCO2}{Sensitivity to CO2 concentration.}
    \item{PAW}{Plant available water.}
    \item{Swater}{Sensitivity to water stress.}
    \item{RUE}{Radiation use efficiency.}
    \item{I50A}{Thermal time at which 50% of maximum interception occurs during growth phase.}
    \item{I50B}{Thermal time at which 50% of maximum interception occurs during senescence phase.}
    \item{Tsum}{Total thermal time for crop growth cycle.}
    \item{HI}{Harvest index.}
    \item{Imax_water}{Maximum impact of water stress.}
  }
}
\value{
  A list containing cumulative biomass and yield.
}
\examples{
  run_simple_model(100, daily_temperatures, daily_max_temperatures, daily_radiation, daily_CO2, daily_ETo, 10, 30, 35, 40, 0.001, 100, 0.01, 1.5, 100, 200, 1000, 0.5, 0.1)
}

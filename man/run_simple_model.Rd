
\name{run_simple_model}
\alias{run_simple_model}
\title{Run the SIMPLE Model to Simulate Crop Growth, Development, and Yield}
\description{
  This function runs the SIMPLE model to simulate crop growth, development, and yield.
  It takes various parameters related to temperature, radiation, CO2 concentration, and water availability.
}
\usage{
run_simple_model(days, daily_temperatures, daily_max_temperatures, daily_radiation, daily_CO2, daily_ETo, Tbase, Topt, Theat, Textreme, SCO2, PAW, Swater, RUE, I50A, I50B, Tsum, HI, Imax_water)
}
\arguments{
  \item{days}{Number of days for the simulation.}
  \item{daily_temperatures}{Vector of daily mean temperatures.}
  \item{daily_max_temperatures}{Vector of daily maximum temperatures.}
  \item{daily_radiation}{Vector of daily solar radiation values.}
  \item{daily_CO2}{Vector of daily atmospheric CO2 concentrations.}
  \item{daily_ETo}{Vector of daily reference evapotranspiration values.}
  \item{Tbase}{Base temperature for cumulative temperature calculation.}
  \item{Topt}{Optimal temperature for biomass growth.}
  \item{Theat}{Heat stress threshold temperature.}
  \item{Textreme}{Extreme temperature threshold.}
  \item{SCO2}{Sensitivity of RUE to CO2 concentration.}
  \item{PAW}{Plant available water.}
  \item{Swater}{Sensitivity of RUE to drought stress (ARID index).}
  \item{RUE}{Radiation use efficiency.}
  \item{I50A}{Parameter for solar radiation interception during growth phase.}
  \item{I50B}{Parameter for solar radiation interception during senescence phase.}
  \item{Tsum}{Cumulative temperature sum for growth phase.}
  \item{HI}{Harvest index.}
  \item{Imax_water}{Maximum impact of drought stress on solar radiation interception.}
}
\value{
  A list containing cumulative biomass (Biomass_cumi) and final yield (Yield).
}
\examples{
  run_simple_model(100, rep(25, 100), rep(30, 100), rep(20, 100), rep(400, 100), rep(5, 100), 10, 30, 35, 40, 0.001, 100, 0.5, 1.5, 50, 50, 1000, 0.5, 0.1)
}

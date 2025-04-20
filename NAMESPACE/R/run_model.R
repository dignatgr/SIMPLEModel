# Load the necessary functions
source('functions.R')

# Example usage of the run_simple_model function
# crop management parameter
fSolar_max = 0.95 # for high density crop

# Define input parameters for the model
days <- 100  # Number of days for the simulation
daily_temperatures <- runif(days, min = 5, max = 30)  # Simulated daily mean temperatures
daily_max_temperatures <- runif(days, min = 10, max = 45)  # Simulated daily maximum temperatures
daily_radiation <- runif(days, min = 10, max = 20)  # Simulated daily solar radiation
daily_CO2 <- rep(400, days)  # Simulated daily CO2 concentration (constant)
daily_ETo <- runif(days, min = 5, max = 15)  # Simulated daily reference evapotranspiration

# Define model parameters
Tbase <- 10  # Base temperature for biomass growth
Topt <- 25  # Optimal temperature for biomass growth
Theat <- 30  # Threshold temperature for heat stress
Textreme <- 40  # Extreme temperature where biomass growth is zero
SCO2 <- 0.01  # Sensitivity of RUE to CO2 concentration
PAW <- 100  # Plant available water
Swater <- 0.5  # Sensitivity of RUE to ARID index
RUE <- 1.5  # Radiation use efficiency
I50A <- 500  # Cumulative temperature for leaf area development
I50B <- 1000  # Cumulative temperature for leaf senescence
Tsum <- 1500  # Cumulative temperature required for maturity
HI <- 0.5  # Harvest index
Imax_water <- 5  # Maximum daily increase in I50B due to drought stress

# Run the model and get the results
results <- run_simple_model(days, daily_temperatures, daily_max_temperatures, daily_radiation, daily_CO2, daily_ETo,
                            Tbase, Topt, Theat, Textreme, SCO2, PAW, Swater,
                            RUE, I50A, I50B, Tsum, HI, Imax_water)

# Print the results
cat("Final cumulative biomass:", results$Biomass_cumi, "\n")
cat("Final yield:", results$Yield, "\n")

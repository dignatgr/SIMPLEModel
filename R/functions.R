# Define the necessary functions for the SIMPLE model

# Function to calculate cumulative temperature
# This function calculates the cumulative temperature for a given day.
# If the daily mean temperature (T) is greater than the base temperature (Tbase), it returns the difference.
# Otherwise, it returns 0.
calculate_cumulative_temperature <- function(T, Tbase) {
  if (T > Tbase) {
    return(T - Tbase)
  } else {
    return(0)
  }
}

# Function to calculate the impact of temperature on biomass growth
# This function calculates the impact of daily mean temperature (T) on biomass growth.
# If T is less than Tbase, it returns 0.
# If T is between Tbase and Topt, it returns a value between 0 and 1 based on the ratio.
# If T is greater than or equal to Topt, it returns 1.
fTemp <- function(T, Tbase, Topt) {
  if (T < Tbase) {
    return(0)
  } else if (Tbase <= T && T < Topt) {
    return((T - Tbase) / (Topt - Tbase))
  } else {
    return(1)
  }
}

# Function to calculate the impact of heat stress on biomass growth
# This function calculates the impact of daily maximum temperature (Tmax) on biomass growth due to heat stress.
# If Tmax is less than or equal to Theat, it returns 1.
# If Tmax is between Theat and Textreme, it returns a value between 1 and 0 based on the ratio.
# If Tmax is greater than Textreme, it returns 0.
fHeat <- function(Tmax, Theat, Textreme) {
  if (Tmax <= Theat) {
    return(1)
  } else if (Theat < Tmax && Tmax <= Textreme) {
    return(1 - (Tmax - Theat) / (Textreme - Theat))
  } else {
    return(0)
  }
}

# Function to calculate the impact of CO2 concentration on radiation use efficiency (RUE)
# This function calculates the impact of atmospheric CO2 concentration on RUE.
# If CO2 concentration is less than 700 ppm, it returns a value based on the sensitivity (SCO2).
# If CO2 concentration is greater than or equal to 700 ppm, it returns a constant value based on SCO2.
fCO2 <- function(CO2, SCO2) {
  if (CO2 < 700) {
    return(1 + SCO2 * (CO2 - 350))
  } else {
    return(1 + SCO2 * 350)
  }
}

# Function to calculate the ARID index for drought stress
# This function calculates the ARID index based on reference evapotranspiration (ETo) and plant available water (PAW).
calculate_ARID <- function(ETo, PAW) {
  return(1 - min(ETo, 0.096 * PAW) / ETo)
}

# Function to calculate the impact of drought stress on RUE
# This function calculates the impact of drought stress on RUE based on the ARID index and sensitivity (Swater).
fWater <- function(ARID, Swater) {
  return(1 - Swater * ARID)
}

# Function to calculate the fraction of solar radiation intercepted with drought stress
# This function calculates the fraction of solar radiation intercepted based on drought stress.
fSolar_water <- function(fWater_value) {
  if (fWater_value < 0.1) {
    return(0.9 * fWater_value)
  } else {
    return(1 - fWater_value)
  }
}

# Function to calculate the fraction of solar radiation intercepted
# This function calculates the fraction of solar radiation intercepted by a crop canopy during growth and senescence phases.
fSolar <- function(TT, I50A, I50B, Tsum, phase) {
  if (phase == "growth") {
    return(fSolar_max * (1 - exp(-0.01 * (TT - I50A))))
  } else if (phase == "senescence") {
    return(fSolar_max * (1 - exp(-0.01 * (TT - (Tsum - I50B)))))
  } else {
    return(0)
  }
}

# Define the main function for the SIMPLE model
# This function runs the SIMPLE model to simulate crop growth, development, and yield.
# It takes various parameters related to temperature, radiation, CO2 concentration, and water availability.
run_simple_model <- function(days, daily_temperatures, daily_max_temperatures, daily_radiation, daily_CO2, daily_ETo, Tbase, Topt, Theat, Textreme, SCO2, PAW, Swater, RUE, I50A, I50B, Tsum, HI, Imax_water) {
  TTi <- 0  # Initialize cumulative temperature
  Biomass_cumi <- 0  # Initialize cumulative biomass
  
  # Loop through each day to calculate daily and cumulative biomass
  for (i in 1:days) {
    # Calculate the cumulative temperature for the day
    ΔTT <- calculate_cumulative_temperature(daily_temperatures[i], Tbase)
    TTi <- TTi + ΔTT
    
    # Determine the growth phase (growth or senescence)
    phase <- ifelse(TTi < Tsum, "growth", "senescence")
    
    # Calculate the fraction of solar radiation intercepted
    fSolar_value <- fSolar(TTi, I50A, I50B, Tsum, phase)
    
    # Calculate the ARID index for drought stress
    ARID <- calculate_ARID(daily_ETo[i], PAW)
    
    # Calculate the impact of drought stress on solar radiation interception
    fSolar_value <- fSolar_water(fWater(ARID, Swater))
    
    # Calculate the daily biomass growth rate
    Biomass_rate <- daily_radiation[i] * fSolar_value * RUE * fCO2(daily_CO2[i], SCO2) * fTemp(daily_temperatures[i], Tbase, Topt) * min(fHeat(daily_max_temperatures[i], Theat, Textreme), fWater(ARID, Swater))
    
    # Update cumulative biomass
    Biomass_cumi <- Biomass_cumi + Biomass_rate
    
    # Update I50B with drought stress impact
    I50B <- I50B + Imax_water * (1 - fWater(ARID, Swater))
  }
  
  # Calculate final yield based on cumulative biomass and harvest index
  Yield <- Biomass_cumi * HI
  
  return(list(Biomass_cumi = Biomass_cumi, Yield = Yield))
}

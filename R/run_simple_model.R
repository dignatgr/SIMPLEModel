
run_simple_model <- function(days, daily_temperatures, daily_max_temperatures, daily_radiation, daily_CO2, daily_ETo, Tbase, Topt, Theat, Textreme, SCO2, PAW, Swater, RUE, I50A, I50B, Tsum, HI, Imax_water) {
  TTi <- 0
  Biomass_cumi <- 0
  
  for (i in 1:days) {
    ΔTT <- calculate_cumulative_temperature(daily_temperatures[i], Tbase)
    TTi <- TTi + ΔTT
    
    phase <- ifelse(TTi < Tsum, "growth", "senescence")
    
    fSolar_value <- fSolar(TTi, I50A, I50B, Tsum, phase)
    
    ARID <- calculate_ARID(daily_ETo[i], PAW)
    
    fSolar_value <- fSolar_water(fWater(ARID, Swater))
    
    Biomass_rate <- daily_radiation[i] * fSolar_value * RUE * fCO2(daily_CO2[i], SCO2) * fTemp(daily_temperatures[i], Tbase, Topt) * min(fHeat(daily_max_temperatures[i], Theat, Textreme), fWater(ARID, Swater))
    
    Biomass_cumi <- Biomass_cumi + Biomass_rate
    
    I50B <- I50B + Imax_water * (1 - fWater(ARID, Swater))
  }
  
  Yield <- Biomass_cumi * HI
  
  return(list(Biomass_cumi = Biomass_cumi, Yield = Yield))
}

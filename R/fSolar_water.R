
fSolar_water <- function(fWater_value) {
  if (fWater_value < 0.1) {
    return(0.9 * fWater_value)
  } else {
    return(1 - fWater_value)
  }
}

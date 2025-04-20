
fCO2 <- function(CO2, SCO2) {
  if (CO2 < 700) {
    return(1 + SCO2 * (CO2 - 350))
  } else {
    return(1 + SCO2 * 350)
  }
}


calculate_ARID <- function(ETo, PAW) {
  return(1 - min(ETo, 0.096 * PAW) / ETo)
}


calculate_cumulative_temperature <- function(T, Tbase) {
  if (T > Tbase) {
    return(T - Tbase)
  } else {
    return(0)
  }
}

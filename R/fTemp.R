
fTemp <- function(T, Tbase, Topt) {
  if (T < Tbase) {
    return(0)
  } else if (Tbase <= T && T < Topt) {
    return((T - Tbase) / (Topt - Tbase))
  } else {
    return(1)
  }
}

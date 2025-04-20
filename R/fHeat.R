
fHeat <- function(Tmax, Theat, Textreme) {
  if (Tmax <= Theat) {
    return(1)
  } else if (Theat < Tmax && Tmax <= Textreme) {
    return(1 - (Tmax - Theat) / (Textreme - Theat))
  } else {
    return(0)
  }
}

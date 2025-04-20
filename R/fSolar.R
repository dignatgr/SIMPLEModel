
fSolar <- function(TT, I50A, I50B, Tsum, phase) {
  if (phase == "growth") {
    return(fSolar_max * (1 - exp(-0.01 * (TT - I50A))))
  } else if (phase == "senescence") {
    return(fSolar_max * (1 - exp(-0.01 * (TT - (Tsum - I50B)))))
  } else {
    return(0)
  }
}


\\name{fSolar}
\\alias{fSolar}
\\title{Calculate Fraction of Solar Radiation Intercepted}
\\description{
  This function calculates the fraction of solar radiation intercepted by a crop canopy during growth and senescence phases.
}
\\usage{
fSolar(TT, I50A, I50B, Tsum, phase)
}
\\arguments{
  \\item{TT}{Thermal time.}
  \\item{I50A}{Thermal time at which 50\% of maximum interception occurs during growth phase.}
  \\item{I50B}{Thermal time at which 50\% of maximum interception occurs during senescence phase.}
  \\item{Tsum}{Total thermal time for the crop.}
  \\item{phase}{Growth phase ("growth" or "senescence").}
}
\\value{
  Fraction of solar radiation intercepted.
}
\\examples{
  fSolar(500, 300, 700, 1000, "growth")
}


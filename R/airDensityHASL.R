#' Air density in [g cm^-3] by using height above sea level information
#'
#' Calculates the approximated density of the air in the laboratory according to its height above sea level
#'
#'
#' @param HASL height altitude above sea level in meters
#'
#' @return air density value
#'
#' @examples
#' airDensity(Temp = 22.3, p = 748.1, h = 37, units = c('deg.C', 'mmHg', '%')) # [g/cm^3]
#' airDensity(Temp = 23.4, p = 612.3, h = 23, units = c('deg.C', 'mmHg', '%')) # [g/cm^3]
#' airDensity(Temp = 23.4, p = 612.3, h = 23, units = c('deg.C', 'mmHg', '%'), opt = 'A') # [g/cm^3]
#'
#' @export

# https://www.nist.gov/system/files/documents/2019/05/13/sop-2-applying-air-buoyancy-20190506.pdf
airDensityHASL <- function(HASL) { # [g/cm^3]
  rho_airHASL <- 1.2 * exp(-(1.2 / 101325) * 9.8 * HASL) / 1000
  return(rho_airHASL)
}

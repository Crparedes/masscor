#' Magnitude of the Air Buoyancy Correction
#'
#' Calculates the magnitude
#'
#' @param rho_s asragsd
#' @export

MABC <- function(rho_s = 0.998, rho_w = 8, rho_air = airDensity()) {
  MABC <- (1 - rho_air/rho_w) / (1 - rho_air/rho_s)
  return(MABC)
}

# MABC() # Agua MichalMariassy: 1.001025
# MABC(rho_s = 7.133) # Zinc MichalMariassy: 1.000017
# MABC(rho_s = 8.96) # Cobre SergioRezzonico: 0.9999856
# MABC(rho_s = 4.53) # nitrato de plomo

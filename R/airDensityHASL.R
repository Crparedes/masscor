#' Air density estimation using height above sea level information
#'
#' Calculates the approximated density of local air using a model that relies
#' on height above sea level (HASL) information. Better alternatives are found
#' in [airDensity()].
#'
#' @param HASL height altitude above sea level in meters.
#' @seealso [airDensity()] for better models to predict air density.
#' @return Approximated air density value in \eqn{g~cm^{-3}}
#' @references Preguntar a Andres
#' @examples
#' airDensityHASL(HASL = 0) # [g/cm^3]
#' airDensityHASL(HASL = 1600) # [g/cm^3]
#' @export

airDensityHASL <- function(HASL) { # [g/cm^3]
  rho_airHASL <- 1.2 * exp(-(1.2 / 101325) * 9.8 * HASL) / 1000
  return(rho_airHASL)
}

#' Converts balance reading to conventional mass using balance calibration information
#'
#' Given a balance reading indication and the calibration information of the balance, the function
#' interpolates error correction for the reading using the errors of the two closest calibration
#' points with mass standards. Interpolated error is later used to produce the conventional mass
#' of the object.
#'
#' The conventional mass value of a body is equal to the mass \eqn{m_c} of a mass standard
#' that balances this body under conventionally chosen conditions: at a temperature \eqn{t_{ref} = 20^o}C,
#' with mass standards of density \eqn{\rho_c=8000} kg m\eqn{^{-3}}, in normal air of density
#' \eqn{\rho_0=1.2} kg m\eqn{^{-3}} (OIML, 2004).
#'
#' @references
#' OIML, (2004). ORGANISATION INTERNATIONALE DE MÉTROLOGIE LÉGALE. Conventional value of the result of weighing in air.
#'
#' Picard, A; Davis, R S; Gläser, M; Fujii, K  (2008).  Revised formula for the density of moist air (CIPM-2007).
#' Metrologia, 45(2), 149–155. doi:10.1088/0026-1394/45/2/004
#'
#' Harris, G. (2019). Selected Laboratory and Measurement Practices and Procedures to Support Basic
#' Mass Calibrations. SOP 2 - Recommended Standard Operating Procedure for Applying Air Buoyancy
#' Corrections. National Institute of Standards and Technology (NIST). doi:10.6028/NIST.IR.6969-2019
#'
#' @param reading Numeric with balance reading for the mass of the object
#' @param units Character with the units of \code{reading}. Must be a SI unit. If not provided, the
#'   balance standard units are assumed. See [calibCert()] for details.
#' @param calibCert Object of class \code{"calibCert"} with the calibration data of the balance.
#'    See [calibCert()] for details.
#' @return Conventional mass value as corrected by using balance calibration data.
#'
#' @examples
#' data(minimalCert)
#' convMass(reading = 12.4835, calibCert = minimalCert)
#' @export
#' @importFrom stats predict lm

convMass <- function(calibCert, reading, units = NULL,
                     rho = NULL, rho_air = NULL) {
  if(missing(units)) {
    fc <- 1
    units <- calibCert$standardUnits
  } else {
    fc <- convertMassUnitsSI(from = units, to = calibCert$standardUnits, value = 1)
  }

  reading <- reading * fc

  if (reading > max(calibCert$indError[, 1]) || reading < min(calibCert$indError[, 1])) {
    warning('Reading is outside calibration interval: ', min(calibCert$indError[, 1]),
            ' - ', max(calibCert$indError[, 1]), ' [', calibCert$standardUnits, ']')
  }

  p1 <- which.min(abs(calibCert$indError[, 1] - reading))
  p2prim <- min(abs(calibCert$indError[, 1][-p1] - reading))
  p2 <- which.min(abs(calibCert$indError[, 1] - p2prim))

  Cp_i <- - calibCert$indError[, 2][c(p1, p2)]
  mp_i <- calibCert$indError[, 1][c(p1, p2)]
  correction <- suppressWarnings(
    predict(lm(Cp_i ~ mp_i), newdata = data.frame(mp_i = reading)))
  corrected <- reading + correction
  corrected <- as.numeric(corrected) / fc
  #if (print) cat(paste0('Masa corregida: ', corrected, ' [', units, ']\n'))

  if (missing(rho) || missing(rho_air)) {
    message('The result corresponds to the mass measurement result at the conditions of the calibration.')
    return(corrected)
  } else {
    corrected <- corrected * (1 + ((rho_air - 0.0012)*(1/rho - 1/8)))
    return(corrected)
  }


}

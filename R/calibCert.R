#' Information of balance calibration certificate
#'
#' Creates an object of class \code{calibCert} containing the information provided in a calibration certificate.
#' The object can later be used to correct mass readings and calculate mass uncertainties.
#' Minimal information must include measurement error with uncertainty for several mass standards and the balance division scale.
#'
#' The units of \code{massSTD}, \code{indError}, \code{uncert} and \code{d} that are provided to the parameter \code{units} can be
#' any multiple or subdivision of the SI unit for mass the kilogram. The greek letter $\mu$ is replaced by the vocal \code{u}.
#' The SI prefixes are case sensitive.
#'
#' Pressure units (\code{p}) can be any of \code{'mmHg'}, \code{'Pa'}, \code{'hPa'} or \code{'kPa'}.
#' Temperature units (\code{Temp}) can be either \code{'deg.C'} (for Celsius degrees) or \code{'K'}.
#' Relative humidity (\code{h}) can be expressed as fraction (\code{'frac'}) or as percentage (\code{%}).
#' A typical arrangement for the parameter \code{unitsENV} could be \code{c('hPa', 'deg.C', '%')}.
#'
#' @param balanceID Character vector with balance identification.
#' @param massSTD Numeric vector with the masses of the mass standards used for calibration
#' @param indError Numeric vector of length \code{length(massSTD)} with the indication error for each mass standard
#' @param uncert Numeric vector of length \code{length(massSTD)} with the uncertainty of the indication error for each mass standard
#' @param expanded If \code{TRUE} (the default), \code{uncert} is assumed to be expanded uncertainties instead of standard uncertainties
#' @param k Coverage factor for \code{uncert} when \code{expanded = TRUE}.
#' @param d Division scale of the balance
#' @param units Character vector of length 4 with the units of \code{massSTD}, \code{indError}, \code{uncert} and \code{d}.
#'   Default is \code{ c('g', 'mg', 'mg', 'mg')} which is typical for an analytic balance. See Details for more options.
#' @param classSTD Character vector with the class of the mass standards used.
#' @param certSTD Character vector with the certificate identification of the mass standards used.
#' @param p Barometric pressure at the moment of the calibration.
#' @param Temp Ambient temperature at the moment of the calibration.
#' @param h Relative humidity at the moment of the calibration.
#' @param unitsENV Character vector of length 3 with the units of \code{p}, \code{Temp} and \code{h}.
#' @param institution Character vector with the institution that performed the calibration.
#' @param date Character vector with the date of the calibration.
#' @param additionalDetails Character vectors with any additional details included in the certificate.
#'
#' @return Object of class \code{calibCert} with information of a calibration certificate for a balance.
#'
#' @seealso [convertMassUnitsSI()]
#'
#' @examples
#' massSTD <- c(0.01, 0.5, 1, 10, 20, 50, 100, 120, 150, 200, 220)  ## [g]
#' indError <- c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -0.2, -0.2) ## [mg]
#' uncert <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.2, 0.2, 0.3, 0.3, 0.4, 0.5) / 2 ## [mg]
#' d <- 0.1 ## [mg]
#'
#' Balance.D1 <- calibCert(balanceID = 'Balance.D1', massSTD = massSTD,
#'                         indError = indError, uncert = uncert, d = d,
#'                         units = c('g', 'mg', 'mg', 'mg'))
#' print(Balance.D1)
#' @export
#'

calibCert <- function(balanceID = 'BalanceID',
                      massSTD,
                      indError,
                      uncert,
                      expanded = TRUE,
                      k = 2,
                      d,
                      units = c('g', 'mg', 'mg', 'mg'),
                      classSTD = NULL,
                      certSTD = NULL,
                      p = NULL,
                      Temp = NULL,
                      h = NULL,
                      unitsENV = NULL,
                      institution = NULL,
                      date = NULL,
                      additionalDetails = NULL
                      ) {
  if (length(massSTD) != length(indError) || length(massSTD) != length(uncert)) {
    stop('Vectors in arguments "massSTD", "indError" and "uncert" must be all numeric of same size.')
  }
  calibCert <- list(balanceID = balanceID,
                    standardUnits = units[1],
                    massSTD = massSTD,
                    indError = convertMassUnitsSI(from = units[2], to = units[1], value = indError),
                    uncert = convertMassUnitsSI(from = units[3], to = units[1], value = uncert),
                    expandUncert = convertMassUnitsSI(from = units[3], to = units[1], value = uncert),
                    d = convertMassUnitsSI(from = units[4], to = units[1], value = d))

  if (expanded) {
    calibCert$uncert <- calibCert$uncert / k
  } else {
    calibCert$expandUncert <- calibCert$expandUncert * k
  }

  if (!missing(classSTD)) calibCert$classSTD <- classSTD
  if (!missing(certSTD)) calibCert$certSTD <- certSTD
  if (!missing(barPress)) calibCert$barPress <- barPress
  if (!missing(ambTempe)) calibCert$ambTempe <- ambTempe
  if (!missing(relHumid)) calibCert$relHumid <- relHumid
  if (!missing(unitsENV)) calibCert$unitsENV <- unitsENV
  if (!missing(institution)) calibCert$institution <- institution
  if (!missing(date)) calibCert$date <- date
  if (!missing(details)) calibCert$details <- details
  #if (!missing()) calibCert$ <-

  class(calibCert) <- 'calibCert'
  return(calibCert)
}

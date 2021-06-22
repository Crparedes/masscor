#' Information of balance calibration certificate
#'
#' Creates an object of class \code{calibCert} containing the information provided in a calibration certificate.
#' The object can later be used to correct mass readings and calculate mass uncertainties.
#' Minimal information must include measurement error with uncertainty for several mass standards and the balance division scale.
#'
#' The units of \code{massSTD}, \code{indError}, \code{uncert} and \code{d} that are provided to the parameter \code{units} can be
#' any multiple or subdivision of the SI unit for mass the kilogram. The greek letter \eqn{\mu} is replaced by the vocal \code{u}.
#' Remember that both R and the SI prefixes are case sensitive.
#'
#' @section unitsENV:
#' Pressure units (\code{p}) can be any of \code{'mmHg'}, \code{'Pa'}, \code{'hPa'} or \code{'kPa'}.
#' Temperature units (\code{Temp}) can be either \code{'deg.C'} (for Celsius degrees) or \code{'K'}.
#' Relative humidity (\code{h}) can be expressed as fraction (\code{'frac'}) or as percentage (\code{'\%'}).
#' A typical arrangement for the parameter \code{unitsENV} could be \code{c('hPa', 'deg.C', '\%')}.
#'
#'
#' @param balanceID Character with balance identification. May be the balance model, brand or internal location.
#' @param serial Serial number of the balance.
#' @param certificate Character with the calibration certificate number and date of issue.
#' @param massSTD Numeric vector with the masses of the mass standards used for calibration.
#' @param indError Numeric vector of length \code{length(massSTD)} with the indication error for each mass standard.
#' @param uncert Numeric vector of length \code{length(massSTD)} with the uncertainty of the indication error for each mass standard.
#' @param expanded If \code{TRUE} (the default), \code{uncert} is assumed to be expanded uncertainties instead of standard uncertainties.
#' @param k Coverage factor for \code{uncert} when \code{expanded = TRUE}.
#' @param d Division scale of the balance
#' @param units Character vector of length 4 with the units of \code{massSTD}, \code{indError} (or \code{CMcorr}), \code{uncert} and \code{d}.
#'   Default is \code{c('g', 'mg', 'mg', 'mg')} which is typical for an analytic balance. See Details for more options.
#'   The units of \code{massSTD} are defined as the balance standard units.
#' @param repet Numeric vector with balance readings for the same object under repeatability conditions.
#'   Can also be a list of numeric vectors
#' @param eccen
#' @param classSTD Character with the class of the mass standards used.
#' @param traceability Character with information regarding the traceability of the calibration.
#' @param p Barometric pressure at the moment of the calibration.
#' @param Temp Ambient temperature at the moment of the calibration.
#' @param h Relative humidity at the moment of the calibration.
#' @param unitsENV Character vector of length 3 with the units of \code{p}, \code{Temp} and \code{h}.
#'   Default is \code{c('deg.C', 'hPa', '\%')}. See **unitsENV** below for more options.
#' @param institution Character with the identification of the calibration laboratory.
#' @param accreditation Character with the accreditation information of the calibration laboratory.
#' @param date Character with the date of the measurements.
#' @param add.info Named list or vector with any additional details included in calibration certificate.
#'
#' @return Object of class \code{calibCert} with information of a calibration certificate for a balance.
#'
#' @seealso S3 methods [print.calibCert()] and [plot.calibCert()] are available. See [convertMassUnitsSI()] for
#'   information about mass units
#'
#' @examples
#' massSTD <- c(0.01, 0.5, 1, 10, 20, 50, 100, 120, 150, 200, 220)        ## [g]
#' indError <- c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -0.2, -0.2) ## [mg]
#' uncert <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.2, 0.2, 0.3, 0.3, 0.4, 0.5) / 2 ## [mg]
#' d <- 0.1                                                               ## [mg]
#'
#' Balance.D1 <- calibCert(balanceID = 'Balance.D1', massSTD = massSTD,
#'                         indError = indError, uncert = uncert, d = d,
#'                         units = c('g', 'mg', 'mg', 'mg'))
#' print(Balance.D1)
#' plot(Balance.D1)
#'
#' @export
#'

calibCert <- function (balanceID = 'BalanceID', serial = NULL, certificate = NULL,
                       d, d.units = 'mg',
                       indError, indError.units = c('g', 'mg', 'mg'),
                       expanded = TRUE, k = 2,
                       rep, rep.units = c('g', 'mg'),
                       eccen, eccen.units = 'mg',
                       classSTD = NULL, traceability = NULL,
                       Temp = NULL, p = NULL, h = NULL,
                       unitsENV = c('deg.C', 'hPa', '%'),
                       institution = NULL, accreditation = NULL,
                       date = NULL, add.info = NULL) {

  if (length(nrow(indError)) < 2) stop('At least two calibration points must be provided.')

  if (expanded) {
    indErrorUncertExpand <- indError[, 3]
    indError[, 3] <- indError[, 3] / k
  } else {
    indErrorUncertExpand <- indError[, 3] * k
  }

  indError <- data.frame(indError[, 1],
                         convertMassUnitsSI(indError.units[2], indError.units[1], indError[, 2]),
                         convertMassUnitsSI(indError.units[3], indError.units[1], indError[, 3]),
                         indErrorUncertExpand)

  if (class(rep) %in% c('numeric', 'integer')) {
    rep[1] <- convertMassUnitsSI(rep.units[1], indError.units[1], rep[1])
    rep[2] <- convertMassUnitsSI(rep.units[2], indError.units[1], rep[2])
    rep.natur <- 'single'
  } else {
    rep[, 1] <- convertMassUnitsSI(rep.units[1], indError.units[1], rep[, 1])
    rep[, 2] <- convertMassUnitsSI(rep.units[2], indError.units[1], rep[, 2])
    rep.natur <- 'multi'
  }

  calibCert <- list(balanceID = balanceID,
                    standardUnits = indError.units[1],
                    d = convertMassUnitsSI(d.units, indError.units[1], d),
                    indError = indError,
                    rep = rep, rep.natur = rep.natur,
                    eccen = convertMassUnitsSI(eccen.units, indError.units[1], eccen),
                    orgdUnits = d.units,
                    orgIndErrorUnits = indError.units,
                    orgRepUnits = rep.units)



  if (!missing(serial)) calibCert$serial <- serial
  if (!missing(classSTD)) calibCert$classSTD <- classSTD
  if (!missing(traceability)) calibCert$traceability <- traceability
  if (!missing(p)) calibCert$p <- p
  if (!missing(Temp)) calibCert$Temp <- Temp
  if (!missing(h)) calibCert$h <- h
  if (!missing(unitsENV)) calibCert$unitsENV <- unitsENV
  if (!missing(institution)) calibCert$institution <- institution
  if (!missing(date)) calibCert$date <- date
  if (!missing(add.info)) calibCert$add.info <- add.info
  #if (!missing()) calibCert$ <-

  class(calibCert) <- 'calibCert'
  return(calibCert)
}

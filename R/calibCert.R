#' creates a calibCert class object with the balance calibration information
#'
#' ## MettlerToledo XPE204 2021-03-18. Certificate number 5143
#' massSTD <- c(0.01, 0.5, 1, 10, 20, 50, 100, 120, 150, 200, 220)  ## [g]
#' indError <- c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -0.2, -0.2) ## [mg]
#' uncert <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.2, 0.2, 0.3, 0.3, 0.4, 0.5) / 2 ## [mg]
#' d <- 0.1 ## [mg]
#' classSTD <- 'E2'
#' certSTD <- '1473 D-K 17296'
#' barPress <- c(750.4, 751.0) ## [hPa]
#' ambTemp <- c(17.4, 17.9) ## [deg.C]
#' relHumid <- c(70.5, 71.4) ## [%]
#' unitsENV <- c('hPa', 'deg.C', '%')
#' date <- '2021/03/18'
#' institution <- 'Instituto Nacional de Metrología de Colombia'
#'
#' calibCert(balanceID = 'MT XPE 204', massSTD = massSTD,
#'           indError = indError, uncert = uncert, d = d,
#'           units = c('g', 'mg', 'mg', 'mg'), classSTD = classSTD,
#'           certSTD = certSTD, barPress = barPress, ambTemp = ambTemp,
#'           relHumid = relHumid, unitsENV = unitsENV, institution = institution)
#'

calibCert <- function(balanceID = 'Balance',
                      massSTD,
                      indError,
                      uncert,
                      d,
                      units = c('g', 'mg', 'mg', 'mg'),
                      classSTD = NULL,
                      certSTD = NULL,
                      barPress = NULL,
                      ambTempe = NULL,
                      relHumid = NULL,
                      unitsENV = NULL,
                      institution = NULL,
                      date = NULL,
                      details = NULL
                      ) {
  if (length(massSTD) != length(indError) || length(massSTD) != length(uncert)) {
    stop('Vectors in arguments "massSTD", "indError" and "uncert" must be all numeric of same size.')
  }
  calibCert <- list(balanceID = balanceID,
                    standardUnits = units[1],
                    massSTD = massSTD,
                    indError = convertMassUnitsSI(from = units[2], to = units[1], value = indError),
                    uncert = convertMassUnitsSI(from = units[3], to = units[1], value = uncert),
                    d = convertMassUnitsSI(from = units[4], to = units[1], value = d))

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

massSTD <- c(0.01, 0.5, 1, 10, 20, 50, 100, 120, 150, 200, 220)  ## [g]
indError <- c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -0.2, -0.2) ## [mg]
uncert <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.2, 0.2, 0.3, 0.3, 0.4, 0.5) / 2 ## [mg]
d <- 0.1 ## [mg]

MT.XPE.204 <- calibCert(balanceID = 'MT XPE 204', massSTD = massSTD,
                        indError = indError, uncert = uncert, d = d,
                        units = c('g', 'mg', 'mg', 'mg'),
                        classSTD = 'E2',
                        certSTD = '1473 D-K 17296',
                        barPress = c(750.4, 751.0), ## [hPa]
                        ambTemp = c(17.4, 17.9), ## [deg.C]
                        relHumid = c(70.5, 71.4), ## [%]
                        unitsENV = c('hPa', 'deg.C', '%'),
                        institution = 'Instituto Nacional de Metrología de Colombia',
                        date = '2021/03/18')

MT.XPE.204



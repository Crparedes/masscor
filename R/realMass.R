#' Converts balance reading to real mass
#'
#' Conventional Mass: “The conventional value of the result of weighing a body in air is equal to the mass of a
#' standard, of conventionally chosen density, at a conventionally chosen temperature, which balances this body at this
#' reference temperature in air of conventionally chosen density.” The conventions are: reference density 8.0 g/cm3;
#' reference temperature 20 °C; normal air density 0.0012 g/cm3. See OIML D28 (2004)
#' @param
#'
#' @return
#'
#' @examples
#'
#' @export
#'

convMass <- function(reading,
                     units = NULL,
                     calibCert,
                     print = TRUE) {
  if(missing(units)) {
    fc <- 1
    units <- calibCert$standardUnits
  } else {
    if (units == calibCert$standardUnits) {
      fc <- 1
    } else {
      fc <- convertMassUnitsSI(from = units, to = calibCert$standardUnits, value = 1)
    }
  }

  reading <- reading * fc
  #corr <- vector('numeric', length = length(reading))
  if (reading > max(calibCert$massSTD) || reading < min(calibCert$massSTD)) {
    warning('Reading is outside calibration interval: ', min(calibCert$massSTD),
            ' - ', max(calibCert$massSTD), ' [', calibCert$standardUnits, ']')
  }

  p1 <- which.min(abs(calibCert$massSTD - reading))
  p2prim <- min(abs(calibCert$massSTD[-p1] - reading))
  p2 <- which.min(abs(calibCert$massSTD - p2prim))

  Cp_i <- -calibCert$indError[c(p1, p2)]
  mp_i <- calibCert$massSTD[c(p1, p2)]
  correction <- suppressWarnings(
    predict(lm(Cp_i ~ mp_i), newdata = data.frame(mp_i = reading)))
  corrected <- reading + correction
  corrected <- corrected / fc
  if (print) cat(paste0('Masa corregida: ', corrected, ' [', units, ']\n'))
  return(corrected)
}

#MT.XPE.204$massSTD

#convMass(reading = 200, calibCert = MT.XPE.204)



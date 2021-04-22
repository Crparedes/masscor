uncertErrorCorr <- function(reading,
                            units = NULL,
                            calibCert) {
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

  if (reading > max(calibCert$massSTD) || reading < min(calibCert$massSTD)) {
    warning('Reading is outside calibration interval: ', min(calibCert$massSTD),
            ' - ', max(calibCert$massSTD), ' [', calibCert$standardUnits, ']')
  }
  reading <- reading * fc

  p1 <- which.min(abs(calibCert$massSTD - reading))
  p2prim <- min(abs(calibCert$massSTD[-p1] - reading))
  p2 <- which.min(abs(calibCert$massSTD - p2prim))

  u_E <- sqrt(calibCert$uncert[p1]^2 + calibCert$uncert[p2]^2)
  return(u_E)
}

uncertReading <- function(calibCert,
                          repValues = NULL,
                          units = NULL) {
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

  u_d <- sqrt(calibCert$d^2/6^2)
  if (missing(repValues)) {
    u_r <- u_d
  }-r else {
    repValues <- repValues * fc
    u_r <- sqrt(u_d^2 + sd(repValues)^2)
  }
  return(u_r)
}

uncertMassConv <- function(reading,
                           units = NULL,
                           calibCert,
                           repValues = NULL) {
  u_E <- uncertErrorCorr(reading = reading,
                         units = units,
                         calibCert = calibCert)
  u_R <- uncertReading(calibCert = calibCert,
                       repValues = repValues,
                       units = units)
  u_m <- sqrt(u_E^2 + u_R^2)
  return(u_m)
}


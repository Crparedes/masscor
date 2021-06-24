#' Uncertainty of conventional mass correction
#'
#' Given a balance reading indication and the calibration information of the balance, the function
#' uses the conventional mass correction uncertainties of the two closest calibration
#' points to the balance reading to estimate the uncertainty due to the conventional mass
#' correction.
#'
#' Calculations involve interpolation...  the quadratic sum of the uncertainties corresponding to
#' the conventional mass corrections for the two mass standards closest to the
#' balance reading.
#'
#' @inheritParams convMass
#'
#' @return Uncertainty of conventional mass correction
#'
#' @examples
#'   data(minimalCert)
#'   uncertErrorCorr(reading = 12.4835, calibCert = minimalCert)
#' @export
#' @seealso [convMass()], [uncertReading()], [uncertMassConv()]

uncertErrorCorr <- function(calibCert,
                            reading,
                            units = NULL) {
  if(missing(units)) {
    fc <- 1
    units <- calibCert$standardUnits
  } else {
    fc <- convertMassUnitsSI(from = units, to = calibCert$standardUnits, value = 1)
  }


  if (reading > max(calibCert$indError[, 1]) || reading < min(calibCert$indError[, 1])) {
    warning('Reading is outside calibration interval: ', min(calibCert$indError[, 1]),
            ' - ', max(calibCert$indError[, 1]), ' [', calibCert$standardUnits, ']')
  }
  reading <- reading * fc


  p1 <- which.min(abs(calibCert$indError[, 1] - reading))
  p2prim <- min(abs(calibCert$indError[, 1][-p1] - reading))
  p2 <- which(abs(calibCert$indError[, 1] - reading) == p2prim)

  #u_E <- sqrt(calibCert$indError[, 3][p1]^2 + calibCert$indError[, 3][p2]^2)
  u_E <- calibCert$indError[, 3][c(p1, p2)]
  mp_i <- calibCert$indError[, 1][c(p1, p2)]
  uncert <- suppressWarnings(
    predict(lm(u_E ~ mp_i), newdata = data.frame(mp_i = reading)))

  return(as.numeric(uncert / fc))
}

#' Uncertainty of balance readings
#'
#' Combination of readability uncertainty due to scale division and lack of repeatability
#'
#' @inheritParams convMass
#' @param repValues Numeric vector with balance readings for the same mass standard under repeatability conditions
#'   or (single numeric value) with the standard deviation
#' @param d Scale division used in balance reading. Useful when operating the balance at a
#'   division scale different from that specified in the calibration certificate (e.g. when operating a
#'   semimicro balance with only four digital places). If not provided. the functions uses
#'   the balance division scale stated in the calibration certificate.
#' @param tare Logical. If \code{TRUE} (the default) the tare uncertainty is considered and
#'   conventional mass uncertainty is multiplied by \eqn{\sqrt{2}} to account for the
#'   mass difference involved in taring the balance.
#'
#' @return Uncertainty of balance readings
#' @examples
#' data(minimalCert)
#' uncertReading(calibCert = minimalCert,
#'               repValues = c(5.0000, 5.0000, 4.9999, 4.9999, 4.9999,
#'                             4.9999, 4.9999, 4.9999, 4.9999, 4.9999),
#'               tare = TRUE)
#' @export
#' @importFrom graphics barplot
#' @importFrom stats sd
#' @seealso [uncertErrorCorr()], [uncertMassConv()]
uncertReading <- function(calibCert, reading, units = NULL,
                          sd = NULL, sd.units = NULL,
                          d = NULL, d.units = NULL) {
  if(missing(units)) {
    fc <- 1
    units <- calibCert$standardUnits
  } else {
    fc <- convertMassUnitsSI(from = units, to = calibCert$standardUnits, value = 1)
  }
  reading <- reading * fc

  if (missing(d)) {
    if (!missing(d.units)) warning("Argument 'd.units' ignored because no value was provided to 'd'.")
    d <- calibCert$d
  } else {
    if (!missing(d.units)) {
      d <- convertMassUnitsSI(from = d.units, to = calibCert$standardUnits, value = d)
    } else {
      if (!missing(units)) {
        d <- convertMassUnitsSI(from = units, to = calibCert$standardUnits, value = d)
      } else {
        message('Provided division scale is assumed to be in the balance standard units: [',
                calibCert$standardUnits, ']')
      }
    }
  }
  u_d <- d/sqrt(12)

  if (missing(sd)) {
    if (!missing(sd.units)) warning("Argument 'sd.units' ignored because no value was provided to 'sd'.")
    if (calibCert$rep.natur == 'single') {
      sd <- calibCert$rep[2]
    } else {
      p1 <- which.min(abs(calibCert$rep[, 1] - reading))
      p2prim <- min(abs(calibCert$rep[, 1][-p1] - reading))
      p2 <- which(abs(calibCert$rep[, 1] - reading) == p2prim)

      sd <- max(calibCert$rep[c(p1, p2), 2])
    }
    } else {
      if (!missing(sd.units)) {
        sd <- convertMassUnitsSI(from = sd.units, to = calibCert$standardUnits, value = sd)
      } else {
        if (!missing(units)) {
          sd <- convertMassUnitsSI(from = units, to = calibCert$standardUnits, value = sd)
        } else {
          message('Provided standard deviation is assumed to be in the balance standard units: [',
                  calibCert$standardUnits, ']')
        }
      }
    }

  u_ecc <- reading * abs(calibCert$eccen[2]) /  (2 * calibCert$eccen[1] * sqrt(3))

  u_r <- sqrt(2 * u_d ^ 2 + sd ^ 2 + u_ecc ^ 2)
  return(u_r / fc)
}

#' Uncertainty in conventional mass value
#'
#' The function combines the uncertainty of the conventional mass correction
#' (as obtained by [uncertErrorCorr()])
#' and the uncertainty in the balance reading (as obtained by [uncertReading()]),
#' to produce the uncertainty of a conventional mass value.
#'
#' @inheritParams uncertErrorCorr
#' @inheritParams uncertReading
#' @return Uncertainty of conventional mass values.
#'
#' @examples
#' data(minimalCert)
#' uncertMassConv(reading = 12.4835, calibCert = minimalCert,
#'                repValues = c(100.0000, 100.0000, 99.9999, 99.9999, 99.9999,
#'                              99.9999, 99.9999, 99.9999, 99.9999, 99.9999))
#' @export
#' @seealso [convMass()], [uncertReading()], [uncertErrorCorr()]

uncertConvMass <- function (calibCert, reading, units,
                            sd, sd.units,
                            d, d.units) {
  u_err <- uncertErrorCorr(calibCert, reading, units)
  u_read <- uncertReading(calibCert, reading, units,
                          sd, sd.units,
                          d, d.units)
  return(sqrt(u_err ^ 2 + u_read ^ 2))
}

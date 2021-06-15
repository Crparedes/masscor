#' Uncertainty of conventional mass correction
#'
#' Given a balance reading indication and the calibration information of the balance, the function
#' uses the conventional mass correction uncertainties of the two closest calibration
#' points to the balance reading to estimate the uncertainty due to the conventional mass
#' correction.
#'
#' Calculations involve the quadratic sum of the uncertainties corresponding to
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
  p2 <- which(abs(calibCert$massSTD - reading) == p2prim)

  u_E <- sqrt(calibCert$uncert[p1]^2 + calibCert$uncert[p2]^2)
  return(signif(u_E, 3))
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
uncertReading <- function(calibCert,
                          repValues = NULL,
                          d = NULL,
                          units = NULL,
                          tare = TRUE) {
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

  u_d <- ifelse(missing(d), sqrt(calibCert$d^2/6), sqrt(d^2/6))

  if (missing(repValues)) {
    warning('Uncertainty contribution from repeatability is not being considered.')
    u_r <- u_d
  } else {
    if (length(repValues) > 1) {
      repValues <- repValues * fc
      u_r <- sqrt(u_d^2 + sd(repValues)^2)
    } else {
      u_r <- sqrt(u_d^2 + repValues^2)
    }
  }

  if (tare) u_r <- u_r * sqrt(2)
  return(signif(u_r, 3))
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

uncertConvMass <- function(reading, units = NULL, d = NULL, calibCert,
                           repValues = NULL, tare = TRUE) {
  dd <- ifelse(missing(d), calibCert$d, d)

  if(missing(units)) {
    u_E <- uncertErrorCorr(reading = reading, calibCert = calibCert)
    if(missing(repValues)) {
      u_R <- uncertReading(calibCert = calibCert, tare = tare, d = dd)
    } else {
      u_R <- uncertReading(calibCert = calibCert, repValues = repValues, d = dd)
    }
  } else {
    u_E <- uncertErrorCorr(reading = reading, units = units, calibCert = calibCert)
    if(missing(repValues)) {
      u_R <- uncertReading(calibCert = calibCert, units = units, tare = tare, d = dd)
    } else {
      u_R <- uncertReading(calibCert = calibCert, repValues = repValues, units = units, d = dd)
    }
  }

  u_m <- sqrt(u_E^2 + u_R^2)
  return(signif(u_m, 3))
}

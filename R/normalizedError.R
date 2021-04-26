#' Calculates normalized in balance verification using a mass standard
#'
#' @param reading Balance reading for the standard mass.
#' @param standard It can be one of two options. An object of class \code{"massStandard"}
#'   (see [createMassStandard()]) or the numeric value of the conventional mass of the standard used.
#' @inheritParams convMass
#' @param u_massStandard Uncertainty of the conventional mass of the standard used. Neccesary only if
#'   \code{standard} is not an object of class \code{"massStandard"}
#'   (see [createMassStandard()]).
#' @return Numeric value of normalized error for balance verification using a mass standard.
#'
# @examples
#'
#' @export
#' @seealso [createMassStandard()].
#'
normalizedError <- function(reading, standard, calibCert, u_massStandard = NULL) {
  #if (conventional) {
  #  convmass <- measurement
  #} else {
  #  if (missing(calibCert)) {
  #    warning('Conventional mass should be used for the measurement argument,
  #    or the balance calibration information should
  #    be provided as a "calibCert" class object. See XXXXXX')
  #    convmass <- measurement
  #  } else {
  #    convmass <- convMass(reading = measurement, calibCert = calibCert)
  #  }
  #}

  if (class(standard) != 'massStandard' && missing(u_massStandard)) {
    stop('Argument "standard" is numeric. A value for "u_massStandard" is neccesary.')
  }
  if (class(standard) == 'massStandard') {
    massSTD <- standard$convMass
    if(!missing(u_massStandard)) {
      if(standard$uncert != u_massStandard) {
        warning('The uncertainty of the mass standard will be taken from the object of class
                "massstandard" provided for the argument standard: ', standard$u_massStandard,
                ' instead of the value provided at u_massStandard argument: ',
                u_massStandard)
      }}
    u_massStandard <- standard$uncert
  }
  if (class(standard) == 'numeric') {
    u_massMSR <- standard
    massSTD <- standard
  }

  normErr <- abs(reading - massSTD)/sqrt(u_massMSR^2 + u_massStandard^2)
  if (normErr > 1) warning('Balance measurement is not in tolerance.')
  return(normErr)
}

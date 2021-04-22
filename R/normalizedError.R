#' Calculates normalized error in the conventional mass of a mass standard
#'
#' @param reading balance reading for the standard mass.
#' @param standard object of class "massstandard" with the information of the
#'   mass standard (see createMassStandard), or numeric value with the
#'   conventional mass of the standard being used.
#' @param calibCert optional object of class "calibCert" with the calibration information
#' @param
normalizedError <- function(reading,
                            standard,
                            #conventional = FALSE,
                            calibCert = NULL,
                            u_meas = NULL,
                            u_massStandard = NULL) {
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

  if (missing(calibCert)) {
    if (missing(u_meas)) {
      stop('The argument u_meas must be provided if no balance
           calibration information is provided in the argument calibCert')
    }
    u_massMSR <- u_meas
  } else {
    u_massMSR <- uncertMeasurement(reading = reading, calibCert = calibCert)
  }

  if (class(standard) == "massstandard") {
    massSTD <- standard$ConvMass
    if(!missing(u_massStandard)) {
      warning('The uncertainty of the mass standar will be taken from the object of class
              "massstandard" provided for the argument standard: ', standard$u_massStandard,
              ' insthead of the numeric value provided at u_massStandard argument: ',
              u_massStandard)}
    u_massSTD <- standard$u_massStandard
  } else {
    if (is.numeric(standard)) {
      massSTD <- standard
    } else {
      stop('Argument standard must be an object of class  "massstandard"  or numeric
           conventional mass of the standard. See XXXXXX')
    }
    if (is.numeric(u_massStandard)) {
      u_massSTD <- u_massStandard
    } else {
      stop('Argument u_massStandard must be numeric when standard is not an
           object of class  "massstandard". See XXXXXX')
    }
  }

  normErr <- abs(reading - massSTD)/sqrt(u_massMSR^2 + u_massSTDR^2)
  if (normErr > 1) warning('Balance measurement is not in tolerance')
  return(normErr)
}

#' S3 method for printing objects of class \code{"massStandardKit"}
#'
#' The function prints objects of class \code{"massStandardKit"} in a certificate-like fashion.
#' @inheritParams print.massStandard
#' @param x Object of class \code{"massStandardKit"}.
#' @seealso [massStandardKit()], [print.massStandard()]
#' @examples
#' data(Box.E2.MS.Kit)
#' print(Box.E2.MS.Kit, minimal = TRUE)
#' print(Box.E2.MS.Kit)
#' @export
print.massStandardKit <- function(x, minimal = FALSE, description = TRUE, institution = TRUE,
                               density = TRUE, envConditions = TRUE, addInfo = TRUE, ...) {
  units <- paste0('[', x[[1]]$standardUnits, ']', collapse = '')

  coreInfo <- data.frame('Nominal mass' = x$merged$nominal, '.' = rep(units, length(x$merged$nominal)),
                         'Conv mass correction' = x$merged$convMassCor,
                         'Conv mass' = x$merged$nominal + x$merged$convMassCor,
                         'Uncertainty' = x$merged$expandUncert)

  if (minimal) {
    cat('CALIBRATED MASS STANDARD:', x$nominal, x$standardUnits,'\n\n')
    print(coreInfo)
    cat('\nUncertainty is expanded uncertainty with a coverage factor of', x$k, '\n')
  } else {
    if (x$partofakit) {
      warning('The mass standard is part of a mass standards kit. To see complete calibration data
      please print the object of class "massStandardKitKit" to which the object
      "massStandardKit" belongs to.')
      cat('\n')
    }
    cat('CALIBRATED MASS STANDARD:', x$nominal, x$standardUnits,'\n\n')

    if (description) {
      cat('Description')
      cat('\n         Class:', ifelse(is.null(x$class), 'Not provided', x$class))
      cat('\n        Serial:', ifelse(is.null(x$serial), 'Not provided', x$serial))
      cat('\n  Manufacturer:', ifelse(is.null(x$manufacturer), 'Not provided', x$manufacturer))
      cat('\n\n')
    }
    cat('Mass information:\n')
    print(coreInfo)
    cat('\nUncertainty is expanded uncertainty with a coverage factor of', x$k, '\n')
    cat('\n\n')

    if (institution) {
      cat('  Calibration perfomed by:',
          ifelse(!is.null(x$institution), x$institution, 'Not provided'))#, '\n')
      cat('\n                     Date:',
          ifelse(!is.null(x$date), x$date, 'Not provided'))#, '\n')
      cat('\n       Certificate number:',
          ifelse(!is.null(x$certificate), x$certificate, 'Not provided'))#, '\n')
      cat('\n Calibration traceability:',
          ifelse(!is.null(x$traceability), x$traceability, 'Not provided'))#, '\n')
      cat('\n\n')
    }

    if (envConditions) {
      cat('Environmental conditions')
      cat('\n                Temperature:',
          ifelse(is.null(x$Temp), 'Not provided',
                 paste0(paste0(x$Temp, collapse = ' - '), ' [', x$unitsENV[1], ']', collapse = '')))
      cat('\n        Barometric pressure:',
          ifelse(is.null(x$p), 'Not provided',
                 paste0(paste0(x$p, collapse = ' - '), ' [', x$unitsENV[2], ']', collapse = '')))
      cat('\n          Relative humidity:',
          ifelse(is.null(x$h), 'Not provided',
                 paste0(paste0(x$h, collapse = ' - '), ' [', x$unitsENV[3], ']', collapse = '')))
      cat('\n\n')
    }

    if (density) {
      cat('Mass standard density: ', x$rho, '±', x$u_rho, paste0(' [',x$unitsrho, ']', collapse = ''))
      cat('\n\n')
    }

    if (addInfo) {
      cat(ifelse(is.null(x$add.info),
                 'No additional information provided.',
                 c('Additional information:\n', x$add.info)))
    }
  }
  # class(x) <- "list"
  # print(x, ...)
}


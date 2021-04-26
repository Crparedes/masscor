#' Creates an object of class \code{"massStandard"}.
#'
#' The can object of class \code{"massStandard"} be used in routine balance verification to calculate
#' normalized error. See [normalizedError()].
#'
#' Several objects of class \code{"massStandard"} can be concatenated in a single list if the mass
#' standards belongs to the same
#'
#' @param standardID The standard ID.
#' @param nominal Nominal mass of the mass standard.
#' @param convMass Conventional mass of mass standard.
#' @param uncert Standard uncertainty of the conventional mass.
#' @param units Character vector of length 2 with the units of \code{convMass} and \code{uncert}.
#'   Default is \code{c('g', 'mg')} which is typical for a mass standard used for analytic balance.
#' @param certificate Character with the certificate information of the mass standard.
#'
#' @return Object of class \code{"massStandard"}
#'
#' @examples
#' Box1.E2.10g <- createMassStandard('E2.10g', nominal = 10, convMass = 10.001,
#'                                   uncert = 0.1, units = c('g', 'mg'))
#' print(Box1.E2.10g)
#' @export
#' @seealso [normalizedError()]
createMassStandard <- function(standardID, nominal, convMass, uncert, units = c('g', 'mg'),
                               certificate = NULL) {
  massStandard <- list(standardID = standardID,
                       nominal = nominal,
                       convMass = convMass,
                       uncert = uncert,
                       units = units)

  if (!missing(certificate)) massStandard$certificate <- certificate

  class(massStandard) <- 'massStandard'
  return(massStandard)
}

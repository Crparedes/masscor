#' Creates an object of class \code{"massStandard"}.
#'
#' The can object of class \code{"massStandard"} be used in routine balance verification to calculate
#' normalized error. See [normalizedError()].
#'
#' Several objects of class \code{"massStandard"} can be concatenated in a single list if the mass
#' standards belongs to the same
#'
#' @param nominal Nominal mass of the mass standard.
#' @param convMassCor Conventional mass correction for the mass standard.
#' @param uncert Standard uncertainty of the conventional mass.
#' @param units Character vector of length 3 with the units of \code{nominal}, \code{convMass} and \code{uncert}, respectively.
#'   Default is \code{c('g', 'mg', 'mg')}.
#' @param serial Serial number of the mass standard.
#' @param manufacturer Character with the manufacturer of the weights.
#' @param class Character with the claimed class of the mass standard according to REF REF (XXXX).
#' @param rho Density...
#' @param u_rho Uncertainty in ...
#' @param unitsrho Units
#' @inheritParams calibCert
#'
#' @return Object of class \code{"massStandard"}
#'
#' @examples
#' Box1.E2.10g <- createMassStandard(nominal = 10, convMass = 10.001,
#'                                   uncert = 0.1, units = c('g', 'mg', 'mg'))
#' print(Box1.E2.10g)
#' @export
#' @seealso [normalizedError()]

massStandard <- function(nominal, convMassCor, uncert, units = c('g', 'mg', 'mg'),
                         serial = NULL, manufacturer = NULL, class = NULL, certificate = NULL, traceability = NULL,
                         Temp = NULL, p = NULL, h = NULL,
                         unitsENV = c('deg.C', 'hPa', '%'),
                         expanded = TRUE, k = 2,
                         rho = 8000, u_rho = 60, unitsrho = 'kg/m^3',
                         institution = NULL, date = NULL, add.info = NULL) {

  massStandard <- list(#standardID = standardID,
                       nominal = nominal,
                       convMassCor = convMassCor,
                       uncert = uncert,
                       units = units)

  if (!missing(certificate)) massStandard$certificate <- certificate

  class(massStandard) <- 'massStandard'
  return(massStandard)
}

#' Creates an object of class \code{"massStandard"}.
#'
#' The object of class \code{"massStandard"} contains the calibration information of a mass standard
#' that is used in routine balance verification (e g. to calculate
#' normalized error. See [normalizedError()]). A version to store information about several mass
#' standards that belong to the same kit is [massStandardKit()].
#'
#' @inheritSection calibCert unitsENV
#'
#' @seealso [massStandardKit()], [normalizedError()]
#' @param nominal Nominal mass of the mass standard.
#' @param convMassCor Conventional mass correction for the mass standard.
#' @param uncert Standard uncertainty of the conventional mass correction.
#' @param units Character vector of length 3 with the units of \code{nominal}, \code{convMass} and
#'   \code{uncert}, respectively.
#'   Default is \code{c('g', 'mg', 'mg')}.
#' @param serial Serial number of the mass standard or mass standards kit.
#' @param manufacturer Character with the manufacturer of the mass standard or mass standards kit.
#' @param class Character with the claimed class of the mass standard or mass standards kit,
#'   according to REF REF (XXXX).
#' @param rho Density of the mass standard.
#' @param u_rho Uncertainty in the density of the mass standard.
#' @param unitsrho Units of the density of the mass standard. Default is \code{'kg/m^3'}.
#' @param partofakit Logical. Is the mass standard part of kit?
#' @inheritParams calibCert
#'
#' @return Object of class \code{"massStandard"} with the information of a calibrated mass standard.
#'
#' @examples
#' singleMS.E2.10g <- massStandard(nominal = 10, convMassCor = 0.001, uncert = 0.1,
#'                                 units = c('g', 'mg', 'mg'))
#' print(singleMS.E2.10g)
#' @export

massStandard <- function(nominal, convMassCor, uncert, units = c('g', 'mg', 'mg'),
                         serial = NULL, manufacturer = NULL, class = NULL, certificate = NULL,
                         traceability = NULL,
                         Temp = NULL, p = NULL, h = NULL,
                         unitsENV = c('deg.C', 'hPa', '%'),
                         expanded = TRUE, k = 2,
                         rho = 8000, u_rho = 60, unitsrho = 'kg/m^3',
                         institution = NULL, date = NULL, add.info = NULL,
                         partofakit = FALSE) {

  massStandard <- list(#standardID = standardID,
                       nominal = nominal,
                       convMassCor = convertMassUnitsSI(from = units[2], to = units[1], value = convMassCor),
                       convMass = nominal + convertMassUnitsSI(from = units[2], to = units[1], value = convMassCor),
                       uncert = convertMassUnitsSI(from = units[2], to = units[1], value = uncert),
                       expandUncert = convertMassUnitsSI(from = units[2], to = units[1], value = uncert),
                       k = k,
                       standardUnits = units[1], originalUnits = units,
                       rho = rho, u_rho = u_rho, unitsrho = unitsrho, partofakit = partofakit)

  if (expanded) {
    massStandard$uncert <- massStandard$uncert / k
  } else {
    massStandard$expandUncert <- massStandard$expandUncert * k
  }

  if (!missing(serial)) massStandard$serial <- serial
  if (!missing(manufacturer)) massStandard$manufacturer <- manufacturer
  if (!missing(class)) massStandard$class <- class
  if (!missing(certificate)) massStandard$certificate <- certificate
  if (!missing(traceability)) massStandard$traceability <- traceability
  if (!missing(Temp)) massStandard$Temp <- Temp
  if (!missing(p)) massStandard$p <- p
  if (!missing(h)) massStandard$h <- h
  if (!missing(unitsENV)) massStandard$unitsENV <- unitsENV
  if (!missing(rho)) massStandard$rho <- rho
  if (!missing(u_rho)) massStandard$u_rho <- u_rho
  if (!missing(unitsrho)) massStandard$unitsrho <- unitsrho
  if (!missing(institution)) massStandard$institution <- institution
  if (!missing(date)) massStandard$date <- date
  if (!missing(add.info)) massStandard$add.info <- add.info
 #if (!missing()) massStandard$ <-

  class(massStandard) <- 'massStandard'
  return(massStandard)
}


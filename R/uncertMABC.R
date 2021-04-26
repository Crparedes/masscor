#' Uncertainty of the Magnitude of Air Buoyancy Correction Magnitude
#'
#' Propagates density uncertainties to calculate the uncertainty of the Air Buoyancy Correction Magnitude
#' (See [MABC()]).
#'
#' Calculations are made according to the
#' Guide to the Guide to the expression of uncertainty in measurement (GUM, JCGM, 2008) as implemented
#' by the package \link[propagate]{propagate} (Spiess, 2018). If air density and associated uncertainty
#' are not provided the default output values of the functions [airDensity()] and [uncertAirDensity()],
#' respectively, are used.
#'
#' @inheritParams MABC
#' @inheritParams uncertAirDensity
#' @param u_rho_s Standard uncertainty of the sample density.
#' @param u_rho_w Standard uncertainty of the mass standard density.
#' @param u_rho_air Standard uncertainty of air density. See [uncertAirDensity()].
#'
#' @references
#' BIMP JCGM (2008) Evaluation of measurement data — Guide to the expression of uncertainty in measurement.
#'
#' Andrej-Nikolai Spiess (2018). propagate: Propagation of Uncertainty. R package version 1.0-6.
#' https://CRAN.R-project.org/package=propagate
#'
#' @importFrom propagate propagate
#' @export

uncertMABC <- function(rho_s = 0.9980,
                       rho_w = 8,
                       rho_air = NULL,
                       u_rho_s = 0.0001,
                       u_rho_w = 0.0001,
                       u_rho_air = NULL,
                       plot = FALSE) {
  if (missing(rho_air)) rho_air <- airDensity()
  if (missing(u_rho_air)) u_rho_air <- uncertAirDensity(printRelSD = FALSE)


  MABC <- expression((1 - rho_air/rho_w) / (1 - rho_air/rho_s))
  uncertMABC <- propagate::propagate(expr = MABC,
                                     data = cbind(rho_air = c(rho_air, u_rho_air),
                                                  rho_s = c(rho_s, u_rho_s),
                                                  rho_w = c(rho_w, u_rho_w)),
                                     do.sim = FALSE)
  return(as.numeric(uncertMABC$prop[3]))
}

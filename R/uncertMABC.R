#' Uncertainty of the Magnitude of Air Buoyancy Correction Magnitude
#'
#' Propagates density uncertainties in the calculation
#' of the Magnitude of Air Buoyancy Correction  (See [MABC()]).
#'
#' Calculations are made according to the
#' Guide to the Guide to the expression of uncertainty in
#' measurement (GUM, JCGM, 2008) as implemented
#' by the package \link[propagate]{propagate} (Spiess, 2018).
#' If air density and associated uncertainty
#' are not provided the default output values of the
#' functions [airDensity()] and [uncertAirDensity()], respectively, are used.
#'
#' @inheritParams MABC
#' @inheritParams uncertAirDensity
#' @param u_rho standard uncertainty of the sample density.
#' @param u_rho_w standard uncertainty of the mass standard density.
#' @param u_rho_air standard uncertainty of air density.
#'   See [uncertAirDensity()].
#'
#' @references
#' BIMP JCGM (2008) Evaluation of measurement data — Guide
#' to the expression of uncertainty in measurement.
#'
#' Andrej-Nikolai Spiess (2018). propagate: Propagation of
#' Uncertainty. R package version 1.0-6.
#' https://CRAN.R-project.org/package=propagate
#'
#' @importFrom propagate propagate
#' @export

uncertMABC <- function(rho = 0.9980,
                       rho_w = 8,
                       rho_air = NULL,
                       u_rho = 0.0001,
                       u_rho_w = 0.006,
                       u_rho_air = NULL,
                       plot = TRUE,
                       printRelSD = TRUE) {
  if (missing(rho_air)) rho_air <- airDensity()
  if (missing(u_rho_air)) u_rho_air <- uncertAirDensity(printRelSD = FALSE)


  MABC <- expression((1 - rho_air/rho_w) / (1 - rho_air/rho))
  uncertMABC <- propagate::propagate(
    expr = MABC,
    data = cbind(rho_air = c(rho_air, u_rho_air),
                 rho = c(rho, u_rho),
                 rho_w = c(rho_w, u_rho_w)),
    do.sim = FALSE)
  if (plot) {
    barplot(diag(uncertMABC$rel.contr)[which(diag(uncertMABC$rel.contr) > 0)])
  }

  if (printRelSD) {
    cat(paste0('Relative uncertainty in MABC: ',
               round(uncertMABC$prop[3]/uncertMABC$prop[1]*100, 4), ' %\n\n'))
  }
  return(as.numeric(uncertMABC$prop[3]))
}

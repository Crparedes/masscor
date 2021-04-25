#' Standard uncertainty for the Air Buoyancy Correction Magnitude
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

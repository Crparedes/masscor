#' Uncertainties in air density calculation
#'
#' Calculates the approximated density of the air in the laboratory according to its height above sea level
#'
#'
#' @param HASL height altitude above sea level in meters
#' @importFrom propagate propagate
#' @return air density value
#'
#' @examples
#' airDensity(Temp = 22.3, p = 748.1, h = 37, units = c('deg.C', 'mmHg', '%')) # [g/cm^3]
#' airDensity(Temp = 23.4, p = 612.3, h = 23, units = c('deg.C', 'mmHg', '%')) # [g/cm^3]
#' airDensity(Temp = 23.4, p = 612.3, h = 23, units = c('deg.C', 'mmHg', '%'), opt = 'A') # [g/cm^3]
#'
#' @export

uncertAirDensities <- function(value = NULL,
                               opt = 'CIMP2007',
                               Temp = 20,
                               p = 1013.25,
                               h = 50,
                               u_Temp = 2.9,
                               u_p = 10.10,
                               u_h = 11.3,
                               plot = TRUE,
                               units = c('deg.C', 'hPa', '%')) {
  # if (method != 'CIMP2007') stop('The function calculates uncertainties only for air densities calculated with')
  # if (method == 'HASL') u_form <- value * 1.2e-2 # diapositiva 118 Andr'es
  if (any(opt == c('Jones1978', 'HASL'))) {
    stop('The "Jones1978" and "HASL" models uncertainties are not avaiable.
         We reccomend calculating air density by choosing one of CIMP models.')
  }

  Temp <- convertTemperature(from = units[1], to = 'K', value = Temp)
  Temp <- c(Temp, u_Temp)
  h <- convertRelHum(from = units[3], to = 'frac', value = c(h, u_h))

  if (opt == 'CIMP.approx') {
    u_form <- 2.4e-4 # Diapositiva 117
    p <- convertPressure(from = units[2], to = 'kPa', value = c(p, u_p))
    rho_air_exp <- expression((3.4848 * p - (0.9024 * h * exp(0.0612 * (Temp - 273.15))))/ (Temp) / 1000 * f_Ec)
  }

  if (opt == 'CIMP2007') {
    u_form <- 22e-6 # A Picard et al Metrologia 45 (2008) 149–155 Table 2
    p <- convertPressure(from = units[2], to = 'Pa', value = c(p, u_p))

    rho_air_exp <- expression((((p*28.96546e-3)/(
      (1 - ((p/Temp)*(1.58123e-6 + (-2.9331e-8)*(Temp - 273.15) +
                        1.1043e-10*(Temp - 273.15)^2 +
                        (5.707e-6 + (-2.051e-8)*(Temp - 273.15))*
                        (h * (1.00062 + 3.14e-8*p + 5.6e-7*(Temp - 273.15)^2) *
                           (1.2378847e-5*Temp^2 + (-1.9121316e-2)*Temp + 33.93711047 + (-6.3431645e3)/Temp) / p) +
                        (1.9898e-4 + -2.376e-6*(Temp - 273.15))*
                        (h * (1.00062 + 3.14e-8*p + 5.6e-7*(Temp - 273.15)^2) *
                           (1.2378847e-5*Temp^2 + (-1.9121316e-2)*Temp + 33.93711047 + (-6.3431645e3)/Temp) / p)^2)) +
         ((p^2/Temp^2)*(1.83e-11 + -0.765e-8*
                          (h * (1.00062 + 3.14e-8*p + 5.6e-7*(Temp - 273.15)^2) *
                             (1.2378847e-5*Temp^2 + (-1.9121316e-2)*Temp + 33.93711047 + (-6.3431645e3)/Temp) / p)^2)))*8.314472*Temp)) *
                                (1 - (h * (1.00062 + 3.14e-8*p + 5.6e-7*(Temp - 273.15)^2) *
                                        (1.2378847e-5*Temp^2 + (-1.9121316e-2)*Temp + 33.93711047 + (-6.3431645e3)/Temp) / p) * (1 - 18.01528e-3/28.96546e-3))) *
        10^-3 * f_Ec)
  }

  uncert <- propagate::propagate(expr = rho_air_exp,
                                 data = cbind(Temp = Temp, p = p, h = h,
                                              f_Ec = c(1, u_form)),
                                 do.sim = FALSE)
  if (plot) barplot(diag(uncert$rel.contr))
  print(cat(paste0(round(uncert$prop[1], 8), ' +/- ', round(uncert$prop[3], 8), ' [g/cm^3]')))
  print(round(uncert$prop[3]/uncert$prop[1]*100, 4))
  return(uncert$prop[3])
}

airDensity(opt = 'CIMP.approx')
uncertAirDensities(opt = 'CIMP.approx')
uncertAirDensities(opt = 'CIMP.approx', u_Temp = 0.29, u_p = 1.01, u_h = 11.3)
uncertAirDensities(opt = 'CIMP.approx', u_Temp = 0.1, u_p = 0.665, u_h = 10)

airDensity(opt = 'CIMP2007')
uncertAirDensities(opt = 'CIMP2007')
uncertAirDensities(opt = 'CIMP2007', u_Temp = 0.29, u_p = 1.01, u_h = 11.3)
uncertAirDensities(opt = 'CIMP2007', u_Temp = 0.1, u_p = 0.665, u_h = 10)

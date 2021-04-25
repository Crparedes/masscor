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

uncertAirDensity <- function(value = NULL,
                             opt = 'CIMP2007',
                             Temp = 20,
                             p = 1013.25,
                             h = 50,
                             u_Temp = 2.9,
                             u_p = 10.10,
                             u_h = 11.3,
                             plot = FALSE,
                             units = c('deg.C', 'hPa', '%'),
                             printRelSD = TRUE) {
  # if (method != 'CIMP2007') stop('The function calculates uncertainties only for air densities calculated with')
  # if (method == 'HASL') u_form <- value * 1.2e-2 # diapositiva 118 Andr'es
  if (any(opt == c('Jones1978', 'HASL'))) {
    stop('The "Jones1978" and "HASL" models uncertainties are not avaiable.
         We reccomend calculating air density by using one of CIMP models.')
  }

  Temp <- convertTemperature(from = units[1], to = 'K', value = Temp)
  Temp <- c(Temp, u_Temp)
  h <- convertRelHum(from = units[3], to = 'frac', value = c(h, u_h))

  if (opt == 'CIMP.approx') {
    u_form <- 2.4e-4 # Diapositiva 117 ANDRES
    p <- convertPressure(from = units[2], to = 'kPa', value = c(p, u_p))
    rho_air_exp <- expression((3.4848 * p - (0.9024 * h * exp(0.0612 * (Temp - 273.15))))/ (Temp) / 1000 * f_Ec)

    uncert <- propagate::propagate(expr = rho_air_exp,
                                   data = cbind(Temp = Temp, p = p, h = h,
                                                f_Ec = c(1, u_form)),
                                   do.sim = FALSE)
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

    uncert <- propagate::propagate(expr = rho_air_exp,
                                   data = cbind(Temp = Temp, p = p, h = h,
                                                f_Ec = c(1, u_form)),
                                   do.sim = FALSE)


    if(F){ # This chunck of code is another (non equivalent?) way to look for CIMP uncertainty

      #rho_air_exp <- expression(((p*M_a)/(Z*R*Temp)) * (1 - x_v * (1 - M_v/M_a))* 10^-3 * f_Ec)

      #uncert <- propagate::propagate(expr = rho_air_exp,
      #                               data = cbind(Temp = Temp, p = p, h = h,
      #                                            f_Ec = c(1, u_form)),
      #                               do.sim = FALSE)

    rho_air_exp <- expression(((p * M_a)/
                                 ((1 - ((p/Temp)*
                                          (a0 + a1*(Temp - 273.15) + a2*(Temp - 273.15)^2 +
                                             (b0 + b1*(Temp - 273.15))*
                                             (h * (1.00062 + 3.14e-8*p + 5.6e-7*(Temp - 273.15)^2) *
                                                (A*Temp^2 + B*Temp + C + D/Temp) / p) +
                                             (c0 + c1*(Temp - 273.15))*
                                             (h * (1.00062 + 3.14e-8*p + 5.6e-7*(Temp - 273.15)^2) *
                                                (A*Temp^2 + B*Temp + C + D/Temp) / p)^2)) +
                                     ((p^2/Temp^2)*
                                        (d + e*(h * (1.00062 + 3.14e-8*p + 5.6e-7*(Temp - 273.15)^2) *
                                                  (A*Temp^2 + B*Temp + C + D/Temp) / p)^2))) *
                                    R * Temp)) *
                                (1 - (h * (1.00062 + 3.14e-8*p + 5.6e-7*(Temp - 273.15)^2) *
                                        (A*Temp^2 + B*Temp + C + D/Temp) / p) *
                                   (1 - M_v/ M_a)) * 10^-3 * f_Ec)

    uncert <- propagate::propagate(expr = rho_air_exp,
                                   data = cbind(Temp = Temp, p = p, h = h,
                                                f_Ec = c(1, u_form),
                                                M_a = c(28.96546e-3, 0), # [kg / mol] molar mass of the air within laboratory
                                                M_v = c(18.01528e-3, 0), # [kg / mol] $/pm$ 0.00017e-3
                                                R = c(8.314472, 0), # [J / (mol K)]  $/pm$ 0.000015 universal gas constant
                                                A = c(1.2378847e-5, 0), # [K^-2]
                                                B = c(-1.9121316e-2, 0), # [K^-1]
                                                C = c(33.93711047, 0), # []
                                                D = c(-6.3431645e3, 0), # [K]
                                                a0 = c(1.58123e-6, 0), # [K Pa^-1]
                                                a1 = c(-2.9331e-8, 0), # [Pa^-1]
                                                a2 = c(1.1043e-10, 0), # [K^-1 Pa^-1]
                                                b0 = c(5.707e-6, 0), # [K Pa^-1]
                                                b1 = c(-2.051e-8, 0), # [Pa^-1]
                                                c0 = c(1.9898e-4, 0), # [K Pa^-1]
                                                c1 = c(-2.376e-6, 0), # [Pa^-1]
                                                d = c(1.83e-11, 0), # [K^2 Pa^-2]
                                                e = c(-0.765e-8, 0) # [K^2 Pa^-2]
                                   ),
                                   do.sim = FALSE)}

  }
  if (plot) barplot(diag(uncert$rel.contr)[which(diag(uncert$rel.contr) > 0)])
#  print(cat(paste0(round(uncert$prop[1], 8), ' +/- ', round(uncert$prop[3], 8), ' [g/cm^3]')))
  if (printRelSD) cat(paste0('Relative uncertainty: ', round(uncert$prop[3]/uncert$prop[1]*100, 4), ' %\n\n'))
  return(as.numeric(uncert$prop[3]))
}

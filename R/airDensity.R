#' Air density in [g cm^-3]
#'
#' Calculates the density of the air in the laboratory according to two models that use
#' temperature, barometric pressure and relative humidity.
#'
#' Jones1978 is the method A in the NIST doccument
#' CIMP2007
#' @param Temp temperature
#'
#' @return air density value
#'
#' @examples
#' airDensity(Temp = 22.3, p = 748.1, h = 37, units = c('deg.C', 'mmHg', '%')) # [g/cm^3]
#' airDensity(Temp = 23.4, p = 612.3, h = 23, units = c('deg.C', 'mmHg', '%')) # [g/cm^3]
#' airDensity(Temp = 23.4, p = 612.3, h = 23, units = c('deg.C', 'mmHg', '%'), opt = 'A') # [g/cm^3]
#'
#' @export

# https://www.nist.gov/system/files/documents/2019/05/13/sop-2-applying-air-buoyancy-20190506.pdf
airDensity <- function(Temp = 20,
                       p = 1013.25,
                       h = 50,
                       units = c('deg.C', 'hPa', '%'),
                       x_CO2 = 0.0004,
                       opt = 'CIMP2007') { # [g/cm^3]

  if (!(opt %in% c('Jones1978', 'CIMP.approx', 'CIMP2007'))) stop("Option parameter must be 'CIMP2007', 'CIMP.approx' or 'Jones1978'. See details.")
  if (!(units[1] %in% c('deg.C', 'K'))) stop("Temperature units must be 'deg.C' or 'K'.")
  if (!(units[2] %in% c('Pa', 'hPa', 'kPa', 'mmHg'))) stop("Pressure units must be 'Pa', 'hPa', 'kPa' or 'mmHg'.")
  if (!(units[3] %in% c('%', 'ND'))) stop("Relative humidity must be '%' or 'frac' (the latter for values between 0 and 1).")

  #if (units[1] == 'deg.C') Temp <- Temp + 273.15
  Temp <- convertTemperature(from = units[1], to = 'K', value = Temp)

  if (opt == 'Jones1978') {
    p <- convertPressure(from = units[2], to = 'mmHg', value = p)
    h <- convertRelHum(from = units[3], to = '%', value = h)
    if (h < 0 || h > 100) stop("Relative humidity must be between 0 and 1 (or 0%-100%).")
    e_s <- 1.3146e9 * exp(-5315.56/Temp)
    rho_air_exp <- expression(((0.46460 * (p - 0.0037960 * h * e_s))/Temp)*10^-3)
    rho_air <- eval(rho_air_exp)
  }

  if (opt == 'CIMP.approx') {
    p <- convertPressure(from = units[2], to = 'kPa', value = p)
    h <- convertRelHum(from = units[3], to = 'frac', value = h)
    if (h < 0.20 || h > 0.80) stop("For CIMP.approx the relative humidity must be between 0.2 and 0.8 (or 20%-80%).")
    if (Temp < (273.15 + 15) || Temp > (273.15 + 27)) stop("For CIMP.approx the temperatures must be between 15 and 27 deg.C.")
    if (p < 60 || p > 110) stop("For CIMP.approx the barometric pressure must be between 60 and 110 hPa.")
    rho_air <- (3.4848 * p - (0.9024 * h * exp(0.0612 * (Temp - 273.15))))/ (Temp) / 1000
  }

  if (opt == 'CIMP2007') {
    p <- convertPressure(from = units[2], to = 'Pa', value = p)
    h <- convertRelHum(from = units[3], to = 'frac', value = h)
    if (h < 0 || h > 1) stop("Relative humidity must be between 0 and 1 (or 0%-100%).")

    rho_air_exp <- expression(((p*M_a)/(Z*R*Temp)) * (1 - x_v * (1 - M_v/M_a)))
    x_v_exp <- expression(h * f * p_sv / p)
    Z_exp <- expression(1 - ((p/Temp)*(a0 + a1*t + a2*t^2 + (b0 + b1*t)*x_v + (c0 + c1*t)*x_v^2)) + ((p^2/Temp^2)*(d + e*x_v^2)))

    M_a <- (28.96546 + 12.011*(x_CO2 - 0.0004))*10^-3 # [kg / mol] molar mass of the air within laboratory
    M_v <- 18.01528e-3 # [kg / mol] $/pm$ 0.00017e-3
    # p [Pa], ambient barometric pressure
    # Temp [K], ambient temperature
    R <- 8.314472 # [J / (mol K)]  $/pm$ 0.000015 universal gas constant
    # h [], relative humidity
    t <- Temp - 273.15 # [deg.C], ambient temperature
    f <- 1.00062 + 3.14e-8*p + 5.6e-7*t^2 # ??

    A <- 1.2378847e-5 # [K^-2]
    B <- -1.9121316e-2 # [K^-1]
    C <- 33.93711047 # []
    D <- -6.3431645e3 # [K]
    p_sv <- exp(A*Temp^2 + B*Temp + C + D/Temp) # [Pa]

    a0 <- 1.58123e-6 # [K Pa^-1]
    a1 <- -2.9331e-8 # [Pa^-1]
    a2 <- 1.1043e-10 # [K^-1 Pa^-1]
    b0 <- 5.707e-6 # [K Pa^-1]
    b1 <- -2.051e-8 # [Pa^-1]
    c0 <- 1.9898e-4 # [K Pa^-1]
    c1 <- -2.376e-6 # [Pa^-1]
    d <- 1.83e-11 # [K^2 Pa^-2]
    e <- -0.765e-8 # [K^2 Pa^-2]

    x_v <- eval(x_v_exp)
    Z <- eval(Z_exp)
    rho_air <- eval(rho_air_exp) * 10^-3
  }
  return(rho_air)
}

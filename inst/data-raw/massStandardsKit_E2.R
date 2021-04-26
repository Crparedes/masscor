serial <- 'NIM 190301'
class <- 'E2'
manufacturer <- 'Mettler Toledo'
certificate <- '4687'
traceability <- 'Set of weights class E1. PTB (CIPM MRA). Certificate number PTB 11086 19.'
Temp <- c(17.4, 17.9) ## [deg.C]
p <- c(750.4, 751.0) ## [hPa]
h <- c(70.5, 71.4) ## [%]
unitsENV <- c('deg.C', 'hPa', '%')
institution <- 'Instituto Nacional de Metrologia de Colombia'
date <- '2020-08-12'

massStandadsKit_E2 <- list(
  w1000  = massStandard(nominal = 1000, convMassCor = 0.0, uncert = 0.5, units = c('g', 'mg', 'mg'),
                        serial = serial, class = class, manufacturer = manufacturer,
                        certificate = certificate, traceability = traceability,
                        Temp = Temp, p = p, h = h, unitsENV = unitsENV,
                        rho = 8012.217, u_rho = 0.096, unitsrho = 'kg/m^3',
                        institution = institution, date = date),

  w500   = massStandard(nominal = 500, convMassCor = -0.03, uncert = 0.25, units = c('g', 'mg', 'mg'),
                        serial = serial, class = class, manufacturer = manufacturer,
                        certificate = certificate, traceability = traceability,
                        Temp = Temp, p = p, h = h, unitsENV = unitsENV,
                        rho = 8008.640, u_rho = 0.090, unitsrho = 'kg/m^3',
                        institution = institution, date = date),

  w200   = massStandard(nominal = 200, convMassCor = 0.03, uncert = 0.1, units = c('g', 'mg', 'mg'),
                        serial = serial, class = class, manufacturer = manufacturer,
                        certificate = certificate, traceability = traceability,
                        Temp = Temp, p = p, h = h, unitsENV = unitsENV,
                        rho = 8011.126, u_rho = 0.160, unitsrho = 'kg/m^3',
                        institution = institution, date = date),

  w200d  = massStandard(nominal = 200, convMassCor = 0.06, uncert = 0.1, units = c('g', 'mg', 'mg'),
                        serial = serial, class = class, manufacturer = manufacturer,
                        certificate = certificate, traceability = traceability,
                        Temp = Temp, p = p, h = h, unitsENV = unitsENV,
                        rho = 8010.722, u_rho = 0.160, unitsrho = 'kg/m^3',
                        institution = institution, date = date),

  w100   = massStandard(nominal = 100, convMassCor = 0.00, uncert = 0.05, units = c('g', 'mg', 'mg'),
                        serial = serial, class = class, manufacturer = manufacturer,
                        certificate = certificate, traceability = traceability,
                        Temp = Temp, p = p, h = h, unitsENV = unitsENV,
                        rho = 8010.935, u_rho = 0.321, unitsrho = 'kg/m^3',
                        institution = institution, date = date),

  w50    = massStandard(nominal = 50, convMassCor = 0.01, uncert = 0.03, units = c('g', 'mg', 'mg'),
                        serial = serial, class = class, manufacturer = manufacturer,
                        certificate = certificate, traceability = traceability,
                        Temp = Temp, p = p, h = h, unitsENV = unitsENV,
                        rho = 8011.937, u_rho = 0.642, unitsrho = 'kg/m^3',
                        institution = institution, date = date),

  w20    = massStandard(nominal = 20, convMassCor = 0.004, uncert = 0.025, units = c('g', 'mg', 'mg'),
                        serial = serial, class = class, manufacturer = manufacturer,
                        certificate = certificate, traceability = traceability,
                        Temp = Temp, p = p, h = h, unitsENV = unitsENV,
                        rho = 8013.881, u_rho = 1.606, unitsrho = 'kg/m^3',
                        institution = institution, date = date),

  w20d   = massStandard(nominal = 20, convMassCor = 0.01, uncert = 0.025, units = c('g', 'mg', 'mg'),
                        serial = serial, class = class, manufacturer = manufacturer,
                        certificate = certificate, traceability = traceability,
                        Temp = Temp, p = p, h = h, unitsENV = unitsENV,
                        rho = 8013.721, u_rho = 1.606, unitsrho = 'kg/m^3',
                        institution = institution, date = date),

  w10    = massStandard(nominal = 10, convMassCor = 0.012, uncert = 0.020, units = c('g', 'mg', 'mg'),
                        serial = serial, class = class, manufacturer = manufacturer,
                        certificate = certificate, traceability = traceability,
                        Temp = Temp, p = p, h = h, unitsENV = unitsENV,
                        rho = 8006.924, u_rho = 5.770, unitsrho = 'kg/m^3',
                        institution = institution, date = date),

  w5     = massStandard(nominal = 5, convMassCor = 0.007, uncert = 0.016, units = c('g', 'mg', 'mg'),
                        serial = serial, class = class, manufacturer = manufacturer,
                        certificate = certificate, traceability = traceability,
                        Temp = Temp, p = p, h = h, unitsENV = unitsENV,
                        rho = 8006.285, u_rho = 11.538, unitsrho = 'kg/m^3',
                        institution = institution, date = date),

  w2     = massStandard(nominal = 2, convMassCor = 0.020, uncert = 0.012, units = c('g', 'mg', 'mg'),
                        serial = serial, class = class, manufacturer = manufacturer,
                        certificate = certificate, traceability = traceability,
                        Temp = Temp, p = p, h = h, unitsENV = unitsENV,
                        rho = 8009.632, u_rho = 28.869, unitsrho = 'kg/m^3',
                        institution = institution, date = date),

  w2d    = massStandard(nominal = 2, convMassCor = 0.007, uncert = 0.012, units = c('g', 'mg', 'mg'),
                        serial = serial, class = class, manufacturer = manufacturer,
                        certificate = certificate, traceability = traceability,
                        Temp = Temp, p = p, h = h, unitsENV = unitsENV,
                        rho = 8008.352, u_rho = 28.860, unitsrho = 'kg/m^3',
                        institution = institution, date = date),

  w1     = massStandard(nominal = 1, convMassCor = -0.004, uncert = 0.010, units = c('g', 'mg', 'mg'),
                        serial = serial, class = class, manufacturer = manufacturer,
                        certificate = certificate, traceability = traceability,
                        Temp = Temp, p = p, h = h, unitsENV = unitsENV,
                        rho = 7997.479, u_rho = 57.563, unitsrho = 'kg/m^3',
                        institution = institution, date = date),

  w.500  = massStandard(nominal = 500, convMassCor = 0.000, uncert = 0.008, units = c('mg', 'mg', 'mg'),
                        serial = serial, class = class, manufacturer = manufacturer,
                        certificate = certificate, traceability = traceability,
                        Temp = Temp, p = p, h = h, unitsENV = unitsENV,
                        institution = institution, date = date),

  w.200  = massStandard(nominal = 200, convMassCor = 0.009, uncert = 0.006, units = c('mg', 'mg', 'mg'),
                        serial = serial, class = class, manufacturer = manufacturer,
                        certificate = certificate, traceability = traceability,
                        Temp = Temp, p = p, h = h, unitsENV = unitsENV,
                        institution = institution, date = date),

  w.200h = massStandard(nominal = 200, convMassCor = 0.006, uncert = 0.006, units = c('mg', 'mg', 'mg'),
                        serial = serial, class = class, manufacturer = manufacturer,
                        certificate = certificate, traceability = traceability,
                        Temp = Temp, p = p, h = h, unitsENV = unitsENV,
                        institution = institution, date = date),

  w.100  = massStandard(nominal = 100, convMassCor = 0.007, uncert = 0.005, units = c('mg', 'mg', 'mg'),
                        serial = serial, class = class, manufacturer = manufacturer,
                        certificate = certificate, traceability = traceability,
                        Temp = Temp, p = p, h = h, unitsENV = unitsENV,
                        institution = institution, date = date),

  w.050  = massStandard(nominal = 50, convMassCor = 0.001, uncert = 0.004, units = c('mg', 'mg', 'mg'),
                        serial = serial, class = class, manufacturer = manufacturer,
                        certificate = certificate, traceability = traceability,
                        Temp = Temp, p = p, h = h, unitsENV = unitsENV,
                        institution = institution, date = date),

  w.020  = massStandard(nominal = 20, convMassCor = 0.003, uncert = 0.003, units = c('mg', 'mg', 'mg'),
                        serial = serial, class = class, manufacturer = manufacturer,
                        certificate = certificate, traceability = traceability,
                        Temp = Temp, p = p, h = h, unitsENV = unitsENV,
                        institution = institution, date = date),

  w.020h = massStandard(nominal = 20, convMassCor = 0.002, uncert = 0.003, units = c('mg', 'mg', 'mg'),
                        serial = serial, class = class, manufacturer = manufacturer,
                        certificate = certificate, traceability = traceability,
                        Temp = Temp, p = p, h = h, unitsENV = unitsENV,
                        institution = institution, date = date),

  w.010  = massStandard(nominal = 10, convMassCor = 0.001, uncert = 0.003, units = c('mg', 'mg', 'mg'),
                        serial = serial, class = class, manufacturer = manufacturer,
                        certificate = certificate, traceability = traceability,
                        Temp = Temp, p = p, h = h, unitsENV = unitsENV,
                        institution = institution, date = date),

  w.005  = massStandard(nominal = 5, convMassCor = 0.002, uncert = 0.003, units = c('mg', 'mg', 'mg'),
                        serial = serial, class = class, manufacturer = manufacturer,
                        certificate = certificate, traceability = traceability,
                        Temp = Temp, p = p, h = h, unitsENV = unitsENV,
                        institution = institution, date = date),

  w.002  = massStandard(nominal = 2, convMassCor = 0.001, uncert = 0.003, units = c('mg', 'mg', 'mg'),
                        serial = serial, class = class, manufacturer = manufacturer,
                        certificate = certificate, traceability = traceability,
                        Temp = Temp, p = p, h = h, unitsENV = unitsENV,
                        institution = institution, date = date),

  w.002h = massStandard(nominal = 2, convMassCor = 0.002, uncert = 0.003, units = c('mg', 'mg', 'mg'),
                        serial = serial, class = class, manufacturer = manufacturer,
                        certificate = certificate, traceability = traceability,
                        Temp = Temp, p = p, h = h, unitsENV = unitsENV,
                        institution = institution, date = date),

  w.001  = massStandard(nominal = 1, convMassCor = 0.002, uncert = 0.003, units = c('mg', 'mg', 'mg'),
                        serial = serial, class = class, manufacturer = manufacturer,
                        certificate = certificate, traceability = traceability,
                        Temp = Temp, p = p, h = h, unitsENV = unitsENV,
                        institution = institution, date = date)
)

usethis::use_data(massStandadsKit_E2, overwrite = TRUE)

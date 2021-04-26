massSTD <- c(0.01, 0.5, 1, 10, 20, 50, 100, 120, 150, 200, 220)  ## [g]
indError <- c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -0.2, -0.2) ## [mg]
uncert <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.2, 0.2, 0.3, 0.3, 0.4, 0.5) / 2 ## [mg]
d <- 0.1 ## [mg]

MT.XPE.204 <- calibCert(balanceID = 'MT XPE 204', massSTD = massSTD,
                        indError = indError, uncert = uncert, d = d,
                        units = c('g', 'mg', 'mg', 'mg'),
                        classSTD = 'E2', certSTD = '1473 D-K 17296',
                        p = c(750.4, 751.0), ## [hPa]
                        Temp = c(17.4, 17.9), ## [deg.C]
                        h = c(70.5, 71.4), ## [%]
                        unitsENV = c('hPa', 'deg.C', '%'),
                        institution = 'Instituto Nacional de Metrología de Colombia',
                        date = '2021/03/18')

usethis::use_data(MT.XPE.204, overwrite = TRUE)

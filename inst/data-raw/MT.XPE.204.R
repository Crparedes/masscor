massSTD  <- c(0.01, 0.5, 1, 10, 20, 50, 100, 120, 150, 200, 220)  ## [g]
indError <- c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -0.2, -0.2) ## [mg]
uncert   <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.2, 0.2, 0.3, 0.3, 0.4, 0.5) ## [mg]
d <- 0.1 ## [mg]
traceability <- 'Set of weights class E2. Certificate number 1473 D-K 17296, 2019-05-10.'


MT.XPE.204 <- calibCert(balanceID = 'MT XPE 204', serial = 'B403223982',
                        massSTD = massSTD,
                        indError = indError, uncert = uncert, d = d,
                        units = c('g', 'mg', 'mg', 'mg'),
                        classSTD = 'E2', traceability = traceability,
                        Temp = c(17.4, 17.9), ## [deg.C]
                        p = c(750.4, 751.0), ## [hPa]
                        h = c(70.5, 71.4), ## [%]
                        unitsENV = c('deg.C', 'hPa', '%'),
                        institution = 'Instituto Nacional de Metrologia de Colombia',
                        date = '2021-03-18')

usethis::use_data(MT.XPE.204, overwrite = TRUE)

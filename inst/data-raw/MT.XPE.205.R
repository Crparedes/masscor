massSTD  <- c(0.01, 0.5, 1, 10, 20, 50, 100, 150, 200, 210, 220)  ## [g]
indError <- c(0.00, 0.00, 0.00, -0.01, -0.01, -0.02, -0.01, 0.00, 0.01, 0.03, 0.02) ## [mg]
uncert   <- c(0.04, 0.05, 0.05, 0.08, 0.10, 0.12, 0.20, 0.31, 0.40, 0.45, 0.47) ## [mg]
d <- 0.01 ## [mg]
traceability <- 'Set of weights class E2. Certificate number 1473 D-K 17296, 2019-05-10.'


MT.XPE.205 <- calibCert(balanceID = 'MT XPE 205', serial = 'B743848411',
                        massSTD = massSTD,
                        indError = indError, uncert = uncert, d = d,
                        units = c('g', 'mg', 'mg', 'mg'),
                        classSTD = 'E2', traceability = traceability,
                        Temp = c(19.7, 20.0), ## [deg.C]
                        p = c(751.7, 751.8), ## [hPa]
                        h = c(44.8, 47.3), ## [%]
                        unitsENV = c('deg.C', 'hPa', '%'),
                        institution = 'Instituto Nacional de Metrologia de Colombia',
                        date = '2020-07-14')

usethis::use_data(MT.XPE.205, overwrite = TRUE)

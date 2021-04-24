minimalCert <- calibCert(massSTD = c(5, 20), #[g]
                         indError = c(-0.2, -0.1), #[mg]
                         uncert = c(0.4, 0.41),
                         d = 0.1, #[mg]
                         units = c('g', 'mg', 'mg', 'mg'))

usethis::use_data(minimalCert)

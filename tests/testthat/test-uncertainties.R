test_that("AndresDiapositiva110", {
  data(minimalCert)
  expect_equal(round(uncertErrorCorr(reading = 12.4835, calibCert = minimalCert), 9),
               0.0005728/2)
  # Hablar con Andr'es de por qu'e en la diapositiva est'a la incertidumbre expandida
})

test_that("AndresDiapositiva111", {
  data(minimalCert)
  expect_equal(uncertReading(calibCert = minimalCert)^2, 1.66667e-9)
})

test_that("AndresDiapositiva112", {
  data(minimalCert)
  expect_equal(round(uncertMassConv(reading = 12.4835, calibCert = minimalCert), 10), 0.0004091251) #Preguntar a Andres porque el n'umero dos
})

# usethis::use_test("")

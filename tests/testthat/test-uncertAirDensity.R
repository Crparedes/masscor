test_that("uncertAirDensity works", {
  airDensity(opt = 'CIMP.approx')
  uncertAirDensity(opt = 'CIMP.approx')
  uncertAirDensity(opt = 'CIMP.approx', u_Temp = 0.29, u_p = 1.01, u_h = 11.3)
  uncertAirDensity(opt = 'CIMP.approx', u_Temp = 0.1, u_p = 0.665, u_h = 10)

  airDensity(opt = 'CIMP2007')
  uncertAirDensity(opt = 'CIMP2007')
  uncertAirDensity(opt = 'CIMP2007', u_Temp = 0.29, u_p = 1.01, u_h = 11.3)
  uncertAirDensity(opt = 'CIMP2007', u_Temp = 0.1, u_p = 0.665, u_h = 10)
})

#' according to https://www.bipm.org/en/measurement-units/si-prefixes

convertMassUnitsSI <- function(from, to, value) {
  prefixes <- list(Y =	1e24, Z =	1e21, E =	1e18, P =	1e15, T =	1e12, G =	1e9,
                   M =	1e6, k = 1e3, h =	1e2, da = 1e1, d = 1e-1, c = 1e-2,
                   m =	1e-3, mu = 1e-6, n =	1e-9, p =	1e-12, f =	1e-15,
                   a =	1e-18, z =	1e-21, y =	1e-24)

  if(grepl(from, to, fixed = TRUE)) {
    i <- mapply(regexpr, from, to) - 1
    un <- substr(to, i, i)
    cVal <- value / prefixes[[un]]
    return(cVal)
  } else {
    if(grepl(to, from, fixed = TRUE)) {
      i <- mapply(regexpr, to, from) - 1
      un <- substr(from, i, i)
      cVal <- value * prefixes[[un]]
      return(cVal)
    } else {
      un <- mf(c(from, to))
      cVal <- value * prefixes[[un[1]]] / prefixes[[un[2]]]
      return(cVal)
      #return(c(prefixes[[un[1]]], prefixes[[un[2]]]))
    }
  }
}

convertMassUnitsSI(from = 'kg', to = 'mg', value = 5.8)
convertMassUnitsSI(from = 'kg', to = 'g', value = 5.8)
convertMassUnitsSI(from = 'g', to = 'kg', value = 5.8)

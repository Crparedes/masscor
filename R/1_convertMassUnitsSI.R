#' Conversion between mass units of The International System of Units
#'
#' Mass values are converted from a SI unit to another SI unit according to the SI prefixes as
#' shown in BIMP (2019). The greek letter \eqn{\mu} is replaced by the vocal \code{u}.
#'
#' This function is mostly to be used internally by \code{masscor} package functions.
#'
#' @param value Numeric vector with the values to be converted.
#' @param from Character with the source unit.
#' @param to Character with the desired unit.
#'
#' @references
#' BIMP, 2019. Bureau International des Poids et Mesures. Brochure of The International System of Units. 9th Edition.
#'
#' @return Converted values to desired mass unit.
#' @examples
#' convertMassUnitsSI(value = c(0.2, 0.4), from = 'mg', to = 'g')
#'
#' @export

convertMassUnitsSI <- function(value, from, to) {
  prefixes <- list(Y =	1e24, Z =	1e21, E =	1e18, P =	1e15, T =	1e12, G =	1e9,
                   M =	1e6, k = 1e3, h =	1e2, da = 1e1, d = 1e-1, c = 1e-2,
                   m =	1e-3, u = 1e-6, n =	1e-9, p =	1e-12, f =	1e-15,
                   a =	1e-18, z =	1e-21, y =	1e-24)

  if(grepl(from, to, fixed = TRUE)) {
    i <- mapply(regexpr, from, to) - 1
    un <- substr(to, i, i)
    cVal <- value / prefixes[[un]]
  } else {
    if(grepl(to, from, fixed = TRUE)) {
      i <- mapply(regexpr, to, from) - 1
      un <- substr(from, i, i)
      cVal <- value * prefixes[[un]]
    } else {
      un <- mf(c(from, to))
      cVal <- value * prefixes[[un[1]]] / prefixes[[un[2]]]
      #return(c(prefixes[[un[1]]], prefixes[[un[2]]]))
    }
  }
  return(as.numeric(cVal))
}

#' S3 method for printing objects of class \code{"massStandard"}
#'
#' The function prints objects of class \code{"massStandard"} in a certificate-like fashion.
#' @param x Object of class \code{"massStandard"}.
#' @param ... Further arguments passed to or from other methods.
#' @examples
#' data(MT.XPE.204)
#' print(MT.XPE.204)
#' @export
print.massStandard <- function(x, ...) {
  class(x) <- "list"
  print(x, ...)
}

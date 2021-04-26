#' S3 method for printing objects of class \code{"calibCert"}
#'
#' The function prints objects of class \code{"calibCert"} in a certificate-like fashion.
#' @inheritParams plot.calibCert
#' @param plot Logical. If \code{TRUE} the calibration info is plotted by internal
#'   call to [plot.calibCert()].
#' @param ... Further arguments passed to or from other methods.
#' @seealso [calibCert()], [plot.calibCert()]
#' @examples
#' data(MT.XPE.204)
#' print(MT.XPE.204)
#' @export
print.calibCert <- function(x, plot = FALSE, ...) {
  if(plot) plot(x)
  class(x) <- "list"
  print(x, ...)
}

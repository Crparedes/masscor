#' S3 method for printing objects of class \code{"calibCert"}
#'
#' The function prints objects of class \code{"calibCert"} in a certificate-like fashion.
#' @inheritParams print.massStandard
#' @param plot Logical. If \code{TRUE} the calibration info is plotted by internal
#'   call to [plot.calibCert()].
#' @param description Logical. If \code{TRUE} (the default) details of balance serial and manufacturer
#'   are printed. Ignored if \code{minimal = TRUE}.
#' @param ... Further arguments passed to or from other methods.
#' @seealso [calibCert()], [plot.calibCert()]
#' @examples
#' data(MT.XPE.204)
#' print(MT.XPE.204)
#' @export
print.calibCert <- function(x, plot = FALSE, minimal = FALSE, description = TRUE,
                            institution = TRUE, envConditions = TRUE, addInfo = TRUE, ...) {
  cat('BALANCE CALIBRATION DATA:', x$balanceID, '\n\n')


  if(plot) plot(x)
  class(x) <- "list"
  print(x, ...)
}

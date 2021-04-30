#' S3 method for plotting objects of class \code{"calibCert"}
#'
#' The function plots the indication error or the conventional mass correction for a balance whose
#' calibration data is in a object of class \code{"calibCert"}.
#' @param x Object of class \code{"calibCert"}.
#' @param error Logical. If \code{TRUE} (the default), the indication error is plotted.
#'    If \code{FALSE} the conventional mass correction is plotted instead.
#' @param ylim Numeric vector of length 2 with the limits for y axis. Default to \code{c(-1, 1)}.
#' @param y0line Logical. If \code{TRUE} (the default) a horizontal line is drawn at y = 0.
#' @param  ... Other graphical parameters used in \code{\link[graphics]{plot}}
#'
#' @return A base plot with calibration data.
#' @examples
#' data(MT.XPE.204)
#' plot(MT.XPE.204)
#'
#' @seealso [calibCert()], [print.calibCert()]
#' @export
#' @importFrom graphics arrows abline
plot.calibCert <- function(x, error = TRUE, ylim = c(-1, 1), y0line = TRUE, ...) {
  mass <- x$massSTD
  indError <- convertMassUnitsSI(value = x$indError, from = x$standardUnits, to = x$originalUnits[2])
  MCcorr <- convertMassUnitsSI(value = x$MCcorr, from = x$standardUnits, to = x$originalUnits[2])
  Uncert <- convertMassUnitsSI(value = x$expandUncert, from = x$standardUnits, to = x$originalUnits[3])

  if(!error) {
    plot(x = mass, y = MCcorr,
         xlab = paste0('Load /[', x$originalUnits[1], ']'),
         ylab = paste0('Conventional mass correction /[', x$originalUnits[2], ']'),
         pch = 18, ylim = ylim, ...)
    arrows(x0 = mass, y0 = MCcorr - Uncert, x1 = mass, y1 = MCcorr + Uncert, code = 3,
           angle = 90, length = 0.1, col = 'steelblue')
  } else  {
    plot(x = mass, y = indError,
         xlab = paste0('Load /[', x$originalUnits[1], ']'),
         ylab = paste0('Indication error /[', x$originalUnits[2], ']'),
         pch = 18, ylim = ylim, ...)
    arrows(x0 = mass, y0 = indError - Uncert, x1 = mass, y1 = indError + Uncert, code = 3,
           angle = 90, length = 0.1, col = 'steelblue')
  }
  if (y0line) abline(h = 0)
}

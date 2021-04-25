#' S3 method for printing objects of class calibCert
#' @examples
#'
#' @export
print.calibCert <- function(x) {
  class(x) <- "list"
  print(x)
}

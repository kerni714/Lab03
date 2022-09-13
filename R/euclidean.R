#' Euclidean
#'
#' @param a - number (integer)
#' @param b - number (integer)
#'
#' @return greatest common divisor
#' @export
#'
#' @examples
#' euclidean(123612, 13892347912)
euclidean <- function (a,b) {
  stopifnot(is.numeric(a) & round(a)==a & length(a)==1,
            is.numeric(b) & round(b)==b & length(b)==1)

  while (b != 0) {
    t = b
    b = a %% b
    a = t
  }
  return(a)
}

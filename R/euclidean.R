#' Euclidean algorithm
#'
#' The euclidean algorithm finds the greatest common divisor of two integers.
#' See \url{https://en.wikipedia.org/wiki/Euclidean_algorithm} for more information.
#' @param a - number (integer)
#' @param b - number (integer)
#'
#' @return greatest common divisor
#' @export
#'
#' @examples
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)
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

#' myclt function
#'
#' @param n sample sizes
#' @param iter amount of iterations in the distribution
#' @param a lower limit of the distribution
#' @param b upper limit of the distribution
#'
#' @return A histogram with a normal density curve overlay and a list containing the sample means
#'
#' @importFrom stats runif
#' @importFrom graphics hist
#'
#' @examples
#' myclt(50, 10000, a = 0, b = 5)
#'
#' @export
myclt = function(n, iter, a = 0, b = 5){
  y = runif(n * iter, a, b)
  data = matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  sm = apply(data, 2, mean)
  hist(sm)
  invisible(sm)
}

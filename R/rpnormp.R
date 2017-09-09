#' @name PowerPowerExponential
#' @examples
#' rpnormp(5, 2, 3, 4, 1)
#' @export
rpnormp = function(n, lambda = 1, mu= 0, sigma = 1, k = 0){
  n = runif(n)
  x = qpnormp(n, lambda, mu, sigma, k)
  return(x)
}

#' @name ReversalPowerPowerExponential
#' @examples
#' rrpnormp(5, 2, 3, 4, 1)
#' @export
rrpnormp = function(n, lambda = 1, mu= 0, sigma = 1, k = 0){
  n = runif(n)
  x = qrpnormp(n, lambda, mu, sigma, k)
  return(x)
}

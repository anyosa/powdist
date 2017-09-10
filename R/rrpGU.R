#' @name ReversalPowerGumbel
#' @examples
#' rrpGU(5, 2, 3, 4)
#' @export
rrpGU = function(n, lambda = 1, mu= 0, sigma = 1){
  n = runif(n)
  x = qrpGU(n, lambda, mu, sigma)
  return(x)
}

#' @name ReversalPowerGumbel
#' @examples
#' rrpgumbel(5, 2, 3, 4)
#' @export
rrpgumbel = function(n, lambda = 1, mu= 0, sigma = 1){
  n = runif(n)
  x = qrpgumbel(n, lambda, mu, sigma)
  return(x)
}

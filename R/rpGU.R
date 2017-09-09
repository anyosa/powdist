#' @name PowerGumbel2
#' @examples
#' rpGU(5, 2, 3, 4)
#' @export
rpGU = function(n, lambda = 1, mu= 0, sigma = 1){
  n = runif(n)
  x = qpGU(n, lambda, mu, sigma)
  return(x)
}

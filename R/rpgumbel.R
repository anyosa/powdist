#' @name PowerGumbel
#' @examples
#' rpgumbel(5, 2, 3, 4)
#' @export
rpgumbel = function(n, lambda = 1, mu= 0, sigma = 1){
  n = runif(n)
  x = qpgumbel(n, lambda, mu, sigma)
  return(x)
}

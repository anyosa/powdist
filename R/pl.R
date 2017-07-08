#' Density, distribution function, quantile function and random generation for the power logistic distribution with parameters mu and sigma.
#'
#' @param x vector of quantiles.
#' @param q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations. If length(n) > 1, the length is taken to be the number required.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) The New S Language. Wadsworth & Brooks/Cole.
#' @examples
#' dpl(1, 1, 3, 4)
dpl <- function(x, lambda, mu = 0, sigma = 1){
  d = (lambda/sigma) * dlogis((x-mu)/sigma) * (plogis((x-mu)/sigma)**(lambda-1))
  return(d)
}

ppl <- function(q, lambda, mu = 0, sigma = 1){
  p = plogis(q, mu, sigma)
  return(p**lambda)
}

qpl <- function(p, lambda,  mu = 0, sigma = 1){
  q = qlogis(p**(1/lambda))* sigma + mu
  return(q)
}

rpl = function(n, lambda, mu= 0, sigma = 1){
  n = runif(n)
  x = qpl(n, lambda, mu, sigma)
  return(x)
}

#' Reversal power cauchy distribution
#'
#' Density function for the reversal power cauchy distribution with parameters mu, sigma and lambda.
#' @param x vector of quantiles.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Bazán, J. L., Torres‐Avilés, F., Suzuki, A. K., & Louzada, F. (2017). Power and reversal power links for binary regressions: An application for motor insurance policyholders. Applied Stochastic Models in Business and Industry, 33(1), 22-34.
#' @examples
#' drpc(1, 1, 3, 4)
#' @export
drpc <- function(x, lambda, mu = 0, sigma = 1){
  d = (lambda/sigma) * dcauchy((x-mu)/sigma) * (pcauchy((-x-mu)/sigma)**(lambda-1))
  return(d)
}

#' Reversal power cauchy distribution
#'
#' Distribution function for the reversal power cauchy distribution with parameters mu, sigma and lambda.
#' @param q vector of quantiles.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Bazán, J. L., Torres‐Avilés, F., Suzuki, A. K., & Louzada, F. (2017). Power and reversal power links for binary regressions: An application for motor insurance policyholders. Applied Stochastic Models in Business and Industry, 33(1), 22-34.
#' @examples
#' prpc(1, 1, 3, 4)
#' @export
prpc <- function(q, lambda, mu = 0, sigma = 1){
  p = pcauchy(-q, mu, sigma)
  return(1-p**lambda)
}

#' Reversal power cauchy distribution
#'
#' Quantile function for the reversal power cauchy distribution with parameters mu, sigma and lambda.
#' @param p vector of probabilities.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Bazán, J. L., Torres‐Avilés, F., Suzuki, A. K., & Louzada, F. (2017). Power and reversal power links for binary regressions: An application for motor insurance policyholders. Applied Stochastic Models in Business and Industry, 33(1), 22-34.
#' @examples
#' qrpc(0.2, 1, 3, 4)
#' @export
qrpc <- function(p, lambda, mu = 0, sigma = 1){
  q = -qcauchy((1-p)**(1/lambda))* sigma + mu
  return(q)
}

#' Reversal power cauchy distribution
#'
#' Random generation for the reversal power cauchy distribution with parameters mu, sigma and lambda.
#' @param n number of observations. If length(n) > 1, the length is taken to be the number required.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Bazán, J. L., Torres‐Avilés, F., Suzuki, A. K., & Louzada, F. (2017). Power and reversal power links for binary regressions: An application for motor insurance policyholders. Applied Stochastic Models in Business and Industry, 33(1), 22-34.
#' @examples
#' rrpc(5, 2, 3, 4)
#' @export
rrpc = function(n, lambda, mu= 0, sigma = 1){
  n = runif(n)
  x = qrpc(n, lambda, mu, sigma)
  return(x)
}

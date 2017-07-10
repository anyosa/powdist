#' Reversal power normal distribution
#'
#' Density function for the reversal power normal distribution with parameters mu, sigma and lambda.
#' @param x vector of quantiles.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Bazán, J. L., Torres‐Avilés, F., Suzuki, A. K., & Louzada, F. (2017). Power and reversal power links for binary regressions: An application for motor insurance policyholders. Applied Stochastic Models in Business and Industry, 33(1), 22-34.
#' @examples
#' drpn(1, 1, 3, 4)
#' @export
drpn <- function(x, lambda, mu = 0, sigma = 1){
  d = (lambda/sigma) * dnorm((x-mu)/sigma) * (pnorm((-x-mu)/sigma)**(lambda-1))
  return(d)
}

#' Reversal power normal distribution
#'
#' Distribution function for the reversal power normal distribution with parameters mu, sigma and lambda.
#' @param q vector of quantiles.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Bazán, J. L., Torres‐Avilés, F., Suzuki, A. K., & Louzada, F. (2017). Power and reversal power links for binary regressions: An application for motor insurance policyholders. Applied Stochastic Models in Business and Industry, 33(1), 22-34.
#' @examples
#' prpn(1, 1, 3, 4)
#' @export
prpn <- function(q, lambda, mu = 0, sigma = 1){
  p = pnorm(-q, mu, sigma)
  return(1-p**lambda)
}

#' Reversal power normal distribution
#'
#' Quantile function for the reversal power normal distribution with parameters mu, sigma and lambda.
#' @param p vector of probabilities.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Bazán, J. L., Torres‐Avilés, F., Suzuki, A. K., & Louzada, F. (2017). Power and reversal power links for binary regressions: An application for motor insurance policyholders. Applied Stochastic Models in Business and Industry, 33(1), 22-34.
#' @examples
#' qrpn(0.2, 1, 3, 4)
#' @export
qrpn <- function(p, lambda, mu = 0, sigma = 1){
  q = -qnorm((1-p)**(1/lambda))* sigma + mu
  return(q)
}

#' Reversal power normal distribution
#'
#' Random generation for the reversal power normal distribution with parameters mu, sigma and lambda.
#' @param n number of observations. If length(n) > 1, the length is taken to be the number required.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Bazán, J. L., Torres‐Avilés, F., Suzuki, A. K., & Louzada, F. (2017). Power and reversal power links for binary regressions: An application for motor insurance policyholders. Applied Stochastic Models in Business and Industry, 33(1), 22-34.
#' @examples
#' rrpn(5, 2, 3, 4)
#' @export
rrpn = function(n, lambda, mu= 0, sigma = 1){
  n = runif(n)
  x = qrpn(n, lambda, mu, sigma)
  return(x)
}

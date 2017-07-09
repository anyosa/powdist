#' Reversal power gumbel of maximum value distribution
#'
#' Density function for the reversal power gumbel of maximum value distribution with parameters mu, sigma and lambda.
#' @param x vector of quantiles.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Bazán, J. L., Torres‐Avilés, F., Suzuki, A. K., & Louzada, F. (2017). Power and reversal power links for binary regressions: An application for motor insurance policyholders. Applied Stochastic Models in Business and Industry, 33(1), 22-34.
#' @examples
#' drpgmav(1, 1, 3, 4)
drpgmav <- function(x, lambda, mu = 0, sigma = 1){
  d = (lambda/sigma) * dgumbel((x-mu)/sigma) * ( pgumbel((-x-mu)/sigma) **(lambda-1))
  return(d)
}

#' Reversal power gumbel of maximum value distribution
#'
#' Distribution function for the reversal power gumbel of maximum value distribution with parameters mu, sigma and lambda.
#' @param q vector of quantiles.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Bazán, J. L., Torres‐Avilés, F., Suzuki, A. K., & Louzada, F. (2017). Power and reversal power links for binary regressions: An application for motor insurance policyholders. Applied Stochastic Models in Business and Industry, 33(1), 22-34.
#' @examples
#' prpgmav(1, 1, 3, 4)
prpgmav <- function(q, lambda, mu = 0, sigma = 1){
  p = pgumbel(-q, mu, sigma)
  return(1-p**lambda)
}

#' Reversal power gumbel of maximum value distribution
#'
#' Quantile function for the reversal power gumbel of maximum value distribution with parameters mu, sigma and lambda.
#' @param p vector of probabilities.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Bazán, J. L., Torres‐Avilés, F., Suzuki, A. K., & Louzada, F. (2017). Power and reversal power links for binary regressions: An application for motor insurance policyholders. Applied Stochastic Models in Business and Industry, 33(1), 22-34.
#' @examples
#' qrpgmav(0.2, 1, 3, 4)
qrpgmav <- function(p, lambda, mu = 0, sigma = 1){
  q = -qgumbel((1-p)**(1/lambda))* sigma + mu
  return(q)
}

#' Reversal power gumbel of maximum value distribution
#'
#' Random generation for the reversal power gumbel of maximum value distribution with parameters mu, sigma and lambda.
#' @param n number of observations. If length(n) > 1, the length is taken to be the number required.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Bazán, J. L., Torres‐Avilés, F., Suzuki, A. K., & Louzada, F. (2017). Power and reversal power links for binary regressions: An application for motor insurance policyholders. Applied Stochastic Models in Business and Industry, 33(1), 22-34.
#' @examples
#' rrpgmav(5, 2, 3, 4)
rrpgmav = function(n, lambda, mu= 0, sigma = 1){
  n = runif(n)
  x = qrpgmav(n, lambda, mu, sigma)
  return(x)
}

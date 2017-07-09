#' Reversal power gumbel of minimum value distribution
#'
#' Density function for the reversal power gumbel of minimum value distribution with parameters mu, sigma and lambda.
#' @param x vector of quantiles.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Bazán, J. L., Torres‐Avilés, F., Suzuki, A. K., & Louzada, F. (2017). Power and reversal power links for binary regressions: An application for motor insurance policyholders. Applied Stochastic Models in Business and Industry, 33(1), 22-34.
#' @examples
#' drpgmiv(1, 1, 3, 4)
drpgmiv <- function(x, lambda, mu = 0, sigma = 1){
  d = (lambda/sigma) * dgumbel((-x-mu)/sigma) * ( (1-pgumbel((x-mu)/sigma)) **(lambda-1))
  return(d)
}

#' Reversal power gumbel of minimum value distribution
#'
#' Distribution function for the reversal power gumbel of minimum value distribution with parameters mu, sigma and lambda.
#' @param q vector of quantiles.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Bazán, J. L., Torres‐Avilés, F., Suzuki, A. K., & Louzada, F. (2017). Power and reversal power links for binary regressions: An application for motor insurance policyholders. Applied Stochastic Models in Business and Industry, 33(1), 22-34.
#' @examples
#' prpgmiv(1, 1, 3, 4)
prpgmiv <- function(q, lambda, mu = 0, sigma = 1){
  p = 1-pgumbel(q, mu, sigma)
  return(1-p**lambda)
}

#' Reversal power gumbel of minimum value distribution
#'
#' Quantile function for the reversal power gumbel of minimum value distribution with parameters mu, sigma and lambda.
#' @param p vector of probabilities.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Bazán, J. L., Torres‐Avilés, F., Suzuki, A. K., & Louzada, F. (2017). Power and reversal power links for binary regressions: An application for motor insurance policyholders. Applied Stochastic Models in Business and Industry, 33(1), 22-34.
#' @examples
#' qrpgmiv(0.2, 1, 3, 4)
qrpgmiv <- function(p, lambda, mu = 0, sigma = 1){
  q = qgumbel(1-((1-p)**(1/lambda)))* sigma + mu
  return(q)
}


#' Reversal power gumbel of minimum value distribution
#'
#' Random generation for the reversal power gumbel of minimum value distribution with parameters mu, sigma and lambda.
#' @param n number of observations. If length(n) > 1, the length is taken to be the number required.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Bazán, J. L., Torres‐Avilés, F., Suzuki, A. K., & Louzada, F. (2017). Power and reversal power links for binary regressions: An application for motor insurance policyholders. Applied Stochastic Models in Business and Industry, 33(1), 22-34.
#' @examples
#' rrpgmiv(5, 2, 3, 4)
rrpgmiv = function(n, lambda, mu= 0, sigma = 1){
  n = runif(n)
  x = qrpgmiv(n, lambda, mu, sigma)
  return(x)
}

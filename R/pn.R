#' Power normal distribution
#'
#' Density function for the power normal distribution with parameters mu, sigma and lambda.
#' @param x vector of quantiles.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Bazán, J. L., Torres‐Avilés, F., Suzuki, A. K., & Louzada, F. (2017). Power and reversal power links for binary regressions: An application for motor insurance policyholders. Applied Stochastic Models in Business and Industry, 33(1), 22-34.
#' @examples
#' dpn(1, 1, 3, 4)
dpn <- function(x, lambda, mu = 0, sigma = 1){
  d = (lambda/sigma) * dnorm((x-mu)/sigma) * (pnorm((x-mu)/sigma)**(lambda-1))
  return(d)
}

#' Power normal distribution
#'
#' Distribution function for the power normal distribution with parameters mu, sigma and lambda.
#' @param q vector of quantiles.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Bazán, J. L., Torres‐Avilés, F., Suzuki, A. K., & Louzada, F. (2017). Power and reversal power links for binary regressions: An application for motor insurance policyholders. Applied Stochastic Models in Business and Industry, 33(1), 22-34.
#' @examples
#' ppn(1, 1, 3, 4)
ppn <- function(q, lambda, mu = 0, sigma = 1){
  p = pnorm(q, mu, sigma)
  return(p**lambda)
}

#' Power normal distribution
#'
#' Quantile function for the power normal distribution with parameters mu, sigma and lambda.
#' @param p vector of probabilities.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Bazán, J. L., Torres‐Avilés, F., Suzuki, A. K., & Louzada, F. (2017). Power and reversal power links for binary regressions: An application for motor insurance policyholders. Applied Stochastic Models in Business and Industry, 33(1), 22-34.
#' @examples
#' qpn(0.2, 1, 3, 4)
qpn <- function(p, lambda, mu = 0, sigma = 1){
  q = qnorm(p**(1/lambda))* sigma + mu
  return(q)
}

#' Power normal distribution
#'
#' Random generation for the power normal distribution with parameters mu, sigma and lambda.
#' @param n number of observations. If length(n) > 1, the length is taken to be the number required.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Bazán, J. L., Torres‐Avilés, F., Suzuki, A. K., & Louzada, F. (2017). Power and reversal power links for binary regressions: An application for motor insurance policyholders. Applied Stochastic Models in Business and Industry, 33(1), 22-34.
#' @examples
#' rpl(5, 2, 3, 4)
rpn = function(n, lambda, mu= 0, sigma = 1){
  n = runif(n)
  x = qpn(n, lambda, mu, sigma)
  return(x)
}

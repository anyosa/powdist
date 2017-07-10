#' Power cauchy distribution
#'
#' Density function for the power cauchy distribution with parameters mu, sigma and lambda.
#' @param x vector of quantiles.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Bazán, J. L., Torres‐Avilés, F., Suzuki, A. K., & Louzada, F. (2017). Power and reversal power links for binary regressions: An application for motor insurance policyholders. Applied Stochastic Models in Business and Industry, 33(1), 22-34.
#' @examples
#' dpc(1, 1, 3, 4)
#' @export
dpc <- function(x, lambda, mu = 0, sigma = 1){
  d = (lambda/sigma) * dcauchy((x-mu)/sigma) * (pcauchy((x-mu)/sigma)**(lambda-1))
  return(d)
}

#' Power cauchy distribution
#'
#' Distribution function for the power cauchy distribution with parameters mu, sigma and lambda.
#' @param q vector of quantiles.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Bazán, J. L., Torres‐Avilés, F., Suzuki, A. K., & Louzada, F. (2017). Power and reversal power links for binary regressions: An application for motor insurance policyholders. Applied Stochastic Models in Business and Industry, 33(1), 22-34.
#' @examples
#' ppc(1, 1, 3, 4)
#' @export
ppc <- function(q, lambda, mu = 0, sigma = 1){
  p = pcauchy(q, mu, sigma)
  return(p**lambda)
}

#' Power cauchy distribution
#'
#' Quantile function for the power cauchy distribution with parameters mu, sigma and lambda.
#' @param p vector of probabilities.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Bazán, J. L., Torres‐Avilés, F., Suzuki, A. K., & Louzada, F. (2017). Power and reversal power links for binary regressions: An application for motor insurance policyholders. Applied Stochastic Models in Business and Industry, 33(1), 22-34.
#' @examples
#' qpc(0.2, 1, 3, 4)
#' @export
qpc <- function(p, lambda, mu = 0, sigma = 1){
  q = qcauchy(p**(1/lambda))* sigma + mu
  return(q)
}

#' Power cauchy distribution
#'
#' Random generation for the power cauchy distribution with parameters mu, sigma and lambda.
#' @param n number of observations. If length(n) > 1, the length is taken to be the number required.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Bazán, J. L., Torres‐Avilés, F., Suzuki, A. K., & Louzada, F. (2017). Power and reversal power links for binary regressions: An application for motor insurance policyholders. Applied Stochastic Models in Business and Industry, 33(1), 22-34.
#' @examples
#' rpc(5, 2, 3, 4)
#' @export
rpc = function(n, lambda, mu= 0, sigma = 1){
  n = runif(n)
  x = qpc(n, lambda, mu, sigma)
  return(x)
}

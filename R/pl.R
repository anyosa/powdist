#' Power logistic distribution
#'
#' Density function for the power logistic distribution with parameters mu, sigma and lambda.
#'
#' @export
#'
#' @param x vector of quantiles.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Bazán, J. L., Torres‐Avilés, F., Suzuki, A. K., & Louzada, F. (2017). Power and reversal power links for binary regressions: An application for motor insurance policyholders. Applied Stochastic Models in Business and Industry, 33(1), 22-34.
#' @examples
#' dpl(1, 1, 3, 4)
dpl <- function(x, lambda, mu = 0, sigma = 1){
  d = (lambda/sigma) * dlogis((x-mu)/sigma) * (plogis((x-mu)/sigma)**(lambda-1))
  return(d)
}

#' Power logistic distribution
#'
#' @export
#'
#' Distribution function for the power logistic distribution with parameters mu, sigma and lambda.
#' @param q vector of quantiles.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Bazán, J. L., Torres‐Avilés, F., Suzuki, A. K., & Louzada, F. (2017). Power and reversal power links for binary regressions: An application for motor insurance policyholders. Applied Stochastic Models in Business and Industry, 33(1), 22-34.
#' @examples
#' ppl(1, 1, 3, 4)
ppl <- function(q, lambda, mu = 0, sigma = 1){
  p = plogis(q, mu, sigma)
  return(p**lambda)
}

#' Power logistic distribution
#'
#' Quantile function for the power logistic distribution with parameters mu, sigma and lambda.
#' @param p vector of probabilities.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Bazán, J. L., Torres‐Avilés, F., Suzuki, A. K., & Louzada, F. (2017). Power and reversal power links for binary regressions: An application for motor insurance policyholders. Applied Stochastic Models in Business and Industry, 33(1), 22-34.
#' @examples
#' qpl(0.2, 1, 3, 4)
qpl <- function(p, lambda,  mu = 0, sigma = 1){
  q = qlogis(p**(1/lambda))* sigma + mu
  return(q)
}

#' Power logistic distribution
#'
#' Random generation for the power logistic distribution with parameters mu, sigma and lambda.
#' @param n number of observations. If length(n) > 1, the length is taken to be the number required.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Bazán, J. L., Torres‐Avilés, F., Suzuki, A. K., & Louzada, F. (2017). Power and reversal power links for binary regressions: An application for motor insurance policyholders. Applied Stochastic Models in Business and Industry, 33(1), 22-34.
#' @examples
#' rpl(5, 2, 3, 4)
rpl = function(n, lambda, mu= 0, sigma = 1){
  n = runif(n)
  x = qpl(n, lambda, mu, sigma)
  return(x)
}

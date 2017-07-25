#' @title The Reversal Power Cauchy Distribution
#' @name revpc
#' @description Density, distribution function,
#' quantile function and random generation for
#' the reversal power Cauchy distribution with parameters mu, sigma and lambda.
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations.
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


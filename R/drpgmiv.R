#' @title The Reversal Power Gumbel of minimum value Distribution
#' @name revpgmiv
#' @description Density, distribution function,
#' quantile function and random generation for
#' the reversal power Gumbel of minimum value distribution with parameters mu, sigma and lambda.
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Bazán, J. L., Torres‐Avilés, F., Suzuki, A. K., & Louzada, F. (2017). Power and reversal power links for binary regressions: An application for motor insurance policyholders. \emph{Applied Stochastic Models in Business and Industry}, \strong{33}(1), 22-34.
#' @references Anyosa, S. A. C. (2017). \emph{Binary regression using power and reversal power links}. (Master's thesis).
#' @import VGAM
#' @examples
#' drpgmiv(1, 1, 3, 4)
#' @export
drpgmiv <- function(x, lambda, mu = 0, sigma = 1){
  d = (lambda/sigma) * dgumbel((-x-mu)/sigma) * ( (1-pgumbel((x-mu)/sigma)) **(lambda-1))
  return(d)
}


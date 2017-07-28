#' @title The Power Normal Distribution
#' @name pn
#' @description Density, distribution function,
#' quantile function and random generation for
#' the power normal distribution with parameters mu, sigma and lambda.
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Bazán, J. L., Torres‐Avilés, F., Suzuki, A. K., & Louzada, F. (2017). Power and reversal power links for binary regressions: An application for motor insurance policyholders. \emph{Applied Stochastic Models in Business and Industry}, \strong{33}(1), 22-34.
#' @references Anyosa, S. A. C. (2017). \emph{Binary regression using power and reversal power links}. (Master's thesis).
#' @examples
#' dpn(1, 1, 3, 4)
#' @export
dpn <- function(x, lambda, mu = 0, sigma = 1){
  d = (lambda/sigma) * dnorm((x-mu)/sigma) * (pnorm((x-mu)/sigma)**(lambda-1))
  return(d)
}


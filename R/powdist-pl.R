#' Power and reversal power distributions
#'
#' @docType package
#' @name powdist-package
#' @aliases ppl
#' @aliases dpl
#'
#' @description
#'
#' Density, distribution function, quantile function and random generation
#' for the normal distribution with mean equal to mean and standard deviation
#' equal to sd.
#' @usage dnorm(x, mean = 0, sd = 1, log = FALSE)
#' @usage pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
#' @usage qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
#' @usage rnorm(n, mean = 0, sd = 1)
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @references Bazán, J. L., Torres‐Avilés, F., Suzuki, A. K., & Louzada, F. (2017). Power and reversal power links for binary regressions: An application for motor insurance policyholders. Applied Stochastic Models in Business and Industry, 33(1), 22-34.
#' @examples
#' dpl(1, 1, 3, 4)
NULL

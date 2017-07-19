#' Power logistic distribution
#'
#' @docType package
#' @name powdist-pl
#' @aliases dpl
#' @aliases ppl
#' @aliases qpl
#' @aliases rpl
#'
#' @description
#'
#' Density, distribution function, quantile function and random generation
#' for the power logistic distribution with parameters mu, sigma and lambda.
#'
#' @usage dpl(x, lambda, mu = 0, sigma = 1)
#' ppl(q, lambda, mu = 0, sigma = 1)
#' @usage qpl(p, lambda,  mu = 0, sigma = 1)
#' @usage rpl(n, lambda, mu= 0, sigma = 1)
#'
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations. If length(n) > 1, the length is taken to be the number required.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#'
#' @references Bazán, J. L., Torres‐Avilés, F., Suzuki, A. K., & Louzada, F. (2017). Power and reversal power links for binary regressions: An application for motor insurance policyholders. Applied Stochastic Models in Business and Industry, 33(1), 22-34.
#'
#' @examples
#' dpl(1, 1, 3, 4)
#' ppl(1, 1, 3, 4)
#' qpl(0.2, 1, 3, 4)
#' rpl(5, 2, 3, 4)
NULL

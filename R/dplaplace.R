#' @title The Power Laplace Distribution
#' @name PowerLaplace
#' @description Density, distribution function,
#' quantile function and random generation for
#' the power Laplace distribution with parameters mu, sigma and lambda.
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations.
#' @param lambda shape parameter.
#' @param mu,sigma location and scale parameters.
#' @param log,log.p logical; if TRUE, probabilities p are given as log(p).
#' @param lower.tail logical; if TRUE (default), probabilities are \eqn{P[X \le x ]}, otherwise, P[X > x].
#' @importFrom stats runif
#' @importFrom rmutil dlaplace
#' @importFrom rmutil plaplace
#' @importFrom rmutil qlaplace
#' @details The power Laplace distribution has density
#'
#' \eqn{f(x)=[\lambda/\sigma][f((x-\mu)/\sigma)][F((x-\mu)/\sigma)] ^(\lambda-1)},
#'
#' where \eqn{-\infty<\mu<\infty} is the location paramether, \eqn{\sigma^2>0} the scale parameter and \eqn{\lambda>0} the shape parameter.
#'
#' @examples
#' dplaplace(1, 1, 3, 4)
#' @export
dplaplace <- function(x, lambda = 1, mu = 0, sigma = 1, log = FALSE){
  d = (lambda/sigma) * dlaplace((x-mu)/sigma) * (plaplace((x-mu)/sigma)**(lambda-1))
  if (log == TRUE) {
    d = log( (lambda/sigma) * dlaplace((x-mu)/sigma) * (plaplace((x-mu)/sigma)**(lambda-1)) )
  }
return(d)
}


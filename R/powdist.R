#' Applied Regression
#'
#' @docType package
#' @name powdist-package
#' @aliases powdist
#'
#' @description
#'
#' The \pkg{rstanarm} package is an appendage to the \pkg{rstan} package that
#' enables many of the most common applied regression models to be estimated
#' using Markov Chain Monte Carlo, variational approximations to the posterior
#' distribution, or optimization. The \pkg{rstanarm} package allows these models
#' to be specified using the customary R modeling syntax (e.g., like that of
#' \code{\link[stats]{glm}} with a \code{formula} and a \code{data.frame}).
#'
#' The set of models supported by \pkg{rstanarm} is large (and will continue to
#' grow), but also limited enough so that it is possible to integrate them
#' tightly with the \code{\link{pp_check}} function for graphical posterior
#' predictive checks and the \code{\link{posterior_predict}} function to easily
#' estimate the effect of specific manipulations of predictor variables or to
#' predict the outcome in a training set.
#'
#' The objects returned by the \pkg{rstanarm} modeling functions are called
#' \code{\link[=stanreg-objects]{stanreg}} objects. In addition to all of the
#' typical \code{\link[=stanreg-methods]{methods}} defined for fitted model
#' objects, stanreg objects can be passed to the \code{\link[loo]{loo}} function
#' in the \pkg{loo} package for model comparison or to the
#' \code{\link[shinystan]{launch_shinystan}} function in the \pkg{shinystan}
#' package in order to visualize the posterior distribution using the ShinyStan
#' graphical user interface. See the \pkg{rstanarm} vignettes for more details
#' about the entire process.
#'
NULL

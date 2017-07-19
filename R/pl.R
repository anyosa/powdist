#' @export
dpl <- function(x, lambda, mu = 0, sigma = 1){
  d = (lambda/sigma) * dlogis((x-mu)/sigma) * (plogis((x-mu)/sigma)**(lambda-1))
  return(d)
}

#' @export
ppl <- function(q, lambda, mu = 0, sigma = 1){
  p = plogis(q, mu, sigma)
  return(p**lambda)
}

#' @export
qpl <- function(p, lambda,  mu = 0, sigma = 1){
  q = qlogis(p**(1/lambda))* sigma + mu
  return(q)
}

#' @export
rpl = function(n, lambda, mu= 0, sigma = 1){
  n = runif(n)
  x = qpl(n, lambda, mu, sigma)
  return(x)
}

qpl <- function(p, lambda,  mu = 0, sigma = 1){
  q = qlogis(p**(1/lambda))* sigma + mu
  return(q)
}

qplr <- function(p, lambda, mu = 0, sigma = 1){
  q = -qlogis((1-p)**(1/lambda))* sigma + mu
  return(q)
}

qpn <- function(p, lambda, mu = 0, sigma = 1){
  q = qnorm(p**(1/lambda))* sigma + mu
  return(q)
}

qpnr <- function(p, lambda, mu = 0, sigma = 1){
  q = -qnorm((1-p)**(1/lambda))* sigma + mu
  return(q)
}

qpc <- function(p, lambda, mu = 0, sigma = 1){
  q = qcauchy(p**(1/lambda))* sigma + mu
  return(q)
}

qpcr <- function(p, lambda, mu = 0, sigma = 1){
  q = -qcauchy((1-p)**(1/lambda))* sigma + mu
  return(q)
}

qpgvma <- function(p, lambda, mu = 0, sigma = 1){
  q = qgumbel(p**(1/lambda))* sigma + mu
  return(q)
}

qpgvmar <- function(p, lambda, mu = 0, sigma = 1){
  q = -qgumbel((1-p)**(1/lambda))* sigma + mu
  return(q)
}

qpgvmi <- function(p, lambda, mu = 0, sigma = 1){
  q = -qgumbel(1-(p**(1/lambda)))* sigma + mu
  return(q)
}

qpgvmir <- function(p, lambda, mu = 0, sigma = 1){
  q = qgumbel(1-((1-p)**(1/lambda)))* sigma + mu
  return(q)
}

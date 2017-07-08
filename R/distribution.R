ppl <- function(q, lambda, mu = 0, sigma = 1){
  p = plogis(q, mu, sigma)
  return(p**lambda)
}

pplr <- function(q, lambda, mu = 0, sigma = 1){
  p = plogis(-q, mu, sigma)
  return(1-p**lambda)
}

ppn <- function(q, lambda, mu = 0, sigma = 1){
  p = pnorm(q, mu, sigma)
  return(p**lambda)
}

ppnr <- function(q, lambda, mu = 0, sigma = 1){
  p = pnorm(-q, mu, sigma)
  return(1-p**lambda)
}

ppc <- function(q, lambda, mu = 0, sigma = 1){
  p = pcauchy(q, mu, sigma)
  return(p**lambda)
}

ppcr <- function(q, lambda, mu = 0, sigma = 1){
  p = pcauchy(-q, mu, sigma)
  return(1-p**lambda)
}#ok

ppgvma <- function(q, lambda, mu = 0, sigma = 1){
  p = pgumbel(q, mu, sigma)
  return(p**lambda)
}

ppgvmar <- function(q, lambda, mu = 0, sigma = 1){
  p = pgumbel(-q, mu, sigma)
  return(1-p**lambda)
}

ppgvmi <- function(q, lambda, mu = 0, sigma = 1){
  p = 1-pgumbel(-q, mu, sigma)
  return(p**lambda)
}

ppgvmir <- function(q, lambda, mu = 0, sigma = 1){
  p = 1-pgumbel(q, mu, sigma)
  return(1-p**lambda)
}

dpl <- function(x, lambda, mu = 0, sigma = 1){
  d = (lambda/sigma) * dlogis((x-mu)/sigma) * (plogis((x-mu)/sigma)**(lambda-1))
  return(d)
}

dplr <- function(x, lambda, mu = 0, sigma = 1){
  d = (lambda/sigma) * dlogis((x-mu)/sigma) * (plogis((-x-mu)/sigma)**(lambda-1))
  return(d)
}

dpn <- function(x, lambda, mu = 0, sigma = 1){
  d = (lambda/sigma) * dnorm((x-mu)/sigma) * (pnorm((x-mu)/sigma)**(lambda-1))
  return(d)
}

dpnr <- function(x, lambda, mu = 0, sigma = 1){
  d = (lambda/sigma) * dnorm((x-mu)/sigma) * (pnorm((-x-mu)/sigma)**(lambda-1))
  return(d)
}

dpc <- function(x, lambda, mu = 0, sigma = 1){
  d = (lambda/sigma) * dcauchy((x-mu)/sigma) * (pcauchy((x-mu)/sigma)**(lambda-1))
  return(d)
}

dpcr <- function(x, lambda, mu = 0, sigma = 1){
  d = (lambda/sigma) * dcauchy((x-mu)/sigma) * (pcauchy((-x-mu)/sigma)**(lambda-1))
  return(d)
}

dpgvma <- function(x, lambda, mu = 0, sigma = 1){
  d = (lambda/sigma) * dgumbel((x-mu)/sigma) * ( pgumbel((x-mu)/sigma) **(lambda-1))
  return(d)
}

dpgvmar <- function(x, lambda, mu = 0, sigma = 1){
  d = (lambda/sigma) * dgumbel((x-mu)/sigma) * ( pgumbel((-x-mu)/sigma) **(lambda-1))
  return(d)
}

dpgvmi <- function(x, lambda, mu = 0, sigma = 1){
  d = (lambda/sigma) * dgumbel((-x-mu)/sigma) * ( (1-pgumbel((-x-mu)/sigma)) **(lambda-1))
  return(d)
}

dpgvmir <- function(x, lambda, mu = 0, sigma = 1){
  d = (lambda/sigma) * dgumbel((-x-mu)/sigma) * ( (1-pgumbel((x-mu)/sigma)) **(lambda-1))
  return(d)
}

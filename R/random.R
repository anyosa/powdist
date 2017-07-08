rpl = function(n, lambda, mu= 0, sigma = 1){
  n = runif(n)
  x = qpl(n, lambda, mu, sigma)
  return(x)
}

rplr = function(n, lambda, mu= 0, sigma = 1){
  n = runif(n)
  x = qplr(n, lambda, mu, sigma)
  return(x)
}

rpn = function(n, lambda, mu= 0, sigma = 1){
  n = runif(n)
  x = qpn(n, lambda, mu, sigma)
  return(x)
}

rpnr = function(n, lambda, mu= 0, sigma = 1){
  n = runif(n)
  x = qpnr(n, lambda, mu, sigma)
  return(x)
}

rpc = function(n, lambda, mu= 0, sigma = 1){
  n = runif(n)
  x = qpc(n, lambda, mu, sigma)
  return(x)
}

rpcr = function(n, lambda, mu= 0, sigma = 1){
  n = runif(n)
  x = qpcr(n, lambda, mu, sigma)
  return(x)
}

rpgvma = function(n, lambda, mu= 0, sigma = 1){
  n = runif(n)
  x = qpgvma(n, lambda, mu, sigma)
  return(x)
}

rpgvmar = function(n, lambda, mu= 0, sigma = 1){
  n = runif(n)
  x = qpgvmar(n, lambda, mu, sigma)
  return(x)
}

rpgvmi = function(n, lambda, mu= 0, sigma = 1){
  n = runif(n)
  x = qpgvmi(n, lambda, mu, sigma)
  return(x)
}

rpgvmir = function(n, lambda, mu= 0, sigma = 1){
  n = runif(n)
  x = qpgvmir(n, lambda, mu, sigma)
  return(x)
}

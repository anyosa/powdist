#chaging names "pot dist rev" or "pow rev dist" 

#have to use regular expression for this kind of functions , see pnorm, also check density for the other and the names
#densities: dpl dplr dpn dpnr dpc dpcr dpgvma dpgvmar dpgvmi dpgvmir

require(VGAM)

dpl <- function(eta,lambda){
  d = lambda * dlogis(eta) * (plogis(eta)**(lambda-1))
  return(d)
}

dplr <- function(eta,lambda){
  d = lambda * dlogis(eta) * (plogis(-eta)**(lambda-1))
  return(d)
}

dpn <- function(eta,lambda){
  d = lambda * dnorm(eta) * (pnorm(eta)**(lambda-1))
  return(d)
}

dpnr <- function(eta,lambda){
  d = lambda * dnorm(eta) * (pnorm(-eta)**(lambda-1))
  return(d)
}

dpc <- function(eta,lambda){
  d = lambda * dcauchy(eta) * (pcauchy(eta)**(lambda-1))
  return(d)
}

dpcr <- function(eta,lambda){
  d = lambda * dcauchy(eta) * (pcauchy(-eta)**(lambda-1))
  return(d)
}

dpgvma <- function(eta,lambda){
  d = lambda * dgumbel(eta) * ( pgumbel(eta) **(lambda-1))
  return(d)
}

dpgvmar <- function(eta,lambda){
  d = lambda * dgumbel(eta) * ( pgumbel(-eta) **(lambda-1))
  return(d)
}

dpgvmi <- function(eta,lambda){
  d = lambda * dgumbel(-eta) * ( (1-pgumbel(-eta)) **(lambda-1))
  return(d)
}

dpgvmir <- function(eta,lambda){
  d = lambda * dgumbel(-eta) * ( (1-pgumbel(eta)) **(lambda-1))
  return(d)
}
#dpgma2 <- function(eta,lambda){
#  d = lambda * (exp(-(eta + exp(-eta)))) * ( (exp(-exp(-eta))) **(lambda-1))
#  return(d)
#}

#dpgmar2 <- function(eta,lambda){
#  d = lambda * (exp(-(eta + exp(-eta)))) * ( (exp(-exp(eta))) **(lambda-1))
#  return(d)
#}

#dpgmi2 <- function(eta,lambda){
#  d = lambda * (exp(-(-eta + exp(eta)))) * ( (1-exp(-exp(eta))) **(lambda-1))
#  return(d)
#}

#dpgmir2 <- function(eta,lambda){
#  d = lambda * (exp(-(-eta + exp(eta)))) * ( (1-exp(-exp(-eta))) **(lambda-1))
#  return(d)
#}
###

#probabilities: ppl pplr ppn ppnr ppc ppcr ppgvma ppgvmar ppgvmi ppgvmir

ppl <- function(eta,lambda){
 	p = plogis(eta)
 	return(p**lambda)
}

pplr <- function(eta,lambda){
 	p = plogis(-eta)
 	return(1-p**lambda)
}

ppn <- function(eta,lambda){
 	p = pnorm(eta)
 	return(p**lambda)
}

ppnr <- function(eta,lambda){
 	p = pnorm(-eta)
 	return(1-p**lambda)
}

ppc <- function(eta,lambda){
 	p = pcauchy(eta)
 	return(p**lambda)
}

ppcr <- function(eta,lambda){
 	p = pcauchy(-eta)
 	return(1-p**lambda)
}

ppgvma <- function(eta,lambda){
  p = pgumbel(eta)
  return(p**lambda)
}

ppgvmar <- function(eta,lambda){
  p = pgumbel(-eta)
  return(1-p**lambda)
}

ppgvmi <- function(eta,lambda){
  p = 1-pgumbel(-eta)
  return(p**lambda)
}

ppgvmir <- function(eta,lambda){
  p = 1-pgumbel(eta)
  return(1-p**lambda)
}


#ppgmi2 <- function(eta,lambda){
# 	p = 1-exp(-exp(eta))
# 	return(p**lambda)
#}

#ppgmir2 <- function(eta,lambda){
# 	p = 1-exp(-exp(-eta))
# 	return(1-p**lambda)
#}

#ppgma2 <- function(eta,lambda){
# 	p = exp(-exp(-eta))
# 	return(p**lambda)
#}

#ppgmar2 <- function(eta,lambda){
# 	p = exp(-exp(eta))
# 	return(1-p**lambda)
#}

###

#quantil: qpl qplr qpn qpnr qpc qpcr qpgvma qpgvmar qpgvmi qpgvmir

qpl <- function(p,lambda){
  q =  qlogis(p**(1/lambda))
  return(q)
}

qplr <- function(p,lambda){
  q =  -qlogis((1-p)**(1/lambda))
  return(q)
}

qpn <- function(p,lambda){
  q = qnorm(p**(1/lambda))
  return(q)
}

qpnr <- function(p,lambda){
  q = -qnorm((1-p)**(1/lambda))
  return(q)
}

qpc <- function(p,lambda){
  q = qcauchy(p**(1/lambda))
  return(q)
}

qpcr <- function(p,lambda){
  q = -qcauchy((1-p)**(1/lambda))
  return(q)
}

#qpgmi2 <- function(p,lambda){
#  q = log(-log(1-(p**(1/lambda))))
#  return(q)
#}

#qpgmir2 <- function(p,lambda){
#  q = -log(-log(1-((1-p)**(1/lambda))))
#  return(q)
#}

#qpgma2 <- function(p,lambda){
#  q = -log(-log(p**(1/lambda)))
#  return(q)
#}

#qpgmar2 <- function(p,lambda){
#  q = log(-log((1-p)**(1/lambda)))
#  return(q)
#}

#####################################

qpgvma <- function(p,lambda){
  q = qgumbel(p**(1/lambda))
  return(q)
}

qpgvmar <- function(p,lambda){
  q = -qgumbel((1-p)**(1/lambda))
  return(q)
}

qpgvmi <- function(p,lambda){
  q = -qgumbel(1-(p**(1/lambda)))
  return(q)
}

qpgvmir <- function(p,lambda){
  q = qgumbel(1-((1-p)**(1/lambda)))
  return(q)
}

#random generated x: rpl rplr

rpl = function(n,lambda){
  n = runif(n)
  x = qpl(n,lambda)
  return(x)
}

rplr = function(n,lambda){
  n = runif(n)
  x = qplr(n,lambda)
  return(x)
}

rpn = function(n,lambda){
  n = runif(n)
  x = qpn(n,lambda)
  return(x)
}

rpnr = function(n,lambda){
  n = runif(n)
  x = qpnr(n,lambda)
  return(x)
}

rpc = function(n,lambda){
  n = runif(n)
  x = qpc(n,lambda)
  return(x)
}

rpcr = function(n,lambda){
  n = runif(n)
  x = qpcr(n,lambda)
  return(x)
}

rpgvma = function(n,lambda){
  n = runif(n)
  x = qpgvma(n,lambda)
  return(x)
}

rpgvmar = function(n,lambda){
  n = runif(n)
  x = qpgvmar(n,lambda)
  return(x)
}

rpgvmi = function(n,lambda){
  n = runif(n)
  x = qpgvmi(n,lambda)
  return(x)
}

rpgvmir = function(n,lambda){
  n = runif(n)
  x = qpgvmir(n,lambda)
  return(x)
}
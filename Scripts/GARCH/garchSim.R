### Packages -----------------------------------------------------------------
library(tidyverse)
library(parallel)

### Variable grid ------------------------------------------------------------


### GARCH Monte Carlo function -----------------------------------------------
set.seed(2019)

garchMonteCarloFun <- function(omega, b, a, mu, K, MT, r, s0, N = 100000){
  h <- s <- matrix(NA, nrow = N, ncol = MT)
  eps <- matrix(rnorm(MT*N), nrow = N, ncol = MT)
  h[,1] <- 0.15^2/252
  s[,1] <- exp(r - .5*h[,1] + mu - r + sqrt(h[,1]))*s0
  
  for (i in 1:(MT - 1)) {
    lambda <- (mu - r)/h[,i]
    h[,i+1] <- omega + b*h[,i] + a*(sqrt(h[,i])*eps[,i] + mu - r - 
                                        lambda*sqrt(h[,i]))^2
    s[,i+1] <- exp(r - .5*h[,i+1] + mu - r + sqrt(h[,i+1]))*s[,i]
  }
  
  g = pmax((s[,MT] - K), 0)*exp(-r*MT)
  
  return(mean(g))
}

garchMonteCarloFun(omega = .2, b = .2, a = .1, mu = 0, K = 10, MT = 10,
                   r = 0.1, s0 = 8, N = 100000)

### Packages -----------------------------------------------------------------
library(tidyverse)
library(parallel)

### Variable grid ------------------------------------------------------------
s0 <- seq(305, 309, by = 1.5) # Current instrument price
K <- seq(250, 300, by = 2) # Strike price
MT <- seq(2, 10, by = 1) # Time to maturity
r <- 15*0.0153/91.5 # Risk free rate
omega <- seq(-0.5, 0.5, by = 0.5)
b <- seq(-0.05, 0.10, by = 0.05)
a <- seq(-0.10, 0.20, by = 0.05)
mu <- seq(-0.10, 0.20, by = 0.05)

variableGrid <- expand_grid(s0 = s0, K = K, r = r, MT = MT,
                            omega = omega, b = b, a = a, mu = mu)

variableGrid <- variableGrid[1:150000,] 

### GARCH Monte Carlo function -----------------------------------------------
set.seed(2019)

garchMonteCarloFun <- function(omega, b, a, mu, K, MT, r, s0, N = 50000){
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

### Simulation ---------------------------------------------------------------
cores <- detectCores() - 1

data <- mcmapply(garchMonteCarloFun,
                 s0 = variableGrid$s0, 
                 K = variableGrid$K, 
                 r = variableGrid$r, 
                 MT = variableGrid$MT,
                 omega = variableGrid$omega, 
                 b = variableGrid$b, 
                 a = variableGrid$a, 
                 mu = variableGrid$mu,
                 mc.cores = cores)

### Save ---------------------------------------------------------------------
write.csv(data, "./Data//garch1.csv.gz")
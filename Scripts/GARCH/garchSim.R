### Packages -----------------------------------------------------------------
library(tidyverse)
library(parallel)

### Variable grid ------------------------------------------------------------
s0 <- seq(305, 309, by = 1) # Current instrument price
K <- seq(250, 350, by = 2) # Strike price
MT <- seq(2, 10, by = 1) # Time to maturity
r <- 15*0.0153/91.5 # Risk free rate
omega <- seq(0, 0.5, by = 0.1)
b <- seq(0, 0.10, by = 0.025)
a <- seq(0, 0.20, by = 0.05)
mu <- seq(0, 0.20, by = 0.05)

variableGrid <- expand_grid(s0 = s0, K = K, r = r, MT = MT,
                            omega = omega, b = b, a = a, mu = mu) %>% 
  filter(a + b < 1, omega + a + b != 0)

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
    s[,i+1] <- exp(r - .5*h[,i+1] + mu - r + sqrt(h[,i+1])*eps[,i+1])*s[,i]
  }
  
  g = pmax((s[,MT] - K), 0)*exp(-r*MT)
  
  return(mean(g))
}

garchMonteCarloFun(variableGrid$omega[1],
                   variableGrid$b[1],
                   variableGrid$a[1],
                   variableGrid$mu[1],
                   variableGrid$K[1],
                   variableGrid$MT[1], 
                   variableGrid$r[1],
                   variableGrid$s0[1])

### Simulation ---------------------------------------------------------------
cores <- detectCores() - 1

data <- pbmapply(garchMonteCarloFun,
                 s0 = variableGrid$s0, 
                 K = variableGrid$K, 
                 r = variableGrid$r, 
                 MT = variableGrid$MT,
                 omega = variableGrid$omega, 
                 b = variableGrid$b, 
                 a = variableGrid$a, 
                 mu = variableGrid$mu)

data <- variableGrid %>% 
  mutate(cHat = data)

### Save ---------------------------------------------------------------------
write.csv("./Data//garch.csv.gz")
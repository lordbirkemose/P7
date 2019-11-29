###  Packages ----------------------------------------------------------------
library(tidyverse)
library(parallel)
library(pbapply)

### Grid ---------------------------------------------------------------------
S0 <- seq(305, 309, by = 1) # Current instrument price
K <- seq(200, 350, by = 1) # Strike price
MT <- seq(1, 30, by = 1) # Time to maturity
# r <- seq(0, 2.5, by = 0.3) # Risk free rate
r <- seq(1, 30, by = 2)*0.0153/91.5
sigma <- seq(0.1, 1, by = 0.05) # Volatility of the instrument

variableGrid <- expand.grid(S0 = S0, K = K, r = r, MT = MT, sigma = sigma)

### Black and Scholes price function -----------------------------------------
set.seed(2019)

monteCarloFun <- function(S0, K, r, MT, sigma) {
  sT <- S0*exp((r - .5*sigma^2)*MT + sigma*sqrt(MT)*rnorm(100000))
  cHat <- pmax(sT - K, 0)*exp(-r*MT)
  return(mean(cHat))
}

### Simulation for grid ------------------------------------------------------

C <- mcmapply(monteCarloFun, 
            S0 = variableGrid$S0,
            K = variableGrid$K,
            r = variableGrid$r,
            MT = variableGrid$MT, 
            sigma = variableGrid$sigma)

BlackScholesMonteCarloData <- variableGrid %>% 
  mutate(C = C)

write.csv(BlackScholesMonteCarloData, 
          gzfile("./Data//BlackScholesMonteCarloDatacsv.gz"), 
          row.names = FALSE)

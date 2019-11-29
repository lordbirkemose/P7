### Packages -----------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(microbenchmark)

### Get data -----------------------------------------------------------------
SPY <- read.csv("./Data//SPY.csv") %>% 
  filter(Type == "call", K >= 200) %>% 
  mutate(MT = as.numeric(as.Date(Tt) - as.Date(Start)),
         r = 0.0153/91.5*MT) %>% 
  filter(MT <= 30) %>% 
  select(S0, K, r, MT, C = P)

variableRange <- SPY %>%  
  select(-C)

### Calibration function -----------------------------------------------------
monteCarloFun <- function(S0, K, r, MT, sigma) {
  sT <- S0*exp((r - .5*sigma^2)*MT + sigma*sqrt(MT)*rnorm(100000))
  cHat <- pmax(sT - K, 0)*exp(-r*MT)
  return(mean(cHat))
}

n <- nrow(variableRange)

funcCalibrate <- function(sigma) {
  
  blackScholes <- mapply(monteCarloFun, 
                         S0 = variableRange$S0,
                         K = variableRange$K,
                         r = variableRange$r,
                         MT = variableRange$MT, 
                         sigma = rep(sigma, n))
  
  return(sum((blackScholes - SPY$C)^2))
}

### Calibration --------------------------------------------------------------
sigma0 <- 2
lB     <- 0.01
uB     <- 5

sigmaOptim <- optim(sigma0, funcCalibrate, 
                    lower = lB, upper = uB, 
                    method = "L-BFGS-B", 
                    control = list(trace = TRUE, maxit = 500))

### Microbenchmark -----------------------------------------------------------
microbenchmark(optim(sigma0, funcCalibrate, 
                     lower = lB, upper = uB, 
                     method = "L-BFGS-B", 
                     control = list(trace = FALSE, maxit = 500)),
               unit = "us")

S0 <- seq(305, 309, by = 1) # Current instrument price
K <- seq(200, 350, by = 1) # Strike price
MT <- seq(1, 30, by = 1) # Time to maturity
# r <- seq(0, 2.5, by = 0.3) # Risk free rate
r <- seq(1, 30, by = 2)*0.0153/91.5
sigma <- seq(0.1, 1, by = 0.05) # Volatility of the instrument

variableGrid <- expand.grid(S0 = S0, K = K, r = r, MT = MT, sigma = sigma)
microbenchmark(mapply(monteCarloFun, 
                      S0 = variableGrid$S0,
                      K = variableGrid$K,
                      r = variableGrid$r,
                      MT = variableGrid$MT, 
                      sigma = variableGrid$sigma),
               unit = "us", times = 10)
### Packages -----------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(parallel)
library(microbenchmark)

### Get data -----------------------------------------------------------------
SPY <- read.csv("./Data//SPY.csv") %>% 
  filter(Type == "call", K >= 200) %>% 
  mutate(MT = as.numeric(as.Date(Tt) - as.Date(Start)),
         r = 0.0153/91.5*MT) %>% 
  filter(MT <= 30) %>% 
  select(S0, K, r, MT, C = P)

SPY <- SPY[1:1000,]

variableRange <- SPY %>%  
  select(-C)

### Calibration function -----------------------------------------------------
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

cores <- detectCores() - 1
n <- nrow(variableRange)

funcCalibrate <- function(theta) {
  garch <- mapply(garchMonteCarloFun,
                    s0 = variableRange$S0,
                    K = variableRange$K,
                    r = variableRange$r,
                    MT = variableRange$MT,
                    omega = rep(theta[1], n),
                    b = rep(theta[2], n),
                    a = rep(theta[3], n),
                    mu = rep(theta[4], n))
  
  return(sum((garch - SPY$C)^2))
}

### Calibration --------------------------------------------------------------
theta0 <- c(0.1, 0.1, 0.1, 0.1)
ui <- rbind(c(0, -1, -1, 0),
            c(0,  1,  1, 0))
ci <- c(-.99, -.99)

sigmaOptim <- constrOptim(theta0, funcCalibrate, 
                    ui = ui, ci = ci, 
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
microbenchmark(mapply(BlackScholesFun, 
                      S0 = variableGrid$S0,
                      K = variableGrid$K,
                      r = variableGrid$r,
                      MT = variableGrid$MT, 
                      sigma = variableGrid$sigma),
               unit = "us", times = 10)
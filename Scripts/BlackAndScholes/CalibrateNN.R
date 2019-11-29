### Packages -----------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(microbenchmark)
library(keras)
library(tensorflow)
install_tensorflow()

### Get data -----------------------------------------------------------------
S0 <- seq(305, 309, by = 1) # Current instrument price
K <- seq(200, 350, by = 1) # Strike price
MT <- seq(1, 30, by = 1) # Time to maturity
# r <- seq(0, 2.5, by = 0.3) # Risk free rate
r <- seq(1, 30, by = 2)*0.0153/91.5
sigma <- seq(0.1, 1, by = 0.05) # Volatility of the instrument

variableGrid <- expand.grid(S0 = S0, K = K, r = r, MT = MT, sigma = sigma)

BlackScholesFun <- function(S0, K, r, MT, sigma) {
  d1 <- (log(S0/K) + (r + sigma^2/2)*MT)/(sigma*sqrt(MT))
  d2 <- d1 - sigma*sqrt(MT)
  
  C <- pnorm(d1)*S0 - pnorm(d2)*K*exp(-r*MT)
  
  return(C)
}

C <- mapply(BlackScholesFun, 
            S0 = variableGrid$S0,
            K = variableGrid$K,
            r = variableGrid$r,
            MT = variableGrid$MT, 
            sigma = variableGrid$sigma)

data <- variableGrid %>% 
  mutate(C = C)

minData <- min(data)
maxData <- max(data)

SPY <- read.csv("./Data//SPY.csv") %>% 
  filter(Type == "call", K >= 200) %>% 
  mutate(MT = as.numeric(as.Date(Tt) - as.Date(Start)),
         r = 0.0153/91.5*MT) %>% 
  filter(MT <= 30) %>% 
  select(S0, K, r, MT, C = P)

NN <- load_model_hdf5(paste0("~/Desktop//P7//LargeDataFromServer"
                             ,"//BlackScholesNn.h5"))

variableRange <- (2*SPY - maxData - minData)/(maxData - minData) 
variableRange %<>% 
  select(-C) %>%
  set_colnames(NULL) %>% 
  as.matrix()

### Calibration function -----------------------------------------------------

n <- nrow(variableRange)

funcCalibrate <- function(sigma) {
  
  nnPredict <- NN %>%  
    predict(cbind(variableRange, rep(sigma, n)))
  
  return(sum(abs((nnPredict*(maxData - minData) + (maxData + minData))/2 - 
                   SPY$C)^2))
}

### Calibration --------------------------------------------------------------

sigma0 <- -0.1
lB     <- -1
uB     <- 1

sigmaOptim <- optim(sigma0, funcCalibrate, 
                    lower = lB, upper = uB, 
                    method = "L-BFGS-B", 
                    control = list(trace = TRUE, maxit = 500))

sigma <- (sigmaOptim$par*(maxData - minData) + (maxData + minData))/2;sigma

### Microbenchmark -----------------------------------------------------------
variableGrid <- variableGrid %>% 
  set_colnames(NULL) %>% 
  as.matrix()

microbenchmark(optim(sigma0, funcCalibrate, 
                     lower = lB, upper = uB, 
                     method = "L-BFGS-B", 
                     control = list(trace = FALSE, maxit = 500)),
               unit = "us")

microbenchmark(NN %>% predict(variableGrid),
               unit = "us", times = 10)

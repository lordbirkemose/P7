### Packages -----------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(keras)
library(tensorflow)
install_tensorflow()

### Get data -----------------------------------------------------------------

SPY <- read.csv("./Data//SPY.csv") %>% 
  filter(Type == "call") %>% 
  mutate(MT = as.numeric(as.Date(Tt) - as.Date(Start)),
         r = 0.0153/91.5*MT) %>% 
  select(S0, K, r, MT, C = P)

BlackScholesNnDropout <- load_model_hdf5(paste0("./Workspaces//",
                                                "BlackScholesNnDropout.h5"))

variableRange <- SPY %>% 
  select(-C) %>%
  set_colnames(NULL) %>% 
  as.matrix()

### Calibration function -----------------------------------------------------

n <- nrow(variableRange)

funcCalibrate <- function(sigma) {
  
  nnPredict <- BlackScholesNnDropout %>%  
    predict(cbind(variableRange, rep(sigma, n)))
  
  return(sum(abs(nnPredict - SPY$C)^2))
}

### Calibration --------------------------------------------------------------

sigma0 <- 0.1
lB     <- 0
uB     <- 100

sigmaOptim <- optim(sigma0, funcCalibrate, lower = lB, upper = uB, 
                    method = "L-BFGS-B", control=list(trace=TRUE, maxit= 500))

### Plots with sigmaOptim ----------------------------------------------------

dataPlot <- dataTest %>% 
  group_by(K, MT) %>% 
  summarise(n = n(),
            diff = sum(abs(C - cHat))) %>% 
  mutate(MAE = diff/n) %>% 
  select(K, MT, MAE)

ggplot(data = dataPlot, aes(y = MT, x = K, fill = MAE)) +
  geom_tile() +
  labs(title  = "Dropout 1000 epochs 100 batch",
       x = "Strike",
       y = "Maturity")
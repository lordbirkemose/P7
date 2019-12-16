### Packages -----------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(microbenchmark)
library(keras)
library(tensorflow)
install_tensorflow()

### Get data -----------------------------------------------------------------
data <- read.csv("./Data/garchMC.csv.gz")

path <- "./Data/garchNn500Epoch50Batch50Neurons/"
load(paste0(path,"DataTest.Rdata"))
dataTrain <- read.csv(paste0(path, "DataTrain.csv.gz"))
NN <- load_model_hdf5(paste0(path, "NN.h5"))

minData <- min(data)
maxData <- max(data)

SPY <- read.csv("./Data//SPY.csv") %>% 
  filter(Type == "call", K >= 200) %>% 
  mutate(MT = as.numeric(as.Date(Tt) - as.Date(Start)),
         r = 0.0153/91.5*MT) %>% 
  filter(MT <= 30) %>% 
  select(S0, K, r, MT, C = P)

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
theta0 <- c(0.1, 0.1, 0.1, 0.1)
ui <- rbind(c(0, -1, -1, 0),
            c(0,  1,  1, 0))
ci <- c(-.99, -.99)

sigmaOptim <- constrOptim(theta0, funcCalibrate, 
                          ui = ui, ci = ci, 
                          method = "Nelder-Mead",
                          control = list(trace = TRUE, maxit = 500))

### Microbenchmark -----------------------------------------------------------
theta0 <- c(0.1, 0.1, 0.1, 0.1)
ui <- rbind(c(0, -1, -1, 0),
            c(0,  1,  1, 0))
ci <- c(-.99, -.99)

microbenchmark(constrOptim(theta0, funcCalibrate, 
                           ui = ui, ci = ci, 
                           control = list(trace = TRUE, maxit = 500)),
               unit = "s", times = 1)


### Full surface -------------------------------------------------------------
nnData <- rbind(dataTrain, dataTest) %>% 
  select(-cHat, -C) %>% 
  as.matrix() %>% 
  set_colnames(NULL)

microbenchmark(predict(NN, nnData),
               unit = "s", times = 1)

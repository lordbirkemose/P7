### Packages -----------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(keras)
library(tensorflow)
library(plotly)
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

pal <- wes_palette("Zissou1", 100, type = "continuous")

nnPredict <- BlackScholesNnDropout %>%  
  predict(cbind(variableRange, rep(sigmaOptim$par, n)))

dataHeatmap <- SPY %>% 
  mutate(cHat = nnPredict) %>% 
  group_by(K, MT) %>% 
  summarise(n = n(),
            diff = sum(abs(C - cHat))) %>% 
  mutate(MAE = diff/n)

ggplot(data = dataHeatmap, aes(y = MT, x = K, fill = MAE)) +
  geom_tile() +
  labs(title  = "Calibrated vs actually",
       x = "Strike",
       y = "Maturity") +
  scale_fill_gradientn(colours = pal)


dataPlot <- SPY %>% 
  mutate(cHat = nnPredict) %>% 
  filter(K == 300)

ggplot(data = dataPlot, aes(x = MT)) +
  geom_line(aes(y = C, colour = "Black and Scholes")) + 
  geom_line(aes(y = cHat, colour = "Neural network")) +
  labs(title = "Strike: 300") +
  xlab("Time to maturity") +
  ylab("Price in US$") +
  scale_colour_manual("", values = c("Black and Scholes" = "#ffb347", 
                                     "Neural network" = "#aec6cf"))



### Packages -----------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(wesanderson)

### Colors -------------------------------------------------------------------
pal <- wes_palette("Zissou1", 100, type = "continuous")

### Dropout 200 epochs 25 batch ----------------------------------------------
load("./Workspaces//BlackScholesNnDataTest.Rdata")

dataPlot <- dataTest %>% 
  group_by(K, MT) %>% 
  summarise(n = n(),
            diff = sum(abs(C - cHat))) %>% 
  mutate(MAE = diff/n) %>% 
  select(K, MT, MAE)

ggplot(data = dataPlot, aes(y = MT, x = K, fill = MAE)) +
  geom_tile() +
  labs(title  = "Dropout 200 epochs 25 batch",
       x = "Strike",
       y = "Maturity") + 
  scale_fill_gradientn(colours = pal,
                       breaks = seq(-100,100,10),
                       limits = c(0, 70))

### Dropout 1000 epochs 100 batch --------------------------------------------
load("./Workspaces//BlackScholesNnDataTest1000Epochs.Rdata")

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
       y = "Maturity") + 
  scale_fill_gradientn(colours = pal,
                       breaks = seq(-100,100,10),
                       limits = c(0, 70))
  
### No dropout 1000 epochs 25 batch ------------------------------------------
load("./Workspaces//BlackScholesNnDataTestNoDropout1000Epochs.Rdata")

dataPlot <- dataTest %>% 
  group_by(K, MT) %>% 
  summarise(n = n(),
            diff = sum(abs(C - cHat))) %>% 
  mutate(MAE = diff/n) %>% 
  select(K, MT, MAE)

ggplot(data = dataPlot, aes(y = MT, x = K, fill = MAE)) +
  geom_tile() +
  labs(title  = "No dropout 1000 epochs 25 batch",
       x = "Strike",
       y = "Maturity") + 
  scale_fill_gradientn(colours = pal,
                       breaks = seq(-100,100,10),
                       limits = c(0, 70))

### Dropout 1000 epochs 25 batch ---------------------------------------------
load("./Workspaces//BlackScholesNnDataTest1000Epochs25Batch.Rdata")

dataPlot <- dataTest %>% 
  group_by(K, MT) %>% 
  summarise(n = n(),
            diff = sum(abs(C - cHat))) %>% 
  mutate(MAE = diff/n) %>% 
  select(K, MT, MAE)

ggplot(data = dataPlot, aes(y = MT, x = K, fill = MAE)) +
  geom_tile() +
  labs(title  = "Dropout 1000 epochs 25 batch",
       x = "Strike",
       y = "Maturity") + 
  scale_fill_gradientn(colours = pal,
                       breaks = seq(-100,100,10),
                       limits = c(0, 70))

### Large data set -----------------------------------------------------------
#'S0 <- 306.17 # Current instrument price
#'K <- seq(200, 350, by = 1) # Strike price
#'MT <- seq(1, 30, by = 1) # Time to maturity
#'# r <- seq(0, 2.5, by = 0.3) # Risk free rate
#'r <- 0.0153*MT/91.5
#'sigma <- seq(0.1, 1, by = 0.05) # Volatility of the instrument

load("~/Desktop//P7//LargeDataFromServer//BlackScholesNnDataTest.Rdata")

dataPlot <- dataTest %>% 
  group_by(K, MT) %>% 
  summarise(n = n(),
            diff = sum(abs(C - cHat))) %>% 
  mutate(MAE = diff/n) %>% 
  select(K, MT, MAE)

ggplot(data = dataPlot, aes(y = MT, x = K, fill = MAE)) +
  geom_tile() +
  labs(title  = "Big data set",
       x = "Strike",
       y = "Maturity") + 
  scale_fill_gradientn(colours = pal,
                       breaks = seq(-100,100,10),
                       limits = c(0, 70))


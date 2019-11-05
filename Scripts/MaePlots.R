### Packages -----------------------------------------------------------------
library(tidyverse)
library(magrittr)

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
       y = "Maturity")

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
       y = "Maturity")


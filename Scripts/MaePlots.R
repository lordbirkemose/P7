### Packages -----------------------------------------------------------------
library(tidyverse)
library(magrittr)

### Load data ----------------------------------------------------------------
load("./Workspaces//BlackScholesNnData.Rdata")

### Test set -----------------------------------------------------------------

dataPlot <- dataTest %>% 
  group_by(K, MT) %>% 
  summarise(n = n(),
            diff = sum(abs(C - cHat))) %>% 
  mutate(MAE = diff/n) %>% 
  select(K, MT, MAE)

ggplot(data = dataPlot, aes(y = MT, x = K, fill = MAE)) +
  geom_tile() +
  labs(title  = "Mean absolute error",
       x = "Strike",
       y = "Maturity")

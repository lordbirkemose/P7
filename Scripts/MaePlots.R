### Packages -----------------------------------------------------------------
library(tidyverse)
library(magrittr)

### Load data ----------------------------------------------------------------
load("./Workspaces//BlackScholesNnData.Rdata")
data <- read.csv("./Data/BlackScholesData.csv.gz")

### Test set -----------------------------------------------------------------

cHat <- rnorm(nrow(data))
data %<>% mutate(cHat = cHat)

dataPlot <- data %>% 
  group_by(K, MT) %>% 
  summarise(n = n(),
            diff = sum(abs(C - cHat))) %>% 
  mutate(MAE = diff/n) %>% 
  select(K, MT, MAE)

ggplot(data = dataPlot, aes(y = MT, x = K, fill = MAE)) +
  geom_tile() +
  labs(title  = "Mean square error",
       x = "Strike",
       y = "Maturity")

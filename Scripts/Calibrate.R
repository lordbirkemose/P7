### Packages -----------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(keras)
library(tensorflow)
install_tensorflow()

### Get data -----------------------------------------------------------------

SPY <- read.csv("./Data//SPY.csv") %>% 
  filter(Type == "call") %>% 
  mutate(MT = as.Date(Tt) - as.Date(Start),
         r = 0.0153/91.5*MT) %>% 
  select(S0, K, r, MT, C = P)

BlackScholesNnDropout <- load_model_hdf5(paste0("./Workspaces//",
                                                "BlackScholesNnDropout.h5"))
nnData <- predict()
### 
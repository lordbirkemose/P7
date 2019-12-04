### Packagets ----------------------------------------------------------------
require("keras")
require("tensorflow")
require("tidyverse")

### Tensorflow setup ---------------------------------------------------------
install_tensorflow()

### Load data ----------------------------------------------------------------
data <- read.csv("./Data//garch.csv.gz")
minData <- min(data)
maxData <- max(data)

### Test and training set ----------------------------------------------------
set.seed(123)

indTrain <- sample(nrow(data), nrow(data)*0.75)

dataTrain <- (2*data[indTrain, ] - maxData - minData)/(maxData - minData)
dataTrain <- dataTrain %>% 
  select(-C) %>% 
  as.matrix() %>% 
  set_colnames(NULL)

dataTest <- (2*data[-indTrain, ] - maxData - minData)/(maxData - minData)
dataTest <- dataTest %>% 
  select(-C) %>% 
  as.matrix() %>% 
  set_colnames(NULL)

dataTrainTarget <- data[indTrain, ] %>% 
  mutate(C = (2*C - maxData - minData)/(maxData - minData)) %>% 
  select(C) %>% 
  as.matrix() %>% 
  set_colnames(NULL)

dataTestTarget <- data[-indTrain, ] %>% 
  mutate(C = (2*C - maxData - minData)/(maxData - minData)) %>% 
  select(C) %>% 
  as.matrix() %>% 
  set_colnames(NULL)

### Construction the model ---------------------------------------------------
NN <- keras_model_sequential()

NN %>% layer_dense(units = 50, activation = 'elu', 
                   input_shape = dim(dataTest)[2]) %>% 
  layer_dropout(rate = 0.8) %>% 
  layer_dense(units = 50, activation = 'elu') %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 50, activation = 'elu') %>% 
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 50, activation = 'elu') %>% 
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1, activation = "linear")

earlyStop <- callback_early_stopping(monitor = "val_loss", patience = 50)

### Compile and fit ----------------------------------------------------------
NN %>% compile(loss = 'mse', 
               optimizer = optimizer_rmsprop(lr = 0.001, rho = 0.9), 
               metrics = 'mean_absolute_error')

history <- BlackScholesNnDropout %>% 
  fit(dataTrain,
      dataTrainTarget,
      epochs = 1000,
      batch_size = 50, 
      validation_split = 0.2,
      verbose = 1,
      callbacks = list(earlyStop))

plot(history)

### Predict ------------------------------------------------------------------
dataTrain <- data[indTrain,] %>% 
  mutate(cHat = predict(NN, dataTrain),
         cHat = (cHat*(maxData - minData) + 
                   (maxData + minData))/2)

dataTest <- data[-indTrain,] %>% 
  mutate(cHat = predict(NN, dataTest),
         cHat = (cHat*(max(data) - min(data)) + 
                   (maxData + minData))/2)

# Saving model ---------------------------------------------------------------
save_model_hdf5(NN, "./Workspaces//NN.h5")

save(dataTest, history,
     file = "./Workspaces//garchDataTest.Rdata")

write.csv(dataTrain, gzfile("./Workspaces//garchDataTrain.csv.gz"), 
          row.names = FALSE)
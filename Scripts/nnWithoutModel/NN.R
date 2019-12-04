### Packagets ----------------------------------------------------------------
require("keras")
require("tensorflow")
require("lubridate")
require("tidyr")
require("dplyr")
require("ggplot2")

### Tensorflow setup ---------------------------------------------------------
Sys.setenv(WORKON_HOME="/q/student/mnorda16")
install_tensorflow()

### Load data ----------------------------------------------------------------

data <- read.csv("./Data//SPY.csv") %>% 
  filter(Type == "call", K >= 200) %>% 
  mutate(MT = as.numeric(as.Date(Tt) - as.Date(Start)),
         r = 0.0153/91.5*MT) %>% 
  filter(MT <= 30) %>% 
  select(S0, K, r, MT, C = P)

minData <- min(data)
maxData <- max(data)

### Test and training set ----------------------------------------------------

set.seed(123)

indTrain <- sample(nrow(data), nrow(data)*0.75)

dataTrain <- (2*data[indTrain, ] - maxData - minData)/(maxData - minData)
dataTrain <- dataTrain %>% 
  select(-C) %>% 
  as.matrix()
dataTest <- (2*data[-indTrain, ] - maxData - minData)/(maxData - minData)
dataTest <- dataTest %>% 
  select(-C) %>% 
  as.matrix()

dimnames(dataTrain) <- NULL
dimnames(dataTest) <- NULL

dataTrainTarget <- data[indTrain, ] %>% 
  mutate(C = (2*C - maxData - minData)/(maxData - minData)) %>% 
  select(C) %>% 
  as.matrix()

dataTestTarget <- data[-indTrain, ] %>% 
  mutate(C = (2*C - maxData - minData)/(maxData - minData)) %>% 
  select(C) %>% 
  as.matrix()

dimnames(dataTrainTarget) <- NULL
dimnames(dataTestTarget) <- NULL

### Construction the model ---------------------------------------------------

BlackScholesNnDropout <- keras_model_sequential()

BlackScholesNnDropout %>% 
  layer_dense(units = 50, activation = 'elu', 
              input_shape = dim(dataTest)[2]) %>% 
  layer_dense(units = 50, activation = 'elu') %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 50, activation = 'elu') %>% 
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 50, activation = 'elu') %>% 
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1, activation = "linear")

earlyStop <- callback_early_stopping(monitor = "val_loss", patience = 50)

### Compile and fit ----------------------------------------------------------

BlackScholesNnDropout %>% compile(
  loss = 'mse', 
  optimizer = optimizer_rmsprop(lr = 0.001, rho = 0.9), 
  metrics = 'mean_absolute_error'
)

history <- BlackScholesNnDropout %>% 
  fit(
    dataTrain,
    dataTrainTarget,
    epochs = 1000,
    batch_size = 50, 
    validation_split = 0.2,
    verbose = 1,
    callbacks = list(earlyStop)
  )

plot(history)

### Predict ------------------------------------------------------------------

testPredict <- BlackScholesNnDropout %>% 
  predict(dataTest)
trainPredict <- BlackScholesNnDropout %>% 
  predict(dataTrain)

dataTrain <- data[indTrain,] %>% 
  mutate(cHat = (trainPredict*(maxData - minData) + 
                   (maxData + minData))/2)
dataTest <- data[-indTrain,] %>% 
  mutate(cHat = (testPredict*(max(data) - min(data)) + 
                   (maxData + minData))/2)

# Saving model ---------------------------------------------------------------
save_model_hdf5(BlackScholesNnDropout, 
                "./Workspaces//BlackScholesNnDropout.h5")

save(dataTest,
     file = "./Workspaces//BlackScholesNnDataTest.Rdata")
a
write.csv(dataTrain, gzfile("./Workspaces//BlackScholesDataTrain.csv.gz"), 
          row.names = FALSE)

quit("no")
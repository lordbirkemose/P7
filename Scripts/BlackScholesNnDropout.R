### Packagets ----------------------------------------------------------------
require("keras")
require("tensorflow")
require("lubridate")
require("tidyr")
require("dplyr")
require("ggplot2")

### Load data ----------------------------------------------------------------
data <- read.csv("./Data/BlackScholesData.csv.gz")

### Test and training set ----------------------------------------------------
set.seed(123)

indTrain <- sample(nrow(data), nrow(data)*0.75)

dataTrain <- data[indTrain, ] %>% 
  select(-C) %>% 
  as.matrix()
dataTest <- data[-indTrain, ] %>% 
  select(-C) %>% 
  as.matrix()

dimnames(dataTrain) <- NULL
dimnames(dataTest) <- NULL

dataTrainTarget <- data[indTrain, ] %>% 
  select(C) %>% 
  as.matrix()

dataTestTarget <- data[-indTrain, ] %>% 
  select(C) %>% 
  as.matrix()

dimnames(dataTrainTarget) <- NULL
dimnames(dataTestTarget) <- NULL

### Construction the model ---------------------------------------------------

BlackScholesNnDropout <- keras_model_sequential()

BlackScholesNnDropout %>% 
  layer_dense(units = 30, activation = 'elu', 
              input_shape = dim(dataTest)[2]) %>% 
  layer_dense(units = 30, activation = 'elu') %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 30, activation = 'elu') %>% 
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 30, activation = 'elu') %>% 
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1)

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
    batch_size = 500, 
    validation_split = 0.2,
    verbose = 1,
    callbacks = list(earlyStop)
  )

### Visualize the model training history -------------------------------------

plot(history)

### Predict ------------------------------------------------------------------

testPredict <- BlackScholesNnDropout %>% 
  predict(dataTest)
trainPredict <- BlackScholesNnDropout %>% 
  predict(dataTrain)

testResidual <- dataTestTarget - testPredict
trainResidual <- dataTrainTarget - trainPredict
plot(testResidual)

# Saving model ---------------------------------------------------------------
save_model_hdf5(BlackScholesNnDropout, 
                "./Workspaces//BlackScholesNnDropout.h5")

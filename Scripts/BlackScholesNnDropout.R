### Packagets ----------------------------------------------------------------
require("keras")
require("tensorflow")
require("lubridate")
require("tidyr")
require("dplyr")
require("ggplot2")

### Data simulation  ---------------------------------------------------------
S0 <- seq(305, 308, by = 1) # Current instrument price
K <- seq(200, 350, by = 2) # Strike price
MT <- seq(1, 10, by = 1) # Time to maturity
# r <- seq(0, 2.5, by = 0.3) # Risk free rate
r <- 0.0153*MT/91.5
sigma <- seq(1, 10, by = 0.5) # Volatility of the instrument

variableGrid <- expand.grid(S0 = S0, r = r, sigma = sigma, K = K, MT = MT)

BlackScholesFun <- function(S0, K, r, MT, sigma) {
  d1 <- (log(S0/K) + (r + sigma^2/2)*MT)/(sigma*sqrt(MT))
  d2 <- d1 - sigma*sqrt(MT)
  
  C <- pnorm(d1)*S0 - pnorm(d2)*K*exp(-r*MT)
  
  return(C)
}

C <- do.call(mapply, c(BlackScholesFun, unname(variableGrid)))

BlackScholesData <- variableGrid %>% 
  mutate(C = C)

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
  layer_dense(units = 1, activation = "linear")

earlyStop <- callback_early_stopping(monitor = "val_loss", patience = 50)

### Compile and fit ----------------------------------------------------------

BlackScholesNnDropout %>% compile(
  loss = 'mse', 
  optimizer = optimizer_rmsprop(lr = 0.001, rho = 0.9), 
  metrics = 'mean_absolute_error'
)

BlackScholesNnDropout %>% 
  fit(
    dataTrain,
    dataTrainTarget,
    epochs = 200,
    batch_size = 50, 
    validation_split = 0.2,
    verbose = 1,
    callbacks = list(earlyStop)
  )

### Predict ------------------------------------------------------------------

testPredict <- BlackScholesNnDropout %>% 
  predict(dataTest)
trainPredict <- BlackScholesNnDropout %>% 
  predict(dataTrain)

dataTrain <- data[indTrain] %>% 
  mutate(cHat = testPredict)
dataTest <- data[-indTrain] %>% 
  mutate(cHat = trainPredict)

# Saving model ---------------------------------------------------------------
save_model_hdf5(BlackScholesNnDropout, 
                "./Workspaces//BlackScholesNnDropout.h5")

save(dataTrain, dataTest,
     file = "./Workspaces//BlackScholesNnData.Rdata")
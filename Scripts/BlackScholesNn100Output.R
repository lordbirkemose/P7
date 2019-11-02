### Packagets ----------------------------------------------------------------
require("keras")
require("tensorflow")
require("lubridate")
require("tidyr")
require("dplyr")
require("ggplot2")

### Make data ----------------------------------------------------------------
S0 <- seq(1, 10, by = 0.5) # Current instrument price
K <- seq(1, 10, by = 1) # Strike price
r <- seq(0, 2.5, by = 0.3) # Risk free rate
MT <- seq(1, 10, by = 1) # Time to maturity
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

for (k in K) {
  for (t in MT) {
    nam <- paste0("C_",k,t)
    dat <- BlackScholesData %>% 
      filter(K == k, MT == t) %>% 
      select(-K, -MT) %>% 
      rename(!!nam := C)
    
    assign(paste0("C_",k,t), dat)
  }
}

for (k in K) {
  for (t in MT) {
    if (k == K[1] & t == MT[1]) {
      data <- left_join(variableGrid, get(paste0("C_",1,1)), 
                        by = c("S0", "r", "sigma"))
    } else data <- left_join(
      data, 
      get(paste0("C_",k,t)), 
      by = c("S0", "r", "sigma"))
  }
}


### Test and training set ----------------------------------------------------
set.seed(123)

indTrain <- sample(nrow(data), 
                   nrow(data)*0.75)

dataTrain <- data[indTrain, ] %>% 
  select(S0, r, sigma) %>% 
  as.matrix()
dataTest <- data[-indTrain, ] %>% 
  select(S0, r, sigma) %>% 
  as.matrix()

dimnames(dataTrain) <- NULL
dimnames(dataTest) <- NULL

dataTrainTarget <- data[indTrain, ] %>% 
  select(-S0, -r, -sigma, -K, -MT) %>% 
  as.matrix()

dataTestTarget <- data[-indTrain, ] %>% 
  select(-S0, -r, -sigma, -K, -MT) %>% 
  as.matrix()

dimnames(dataTrainTarget) <- NULL
dimnames(dataTestTarget) <- NULL

### Construction the model ---------------------------------------------------

BlackScholesNNKeras <- keras_model_sequential()

BlackScholesNNKeras %>% 
  layer_dense(units = 30, activation = 'elu', 
              input_shape = dim(dataTest)[2]) %>% 
  layer_dense(units = 30, activation = 'elu') %>% 
  layer_dense(units = 30, activation = 'elu') %>% 
  layer_dense(units = 30, activation = 'elu') %>% 
  layer_dense(units = dim(dataTrainTarget)[2])

earlyStop <- callback_early_stopping(monitor = "val_loss", patience = 50)

### Compile and fit ----------------------------------------------------------

BlackScholesNNKeras %>% compile(
  loss = 'mse', 
  optimizer = optimizer_rmsprop(lr = 0.001, rho = 0.9), 
  metrics = 'mean_absolute_error'
)

history <- BlackScholesNNKeras %>% 
  fit(
    dataTrain,
    dataTrainTarget,
    epochs = 1000,
    batch_size = 50, 
    validation_split = 0.2,
    verbose = 1,
    callbacks = list(earlyStop)
  )

### Visualize the model training history -------------------------------------

plot(history)

### Predict ------------------------------------------------------------------

testPredict <- BlackScholesNNKeras %>% 
  predict(dataTest)
trainPredict <- BlackScholesNNKeras %>% 
  predict(dataTrain)

testResidual <- dataTestTarget - testPredict
trainResidual <- dataTrainTarget - trainPredict
plot(testResidual[,1])

# Saving model ---------------------------------------------------------------
save_model_hdf5(BlackScholesNNKeras, 
                "./Workspaces//BlackScholesNnMOutputBS50.h5")

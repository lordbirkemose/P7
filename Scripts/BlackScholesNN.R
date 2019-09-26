### Pakker ------------------------------------------------------------------------------
library(tidyr)
library(dplyr)
library(neuralnet)

### Inlæs data --------------------------------------------------------------------------
data <- read.csv("./Data/BlackScholesData.csv.gz")

set.seed(1)
indTrain <- sample(nrow(data), nrow(data)*0.8)
dataTrain <- data[indTrain,]
dataTest <- data[-indTrain,]

### Træn NN -----------------------------------------------------------------------------
startTime <- Sys.time()

nnBlackScholes <- neuralnet(C ~ S0 + K + r + MT + sigma, data = dataTrain, 
                            hidden = c(30,30),
                            threshold = 0.01,
                            stepmax = 1e+6,
                            learningrate.factor = list(minus = 0.5,
                                                       plus = 1.2),
                            lifesign = "minimal")

endTime <- Sys.time()
trainTime <- endTime - startTime

### Prædikterer -------------------------------------------------------------------------
predictTrain <- compute(nnBlackScholes, dataTrain)
predictTest <- compute(nnBlackScholes, dataTest)


dataTrain <- dataTrain %>% 
  mutate(predictedNN = predictTrain[["net.result"]])
dataTest <- dataTest %>% 
  mutate(predictedNN = predictTest[["net.result"]])


### Gemmer model ------------------------------------------------------------------------
save(nnBlackScholes, trainTime,
     file = "./Workspaces/BlackScholesNnWorkspace.Rdata")

quit(save = "no")
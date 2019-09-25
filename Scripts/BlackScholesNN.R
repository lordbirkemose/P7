### Pakker ------------------------------------------------------------------------------
library(tidyr)
library(dplyr)
library(neuralnet)

### Inlæs data --------------------------------------------------------------------------
Data <- read.csv("./Data/BlackScholesData.csv")

set.seed(1)
trainInd <- sample(nrow(Data), nrow(Data)*0.001)
trainData <- Data[trainInd,]
testData <- Data[-trainInd,]

### Træn NN -----------------------------------------------------------------------------
nnBlackScholes <- neuralnet(CHat~S0 + K + r + MT + sigma, data = trainData, 
                            hidden = c(30,30),
                            stepmax = )

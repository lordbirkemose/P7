### Pakker ------------------------------------------------------------------------------
library(tidyr)
library(dplyr)
library(neuralnet)

### Grid --------------------------------------------------------------------------------
S0 <- seq(1, 10, by = 0.5) # Current instrument price
K <- seq(1, 10, by = 1) # Strike price
r <- seq(0, 2.5, by = 0.3) # Risk free rate
MT <- seq(1, 10, by = 1) # Time to maturity
sigma <- seq(1, 10, by = 0.5) # Volatility of the instrument

variableGrid <- expand.grid(S0 = S0, r = r, sigma = sigma, K = K, MT = MT)

### Model -------------------------------------------------------------------------------
BlackScholesFun <- function(S0, K, r, MT, sigma) {
  d1 <- (log(S0/K) + (r + sigma^2/2)*MT)/(sigma*sqrt(MT))
  d2 <- d1 - sigma*sqrt(MT)
  
  C <- pnorm(d1)*S0 - pnorm(d2)*K*exp(-r*MT)
  
  return(C)
}

### Simulering --------------------------------------------------------------------------

C <- do.call(mapply, c(BlackScholesFun, unname(variableGrid)))

BlackScholesData <- variableGrid %>% 
  mutate(C = C)


# write.csv(BlackScholesData, gzfile("./Data//BlackScholesData.csv.gz"), row.names =FALSE)

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
      BlackScholesMultpliOutputData <- left_join(variableGrid, get(paste0("C_",1,1)), 
                                                 by = c("S0", "r", "sigma"))
    } else BlackScholesMultpliOutputData <- left_join(BlackScholesMultpliOutputData, 
                                                      get(paste0("C_",k,t)), 
                                                      by = c("S0", "r", "sigma"))
  }
}

### NN Træning --------------------------------------------------------------------------

set.seed(1)
indTrain <- sample(nrow(BlackScholesMultpliOutputData), 
                   nrow(BlackScholesMultpliOutputData)*0.8)
dataTrain <- BlackScholesMultpliOutputData[indTrain,]
dataTest <- BlackScholesMultpliOutputData[-indTrain,]

startTime <- Sys.time()
BlackScholesNNMultpliOutput <- neuralnet(as.formula(
  C_11 + C_12 + C_13 + C_14 + C_15 + C_16 + C_17 + C_18 + C_19 + C_110 + 
  C_21 + C_22 + C_23 + C_24 + C_25 + C_26 + C_27 + C_28 + C_29 + C_210 + 
  C_31 + C_32 + C_33 + C_34 + C_35 + C_36 + C_37 + C_38 + C_39 + C_310 + 
  C_41 + C_42 + C_43 + C_44 + C_45 + C_46 + C_47 + C_48 + C_49 + C_410 + 
  C_51 + C_52 + C_53 + C_54 + C_55 + C_56 + C_57 + C_58 + C_59 + C_510 + 
  C_61 + C_62 + C_63 + C_64 + C_65 + C_66 + C_67 + C_68 + C_69 + C_610 +
  C_71 + C_72 + C_73 + C_74 + C_75 + C_76 + C_77 + C_78 + C_79 + C_710 +
  C_81 + C_82 + C_83 + C_84 + C_85 + C_86 + C_87 + C_88 + C_89 + C_810 +
  C_91 + C_92 + C_93 + C_94 + C_95 + C_96 + C_97 + C_98 + C_99 + C_910 +
  C_101 + C_102 + C_103 + C_104 + C_105 + C_106 + C_107 + C_108 + C_109 + C_1010 
 ~ S0 + r + sigma), 
                            data = dataTrain, 
                            hidden = c(30, 30, 30, 30),
                            rep = 1,
                            threshold = 0.1,
                            stepmax = 1e+6,
                            learningrate.factor = list(minus = 0.5,
                                                       plus = 1.2),
                            act.fct = "logistic",
                            linear.output = TRUE, #Set F for apply act.fct on output
                            lifesign = "full")
endTime <- Sys.time()
trainTime2 <- endTime - startTime

### Prædikterer -------------------------------------------------------------------------
predictTrain <- compute(BlackScholesNNMultpliOutput, dataTrain)
predictTest <- compute(BlackScholesNNMultpliOutput, dataTest)


dataTrain <- dataTrain %>% 
  mutate(predictedNN = predictTrain[["net.result"]])
dataTest <- dataTest %>% 
  mutate(predictedNN = predictTest[["net.result"]])

### Gemmer model ------------------------------------------------------------------------
save(BlackScholesNNMultpliOutput, trainTime2,
     file = "./Workspaces/BlackScholesMultipleOutputWorkspace.Rdata")

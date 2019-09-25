### Pakker ------------------------------------------------------------------------------
library(tidyr)
library(dplyr)

### Grid --------------------------------------------------------------------------------
S0 <- seq(1, 10, by = 0.5) # Current instrument price
K <- seq(1, 10, by = 0.5) # Strike price
r <- seq(0, 2.5, by = 0.3) # Risk free rate
MT <- seq(1, 10, by = 1) # Time to maturity
sigma <- seq(1, 10, by = 0.5) # Volatility of the instrument

variableGrid <- expand.grid(S0 = S0, K = K, r = r, MT = MT, sigma = sigma)

### Model -------------------------------------------------------------------------------
BlackScholesFun <- function(S0, K, r, MT, sigma) {
  d1 <- (log(S0/K) + (r + sigma^2/2)*MT)/(sigma*sqrt(MT))
  d2 <- d1 - sigma*sqrt(MT)
  
  C <- pnorm(d1)*S0 - pnorm(d2)*K*exp(-r*MT)
  
  return(C)
}

### Simulering --------------------------------------------------------------------------
startTime <- Sys.time()

C <- do.call(mapply, c(BlackScholesFun, unname(variableGrid)))

endTime <- Sys.time()
simTime <- endTime - startTime

BlackScholesData <- variableGrid %>% 
  mutate(C = C)

write.csv(BlackScholesData, gzfile("./Data//BlackScholesData.csv.gz"), row.names = FALSE)

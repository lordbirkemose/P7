### Pakker -------------------------------------------------------------------
library(tidyr)
library(dplyr)

### Grid ---------------------------------------------------------------------
S0 <- seq(305, 309, by = 1) # Current instrument price
K <- seq(200, 350, by = 1) # Strike price
MT <- seq(1, 30, by = 1) # Time to maturity
# r <- seq(0, 2.5, by = 0.3) # Risk free rate
r <- seq(1, 30, by = 2)*0.0153/91.5
sigma <- seq(0.1, 1, by = 0.05) # Volatility of the instrument

variableGrid <- expand.grid(S0 = S0, K = K, r = r, MT = MT, sigma = sigma)

### Model --------------------------------------------------------------------
BlackScholesFun <- function(S0, K, r, MT, sigma) {
  d1 <- (log(S0/K) + (r + sigma^2/2)*MT)/(sigma*sqrt(MT))
  d2 <- d1 - sigma*sqrt(MT)
  
  C <- pnorm(d1)*S0 - pnorm(d2)*K*exp(-r*MT)
  
  return(C)
}

### Simulering ---------------------------------------------------------------

C <- mapply(BlackScholesFun, 
            S0 = variableGrid$S0,
            K = variableGrid$K,
            r = variableGrid$r,
            MT = variableGrid$MT, 
            sigma = variableGrid$sigma)

BlackScholesData <- variableGrid %>% 
  mutate(C = C)

write.csv(BlackScholesData, gzfile("./Data//BlackScholesData.csv.gz"), 
          row.names = FALSE)
### Packages -----------------------------------------------------------------
library(quantmod)
library(lubridate)
library(dplyr)
library(tidyr)

### Fetch function -----------------------------------------------------------

optionDataFetch <- function(RootSymbol, ExpYear) {
  Start <- lubridate::today()
  
  quantmod::getSymbols(RootSymbol, 
                       from = Start, 
                       to = Start, 
                       src =  "yahoo", 
                       adjust =  TRUE)
  S0 <- Cl(get(RootSymbol)) %>% 
    zoo::coredata()
  
  optionChain <- quantmod::getOptionChain(RootSymbol, ExpYear)
  data <- do.call(rbind, lapply(optionChain, 
                                function(x) do.call(rbind, x)))
  data %<>% 
    mutate(Tt = as.Date(substr(row.names(data), 1, 11), "%b.%d.%Y"),
           Type = ifelse(substr(row.names(data), 13, 13) == "c",
                         "call", "put"),
           Start = Start,
           S0 = S0) %>%
    select(Start, S0, K = Strike, Tt, Type, P = Last)

  return(data)
}

### Write data to csv --------------------------------------------------------
symbols <- c("SPY", "BAC", "ATVI", "JNJ", "JPM", "TSLA",
             "FB", "AAPL", "AMZN", "NFLX", "GOOG")
path <- "./Data//"

lapply(1:length(symbols), function(i) {
  optionDataFetch(symbols[i], "2019/2020") %>% 
    write.csv(file = paste0(path, symbols[i], ".csv"), row.names = FALSE)
  }
)
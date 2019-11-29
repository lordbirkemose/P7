### Packages -----------------------------------------------------------------
require("tidyverse")
require("Cairo")
require("magrittr")
require("wesanderson")
require("latex2exp")
require("keras")
require("tensorflow")
install_tensorflow()

### Colors and themes --------------------------------------------------------
# Heatmaps
pal <- wes_palette("Zissou1", 100, type = "continuous")
colors <- c("#007e89", "#fc8d62", "#aec6cf", "#66c2a5", "#779ecc")

# Heatmaps
themeHeatmap <- list(theme(panel.background = element_blank()))

# Others  no legend
theme <- list(theme_minimal(),
              theme(panel.grid.major = element_line(), 
                    panel.grid.minor = element_line(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black")))
themeLegend <- list(theme_minimal(),
                    theme(panel.grid.major = element_line(), 
                          panel.grid.minor = element_line(),
                          panel.background = element_blank(), 
                          axis.line = element_line(colour = "black"),
                          legend.position = "top" , 
                          legend.justification = "left" , 
                          legend.direction = "horizontal", 
                          legend.background = element_blank()))

### Data ---------------------------------------------------------------------
S0 <- seq(305, 309, by = 1) # Current instrument price
K <- seq(200, 350, by = 1) # Strike price
MT <- seq(1, 30, by = 1) # Time to maturity
# r <- seq(0, 2.5, by = 0.3) # Risk free rate
r <- seq(1, 30, by = 2)*0.0153/91.5
sigma <- seq(0.1, 1, by = 0.05) # Volatility of the instrument

variableGrid <- expand.grid(S0 = S0, K = K, r = r, MT = MT, sigma = sigma)

BlackScholesFun <- function(S0, K, r, MT, sigma) {
  d1 <- (log(S0/K) + (r + sigma^2/2)*MT)/(sigma*sqrt(MT))
  d2 <- d1 - sigma*sqrt(MT)
  
  C <- pnorm(d1)*S0 - pnorm(d2)*K*exp(-r*MT)
  
  return(C)
}

C <- mapply(BlackScholesFun, 
            S0 = variableGrid$S0,
            K = variableGrid$K,
            r = variableGrid$r,
            MT = variableGrid$MT, 
            sigma = variableGrid$sigma)

data <- variableGrid %>% 
  mutate(C = C)

load("~/Desktop//P7//LargeDataFromServer//BlackScholesNnDataTest.Rdata")
NN <- load_model_hdf5(paste0("~/Desktop//P7//LargeDataFromServer"
                             ,"//BlackScholesNn.h5"))

dataTest$cHat <- dataTest$cHat[,1]
dataTrain$cHat <- dataTrain$cHat[,1]

### Heatmap ------------------------------------------------------------------
dataPlotHeatmap <- dataTest %>% 
  filter(MT > 2, K < 350) %>% 
  group_by(K, MT) %>% 
  summarise(n = n(),
            diff = sum(abs((C - cHat))/C)*100) %>% 
  mutate(MPE = diff/n) %>% 
  select(K, MT, MPE)

ggplot(data = dataPlotHeatmap, aes(y = MT, x = K, fill = MPE)) +
  geom_tile() +
  labs(title  = "",
       x = "Strike",
       y = "Maturity") + 
  scale_fill_gradientn(colours = pal) +
  themeHeatmap

ggsave(file = paste0("./Plots/","heatmap",".eps"), 
       width =  9, height = 3 , device = cairo_ps , dpi = 600)

### Surface plot -------------------------------------------------------------
dataPlotBS <- data %>% 
  group_by(K) %>% 
  select(K, sigma, C)

plot_ly(data = dataPlotBS, x = dataPlotBS$K, y = dataPlotBS$C, 
        z = dataPlotBS$sigma, type = "mesh3d") %>% 
  layout(title = "Black and Scholes",
         scene = list(
           xaxis = list(title = "Strike"),
           yaxis = list(title = "Call Price"),
           zaxis = list(title = "Sigma")
         ))


dataPlotNn <- dataTest %>% 
  group_by(K) %>% 
  select(K, sigma, C)

plot_ly(data = dataPlotNn, x = dataPlotNn$K, y = dataPlotNn$C, 
        z = dataPlotNn$sigma, type = "mesh3d") %>% 
  layout(title = "Neural Network (Training set)",
         scene = list(
           xaxis = list(title = "Strike"),
           yaxis = list(title = "Call Price"),
           zaxis = list(title = "Sigma")
         ))

### Line plot ----------------------------------------------------------------
dataLine <- dataTest %>% 
  rbind(dataTrain) %>% 
  filter(K == 250, MT == 10, S0 == 305, 
         abs(r - 0.0048491803) < 0.000001) %>% 
  select(sigma, C, cHat)
  
ggplot(data = dataLine) +
  geom_line(aes(x = sigma, y = C, colour = "Black & Scholes")) +
  geom_line(aes(x = sigma, y = cHat, colour = "Neural Network")) +
  xlab(TeX("$\\sigma$")) +
  ylab("Call price") +
  scale_colour_manual("", values = c("Black & Scholes" = colors[1], 
                                     "Neural Network" = colors[2])) +
  themeLegend

ggsave(file = paste0("./Plots/","volatility",".eps"), 
       width =  9/2, height = 3.5 , device = cairo_ps , dpi = 600)

### BS vs NN -----------------------------------------------------------------
set.seed(1)
dataCompare <- dataTest[sample(nrow(dataTest), nrow(dataTest)*0.1),]
axisMax <- range(dataCompare$C, dataCompare$cHat)[2]

ggplot(data = dataCompare) + 
  geom_point(aes(x = C, y = cHat), color = colors[3]) +
  geom_abline(slope = 1, intercept = 0) +
  xlab("Black & Scholes") +
  ylab("Neural Network") +
  scale_x_continuous(limits = c(0, axisMax)) + 
  scale_y_continuous(limits = c(0, axisMax)) +
  theme

ggsave(file = paste0("./Plots/","compare",".eps"), 
       width =  9/2, height = 3 , device = cairo_ps , dpi = 600)




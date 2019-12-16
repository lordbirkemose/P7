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

savePlots <- FALSE # Set TRUE if plots should be saved
### Data ---------------------------------------------------------------------
data <- read.csv("./Data/garchMC.csv.gz")

path <- "./Data/garchNn500Epoch500Batch30NeuronsFiltered/"
load(paste0(path,"DataTest.Rdata"))
dataTrain <- read.csv(paste0(path, "DataTrain.csv.gz"))
NN <- load_model_hdf5(paste0(path, "NN.h5"))

### Heatmap ------------------------------------------------------------------
dataPlotHeatmap <- dataTest %>% 
  filter(MT > 2, K < 350) %>% 
  group_by(K, MT) %>% 
  summarise(n = n(),
            diff = sum(abs(C - cHat)/C)*100) %>% 
  mutate(MAPE = diff/n) %>% 
  select(K, MT, MAPE)

ggplot(data = dataPlotHeatmap, aes(y = MT, x = K, fill = MAPE)) +
  geom_tile() +
  labs(title  = "",
       x = "Strike",
       y = "Maturity") + 
  scale_fill_gradientn(colours = pal) +
  themeHeatmap

if (savePlots == TRUE) {
  ggsave(file = paste0("./Plots/","heatmapGarch",".eps"), 
         width =  9/2, height = 3.4 , device = cairo_ps , dpi = 600)
}


### MC vs NN -----------------------------------------------------------------
set.seed(1)
dataCompare <- dataTest[sample(nrow(dataTest), nrow(dataTest)*0.2),]
axisMax <- range(dataCompare$C, dataCompare$cHat)[2]

ggplot(data = dataCompare) + 
  geom_point(aes(x = C, y = cHat), color = colors[3]) +
  geom_abline(slope = 1, intercept = 0) +
  xlab("Monte Carlo") +
  ylab("Neural network") +
  scale_x_continuous(limits = c(0, axisMax)) + 
  scale_y_continuous(limits = c(0, axisMax)) +
  theme

if(savePlots == TRUE) {
  ggsave(file = paste0("./Plots/","compareGarch",".eps"), 
         width =  9/2, height = 3 , device = cairo_ps , dpi = 600)
}

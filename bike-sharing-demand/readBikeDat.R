

library(tidyverse)
library(vroom)
library(patchwork)
library(DataExplorer)

bikeData = vroom("train.csv")


glimpse(bikeData)
plot_bar(bikeData)

# outliers/shape
histograms = DataExplorer::plot_histogram(bikeData)

# missing data
intro_plot = plot_intro(bikeData)

# collinearity
corrPlot = plot_correlation(bikeData)

# strong vs weak relationship
humidityPlot = bikeData %>% ggplot(mapping = aes(x = humidity, y = count)) +
  geom_point(color = "purple") +
  geom_smooth(fill = "green") +
  labs(
    
    title = "Count by humidity"
    
  ) + theme_bw()

windPlot = bikeData %>% ggplot(mapping = aes(x = windspeed)) +
  geom_histogram(fill = "black") +
  labs(
    
    title = "Windspeed Histogram"
    
  ) + theme_bw()

(humidityPlot+ corrPlot) / (windPlot + intro_plot)


(humidityPlot+ corrPlot) / (histograms + intro_plot)







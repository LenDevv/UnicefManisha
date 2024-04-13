install.packages("tidyverse")
install.packages("quarto")
install.packages("ggplot2")
# Load data
indicator_1 <- read.csv("unicef_indicator_1.csv")
indicator_2 <- read.csv("unicef_indicator_2.csv")
metadata <- read.csv("unicef_metadata.csv")
# Merge datasets 
merged_data <- merge(indicator_1, indicator_2, by = "country")
library(ggplot2)
library(tidyverse)
library(ggplot2)

# Visualization 1: World Map Chart
world_map_data <- map_data("world")
# Example code:
world_map <- ggplot() +
  geom_map(data = world_map_data, map = world_map_data, aes(map_id = region), fill = "white", color = "black") +
  theme_void()
print(world_map)
# Visualization 2: Bar Chart
bar_chart <- ggplot(data = indicator_1) +
  geom_bar(aes(x = country, y = obs_value), stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
print(bar_chart)

# Visualization 3: Scatterplot with Linear Regression Line
scatterplot <- ggplot(data = indicator_2) +
  geom_point(aes(x = time_period, y = obs_value), alpha = 0.5) +
  geom_smooth(aes(x = time_period, y = obs_value), method = "lm", se = FALSE)
print(scatterplot)

# Visualization 4: Time-series Chart
time_series <- ggplot(data = metadata, aes(x = year, y = Population..total)) +
  geom_line() +
  labs(y = "Population, total", x = "Year") +
  ggtitle("Time Series of Total Population")
print(time_series)




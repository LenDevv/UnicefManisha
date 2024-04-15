install.packages("tidyverse")
install.packages("maps") 

# Load data
indicator_1 <- read.csv("unicef_indicator_1.csv")
indicator_2 <- read.csv("unicef_indicator_2.csv")
metadata <- read.csv("unicef_metadata.csv")
# Merge datasets 
merged_data <- merge(indicator_1, indicator_2, by = "country")

library(tidyverse)

options(scipen=999)

# Visualization 1: World Map Chart - Population
world_map_data <- map_data("world")

ind_data_2018 <- metadata %>%
  filter(year == 2018) %>%
  mutate(region = ifelse(country == "United States", "USA", country))

map_data_2018 <- full_join(world_map_data, ind_data_2018)

ggplot(map_data_2018) + 
  aes(long, lat, group = group, fill = Life.expectancy.at.birth..total..years.) + 
  geom_polygon() +
  scale_fill_gradient(low = "darkred", high = "mistyrose", na.value = "grey") +
  theme_bw() + 
  labs(
    title = "Life Expectancy at Birth across the world in 2018",
    x = "Countries",
    y = "Life Expectamcy",
    fill = "Life Expectancy"
  )


# 
# ggplot(map_data_2018) +
#   aes(long, lat, group = group, fill = numeric_code +
#   geom_polygon() +
#   scale_fill_gradient(low = "darkred", high = "mistyrose", na.value = "grey") +
#   theme_void() +
#   labs(
#     title = "Percentage children suffering at least four deprivation. Homogeneous moderate standards",
#     x = "Countries",
#     y = "Percetage of children",
#     fill = "som"
#     )



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




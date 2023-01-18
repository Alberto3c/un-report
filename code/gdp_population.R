# Run Life expectancy and CO2 emissions versus population with gapminder
# Date: 1-17-2023
# Author: Alberto Correa III

# Load in packages necessary for analysis
library("tidyverse")
library(readr)

# Read in data for analysis
gapminder_1997 <- read_csv("gapminder_1997.csv")

# Plotting data for visualization
ggplot(data = gapminder_1997) + 
  aes(x = gdpPercap, y = lifeExp, color = continent, size = pop/1000000, shape = continent) +
  labs(x = "GDP Per Capita", y = "Life Expectancy (yrs)", title = "Do people in wealthy countries live longer?", size = "Population (in millions)") +
  geom_point() +
  scale_color_brewer(palette = "Set1")

# Read in all gapminder data years beyond 1997
gapminder_data <- read_csv("gapminder_data.csv")
view(gapminder_data)

# Plotting Life Expectancy as a Function of Year Data was Recorded
ggplot(data = gapminder_data) +
  aes(x=year, y=lifeExp, color=continent, group = country) +
  geom_line()

# Investigating the structure (str) of gapminder data
str(gapminder_data)

#Creating Box Plots/violin plots/jittering points of the 1997 Data
?geom_boxplot
ggplot(data = gapminder_data) +
  aes(x = continent, y = lifeExp, color = continent) +
  geom_violin() +
  geom_point(aes(size=pop))

# Generating histograms of data
ggplot(gapminder_1997)+
  aes(x = lifeExp) +
  geom_histogram(bins = 10) +
  theme_prism()

# Instaling Prisim Themes
install.packages("ggprism")
library(ggprism)
?ggprism

# Using Facets function to generate multiple graphs for comparison
# This seems like it could be really useful for genome replication profiling
ggplot(gapminder_1997) +
  aes(x = gdpPercap, y = lifeExp) + 
  geom_point() +
  facet_grid(vars(continent))

# Saving plots to export as jpg/tiffs
ggsave("awesome_plot.jpg", width = 6, height = 4)

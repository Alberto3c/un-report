library(tidyverse)
getwd()
gapminder_data <- read_csv("data/gapminder_data.csv")

# Summarize data from a column within your data
summarize (gapminder_data, averageLife=mean(lifeExp),medianLifeExp=median(lifeExp))

#Learning to Pipe - this is useful for writing more readable code
# piping %>% moves data from a particular table into another function 
gapminder_data%>%
  summarize(averageLifeExp=mean(lifeExp))

# Filtering
average_life_expectancy_gapminder_2007 <- gapminder_data%>%
  filter(year == 2007)%>%
  summarize(average = mean(lifeExp))
  
# Challange to find average GDP per capita for the first year in the data set
Average_GDP_of_earliest_year <- gapminder_data%>%
  filter(year == 1952)%>%
  summarize(averageGDP = mean(gdpPercap))

#Using group-by() to group variables within a dataframe according to a certain
# feature you are interested in
gapminder_data%>%
  group_by(year, continent)
  summarize(average = mean(lifeExp),
            error = sd(lifeExp))

# Using mutate
gapminder_data%>%
  mutate(gdp = pop*gdpPercap)

# Mutate a new column which is population in millions
gapminder_data%>%
  mutate('Population in millions' = pop/1000000)

#Select function selectively include columns from a data frame and write them 
# to a new data frame
gapminder_data%>%
    select(pop, year)

#Select can also be used to remove a column from a dataframe, in the example
# below we are removing the continent column
gapminder_data%>%
  select(-continent)

#We can do the same thing to remove multiple columns by separating column names
# by commas 
gapminder_data%>%
  select(-continent, -pop)

# Changing shape of data, wide from data vs. long form data, using pivot wider
gapminder_data%>%
  select(country, continent, year, lifeExp)%>%
  pivot_wider(names_from = year, values_from = lifeExp )

# Working with messy data, read in the UN C02 data from the CSV file
read_csv("co2-un-data.csv")

# We can use the skip function in order to skip the first few rows in the csv
read_csv("co2-un-data.csv", 
         skip = 2, 
         col_names = c("region","country","year", "series", "value", "footnotes", "source"))

#Once we have figured out which columns we want from the data frame we should
# save it as an object so that we can work with it further
dirty_c02_emissions <- read_csv("co2-un-data.csv", 
                                 skip = 2, 
                                 col_names = c("region","country","year", "series", "value", "footnotes", "source"))

#We now have an object that is the proper dataset formatted how we want it,
# so we can call the object to make sure that it looks like what we want. This
# will just print it to the the console but we can use the View() function to
# pull the data up into a seperate window in the source.
dirty_c02_emissions

#Now we can alter the data from the dirty_c02_emissions object to rename certain
# series of data as well as well as only inclusing data from the series we want.
dirty_c02_emissions %>% 
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions"))

#Than we can reorganize the data so that we seperate out the total emissions and
# per capita emissions. 
dirty_c02_emissions %>% 
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions"))%>%
  pivot_wider(names_from = series, values_from = value)

#Challange question: Filter out only the data from the year 2005 and remove the
# year column since it is irrelavent, and store it as an object with a good name
c02_emissions_2005 <- dirty_c02_emissions %>% 
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions"))%>%
  pivot_wider(names_from = series, values_from = value)%>%
  filter(year == 2005)%>%
  select(-year)
  
#WE CAN USE THE REMOVE(OBJECT) FUNCTION IN ORDER TO REMOVE OBJECTS FROM THE
# GLOBAL ENVIROENMNET, THE BROOM STICK DOES THE SAME BUT IT WILL DELETE ALL!.
# CLICKING THE DROPDOWN WITH LIST AND SWAPPING TO GRID LETS YOU CLICK SPECIFIC 
# OBJECTS THAT CAN THAN BE THEN BE DELETED WITH THE BROOM ICON.

#Bringing in 2007 population data
gapminder_data_2007 <- gapminder_data%>%
  filter(year ==2007)%>%
  select(country, pop, lifeExp, gdpPercap)

#Inner join?
inner_join(gapminder_data, c02_emissions_2005)

#Anti-join?
anti_join(gapminder_data, c02_emissions_2005, by="country")

#Full-join
full_join(c02_emissions_2005, gapminder_data_2007)%>% View()

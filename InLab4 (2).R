install.packages("pacman")
library(pacman)
p_load(tidyverse, gapminder)
data("gapminder")

data_df = us_rent_income
data_df

data_long=data_df[c("NAME","variable","estimate")]


data_wide = pivot_wider(data_long,names_from = variable , values_from = estimate)
data_wide

state_long= pivot_longer(data_wide, !NAME, names_to = "income", values_to = "count")


####### Examining data ##########
str(gapminder)
glimpse(gapminder)

####### filtering basics #######
gapminder %>%
  filter(year == 2007)

gapminder %>%
  filter(country == "Jordan")

gapminder %>%
  filter(year %in% c(2002,2007), country == "Jordan")

gapminder %>%
  filter(year %in% c(2002,2007) & country == "Jordan")

###### Arranging data ##########
gapminder %>%
  arrange(gdpPercap)

gapminder %>%
  arrange(desc(gdpPercap))

gapminder %>%
  filter(year == 2007) %>%
  arrange(desc(gdpPercap))

gapminder %>%
  mutate(pop = round(pop/1000000,1))

gapminder %>%
  mutate(gdp = gdpPercap * pop)

gapminder %>%
  mutate(gdp = gdpPercap * pop) %>%
  filter(year == 2007) %>%
  arrange(desc(gdp))


########## dplyr #####################
gapminder %>%
  filter(year == 2007) %>%
  summarise(meanLifeExp = mean(lifeExp))

gapminder %>%
  filter(year == 1982) %>%
  summarise(meanLifeExp = mean(lifeExp),
            totalPop = sum(pop),
            numCountries = n())

gapminder %>%
  group_by(year) %>%
  summarize(meanLifeExp = mean(lifeExp),
            totalPop = sum(pop))
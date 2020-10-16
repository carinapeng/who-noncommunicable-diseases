
# Load libraries
library(readxl)
library(magrittr)
library(janitor)
library(ggplot2)
library(dplyr)

# Read data
gbd <- read_excel("/Users/carinapeng/PAHO : WHO/who-noncommunicable-diseases/data/Input tables for Carina_6Oct2020.xlsx", sheet = 3)
pop <- read_excel("/Users/carinapeng/PAHO : WHO/who-noncommunicable-diseases/data/Input tables for Carina_6Oct2020.xlsx", sheet = 4)

# Clean data
gbd_country <- gbd %>% 
  clean_names() %>%
  filter(country == "Afghanistan", sex == "Male")

pop_country <- pop %>% 
  clean_names() %>%
  filter(country == "Afghanistan" & sex == "Male") %>%
  select(upper_age, value)

# Join GBD and Population data

joined <- inner_join(pop_country, gbd_country) %>%
  mutate(people = value * percentage_of_population)


# Plot a stacked density plot

plot_prev <- joined %>% 
  ggplot(aes(x=upper_age, y=percentage_of_population, fill=condition)) +
  geom_area( )

plot_pop <- joined %>%
  ggplot(aes(x=upper_age, y=people, fill=condition)) +
  geom_area( )






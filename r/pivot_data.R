
library(readxl)
library(magrittr)
library(janitor)
library(ggplot2)
library(dplyr)
library(viridis)
library(hrbrthemes)
library(gcookbook)
library(tidyverse)

pop1 <- read_excel("/Users/carinapeng/PAHO : WHO/who-noncommunicable-diseases/data/Input tables for Carina_6Oct2020.xlsx", sheet = 1)


pop1_pivot <- pop1 %>%
  pivot_longer(cols = !c(1, 2, 3, 4, 5),
  #pivot_longer(cols = !c(indicator, sex, lower_age, upper_age, gbd_location_name),
               names_to = "country",
               values_to = "value") %>%
  clean_names()

gbd1 <- read_excel("/Users/carinapeng/PAHO : WHO/who-noncommunicable-diseases/data/Input tables for Carina_6Oct2020.xlsx", sheet = 2)

gbd1_pivot <- gbd1 %>%
  pivot_longer(cols = !c(1:5),
               names_to = "country",
               values_to = "percentage_of_population") %>%
  clean_names()

afg <- gbd1_pivot %>%
  filter(country == "Afghanistan")

# --------------

joined <- readRDS("./app/data/joined.rds")

x <- joined %>%
  filter(country == "Argentina", sex == "Both") %>%
  mutate(prev_perc = round((prev*100),2)) %>%
  mutate(first_step = (1-prev)) %>%
  group_by(age) %>%
  # Create product of first step values
  mutate(prod = prod(first_step)) %>%
  # Create final value by subtracting the product value from 1
  mutate(final_value = (1-prod)) %>%
  mutate(risk_pop = final_value * pop_total) %>%
  mutate(risk_prev = round((final_value * 100),2)) %>%
  mutate(risk_condition = "Increased Risk")


y <- x %>%
  filter(condition == "Cardiovascular diseases") 

risk_value <- sum(y$risk_pop) / sum(y$pop_total)
print(risk_value)






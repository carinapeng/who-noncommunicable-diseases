
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






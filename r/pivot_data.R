
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

pop1_clean <- pop1 %>% 
  clean_names()

pop1_pivot <- pop1_clean %>%
  pivot_longer(cols = !c(indicator, sex, lower_age, upper_age, gbd_location_name),
               names_to = "country",
               values_to = "value")

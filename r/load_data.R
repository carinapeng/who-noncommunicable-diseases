

# Load packages
library(tidyr)
library(readxl)
library(magrittr)
library(janitor)
library(dplyr)


# Load data
gbd_raw <- read_excel("/Users/carinapeng/PAHO : WHO/who-noncommunicable-diseases/data/Input tables for Carina_6Oct2020.xlsx", sheet = 2) %>%
  pivot_longer(cols = !c(1:5),
               names_to = "country",
               values_to = "percentage_of_population") %>%
  clean_names()

pop_raw <- read_excel("/Users/carinapeng/PAHO : WHO/who-noncommunicable-diseases/data/Input tables for Carina_6Oct2020.xlsx", sheet = 1) %>%
  pivot_longer(cols = !c(1, 2, 3, 4, 5),
               names_to = "country",
               values_to = "value") %>%
  clean_names()

joined_raw <- inner_join(gbd_raw, pop_raw) %>%
  mutate(people = value * percentage_of_population,
         perc = percentage_of_population * 100) %>%
  rename(age = gbd_location_name)

# Order age group
joined_raw$age <- as.factor(joined_raw$age)
joined_raw$age <- factor(joined_raw$age, levels=unique(joined_raw$age), ordered=TRUE)

saveRDS(gbd_raw, file = "gbd.rds")
saveRDS(pop_raw, file = "pop.rds")
saveRDS(joined_raw, file = "joined.rds")




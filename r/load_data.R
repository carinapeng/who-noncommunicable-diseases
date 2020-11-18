

# Load packages
library(tidyr)
library(readxl)
library(magrittr)
library(janitor)
library(dplyr)


# Load data
# Read GBD data, UN population data, and the joined dataset of the two
gbd_raw <- read_excel("/Users/carinapeng/PAHO : WHO/who-noncommunicable-diseases/data/Input tables for Carina_6Oct2020.xlsx", sheet = 2) %>%
  pivot_longer(cols = !c(1:5),
               names_to = "country",
               values_to = "prev") %>%
  clean_names()

# UNPOP (2019 revision) population size for the year 2020 by country, age and sex
pop_raw <- read_excel("/Users/carinapeng/PAHO : WHO/who-noncommunicable-diseases/data/Input tables for Carina_6Oct2020.xlsx", sheet = 1) %>%
  pivot_longer(cols = !c(1, 2, 3, 4, 5),
               names_to = "country",
               values_to = "pop_total") %>%
  clean_names()

# Join GBD and UN Population data
joined_raw <- inner_join(gbd_raw, pop_raw) %>%
  # Calculate the population with underlying conditions by multiplying population of age group with prevalence of condition
  mutate(pop_condition = pop_total * prev) %>%
  rename(age = gbd_location_name) %>%
  select(condition, sex, age, country, prev, pop_total, pop_condition)

# Order age group
joined_raw$age <- as.factor(joined_raw$age)
joined_raw$age <- factor(joined_raw$age, levels=unique(joined_raw$age), ordered=TRUE)

saveRDS(gbd_raw, file = "gbd.rds")
saveRDS(pop_raw, file = "pop.rds")
saveRDS(joined_raw, file = "joined.rds")




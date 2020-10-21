
# Load libraries
library(readxl)
library(magrittr)
library(janitor)
library(ggplot2)
library(dplyr)
library(viridis)
library(hrbrthemes)
library(gcookbook)
library(tidyverse)

# Read data
gbd0 <- read_excel("/Users/carinapeng/PAHO : WHO/who-noncommunicable-diseases/data/Input tables for Carina_6Oct2020.xlsx", sheet = 3)
pop0 <- read_excel("/Users/carinapeng/PAHO : WHO/who-noncommunicable-diseases/data/Input tables for Carina_6Oct2020.xlsx", sheet = 4)

# Clean data
gbd_country0 <- gbd0 %>% 
  clean_names() %>%
  filter(country == "Afghanistan", sex == "Male")

pop_country0 <- pop0 %>% 
  clean_names() %>%
  filter(country == "Afghanistan" & sex == "Male") %>%
  select(upper_age, value)

# Join GBD and Population data

joined0 <- inner_join(pop_country0, gbd_country0) %>%
  mutate(people = value * percentage_of_population,
         perc = percentage_of_population * 100)


# Plots

plot_prev0 <- joined0 %>% 
  ggplot(aes(x=upper_age, y=perc, color=condition)) +
  geom_line(size=2, alpha=1) +
  geom_point(color = "black") +
  scale_color_viridis(discrete=TRUE, option = "magma") +
  theme_minimal() +
  labs(color = "Conditions",
       title = "Prevalence of underlying conditions by age") +
  ylab("Percentage of Population") +
  xlab("Age (years)") 

plot_pop0 <- joined0 %>%
  ggplot(aes(x=upper_age, y=people, fill=condition)) +
  geom_area(alpha=0.6 , size=1, colour="black") +
  scale_fill_viridis(discrete = TRUE, option = "magma") +
  theme_minimal() +
  labs(fill = "Conditions",
       title="Population (millions) with underlying conditions by age",
       x = "Age (years)",
       y = "Population (millions)")

facet0 <- joined0 %>%
  ggplot(aes(x=upper_age, y=perc, color=condition)) +
  geom_line(size=2, alpha=1) +
  scale_color_viridis(discrete=TRUE, option = "magma") + 
  theme_minimal() +
  labs(color = "Conditions",
       title = "Prevalence of underlying conditions by age") +
  ylab("Percentage of Population") +
  xlab("Age (years)") +
  facet_wrap(~condition, scales="free")








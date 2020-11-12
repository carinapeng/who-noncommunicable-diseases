
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

# Filter data - Afghanistan male for example
pop_country0 <- pop0 %>% 
  clean_names() %>%
  filter(country == "Afghanistan" & sex == "Male") %>%
  select(upper_age, value)

# Join GBD and Population data
joined0 <- inner_join(pop_country0, gbd_country0) %>%
  mutate(people = value * percentage_of_population,
         perc = percentage_of_population * 100) %>%
  rename(age = gbd_location_name)

# Add formula for population at increased risk 

# 1- ((1 - filtered$percentage_of_population[1]) * 
#       (1 - filtered$percentage_of_population[2]) *
#       (1 - filtered$percentage_of_population[3]) *
#       (1 - filtered$percentage_of_population[4]) *
#       (1 - filtered$percentage_of_population[5]) *
#       (1 - filtered$percentage_of_population[6]) *
#       (1 - filtered$percentage_of_population[7]) *
#       (1 - filtered$percentage_of_population[8]) *
#       (1 - filtered$percentage_of_population[9]) *
#       (1 - filtered$percentage_of_population[10]) *
#       (1 - filtered$percentage_of_population[11]))

increase_risk0 <- joined0 %>%
  mutate(first = (1-percentage_of_population)) %>%
  group_by(lower_age) %>%
  summarise(prod = prod(first)) %>%
  mutate(final = (1-prod))

# Plot for increased risk population
increase_risk0 %>%
  ggplot(aes(x = lower_age, y = final)) +
  geom_point() +
  geom_line()

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


# Pyramid Plot
library(XML)
library(reshape2)
library(ggplot2)
library(plyr)

gbd %>%
  filter(country == "Argentina") %>%
  filter(sex != "Both") %>%
  ggplot(aes(x = percentage_of_population, y = sex, fill = condition)) + 
  geom_bar(subset = .(condition == "Cardiovascular diseases"), stat = "identity") + 
  geom_bar(subset = .(condition == "Chronic kidney diseases"), stat = "identity") + 
  # scale_y_continuous(breaks = seq(-15000000, 15000000, 5000000), 
  #                    labels = paste0(as.character(c(seq(15, 0, -5), seq(5, 15, 5))), "m")) + 
  coord_flip() + 
  #scale_fill_brewer(palette = "Set1") + 
  theme_bw()








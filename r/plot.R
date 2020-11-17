
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

joined0 <- joined0 %>%
  mutate(first = (1-percentage_of_population)) %>%
  group_by(lower_age) %>%
  mutate(prod = prod(first)) %>%
  mutate(final = (1-prod)) %>%
  mutate(risk_pop = final * value)

# Plot for increased risk population
plot_risk_pop <- joined0 %>%
  ggplot(aes(x = lower_age, y = risk_pop)) +
  geom_point() +
  geom_line()

plot_risk_prev <- joined0 %>%
  ggplot(aes(x = lower_age, y = final)) +
  geom_point() +
  geom_line()

# Plots
plot_prev0 <- joined0 %>% 
  ggplot() +
  geom_line(aes(x=upper_age, y=perc, color=condition), size=2, alpha=1) +
  geom_point(aes(x=upper_age, y=perc, color=condition), color = "black") +
  geom_line(aes(x=upper_age, y=(final*100)), size=2, alpha=1, color = "orange") +
  scale_color_viridis(discrete=TRUE, option = "magma") +
  theme_minimal() +
  labs(color = "Conditions",
       title = "Prevalence of underlying conditions by age") +
  ylab("Percentage of Population") +
  xlab("Age (years)") 

plot_risk_pop0 <- joined0 %>%
  ggplot() +
  geom_point(aes(x = upper_age, y = value), color = "black") +
  geom_line(aes(x = upper_age, y = value), size=1, alpha=1) +
  geom_line(aes(x = upper_age, y = risk_pop), color = "orange", size=1, alpha=1) +
  theme_minimal() +
  labs(title = "Population at increased risk of severe COVID-19 disease by age group") +
  ylab("Population") +
  xlab("Age (years)")

plot_pop0 <- joined0 %>%
  ggplot() +
  geom_area(aes(x=upper_age, y=people, fill=condition), alpha=0.6 , size=1, colour="black") +
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

# Load data
gbd <- read_excel("/Users/carinapeng/PAHO : WHO/who-noncommunicable-diseases/data/Input tables for Carina_6Oct2020.xlsx", sheet = 2) %>%
  pivot_longer(cols = !c(1:5),
               names_to = "country",
               values_to = "percentage_of_population") %>%
  clean_names()

pop <- read_excel("/Users/carinapeng/PAHO : WHO/who-noncommunicable-diseases/data/Input tables for Carina_6Oct2020.xlsx", sheet = 1) %>%
  pivot_longer(cols = !c(1, 2, 3, 4, 5),
               names_to = "country",
               values_to = "value") %>%
  clean_names()

# Join population and GBD data
joined <- inner_join(gbd, pop) %>%
  mutate(people = value * percentage_of_population,
         perc = percentage_of_population * 100) %>%
  rename(age = gbd_location_name)

# Order age group
joined$age <- as.factor(joined$age)
joined$age <- factor(joined$age, levels=unique(joined$age), ordered=TRUE)

# Data for pyramid plot (prevalence)
pyramid_data0 <- joined %>%
  filter(condition == "Cardiovascular diseases" & country == "Afghanistan" & sex != "Both") %>%
  mutate(popPerc = case_when(sex == "Male" ~round(percentage_of_population*100,2),
                             TRUE ~-round(percentage_of_population*100,2)),
    signal = case_when(sex == "Male" ~1,
                            TRUE~-1))

# Produce prevalence pyramid plot
pyramid_plot1 <- pyramid_data0 %>%
  ggplot() +
  geom_bar(aes(x=popPerc, y=age, fill=sex), stat = "identity") +
  scale_fill_manual(name="", values=c("#F2BC94", "#104c91")) +
  scale_x_continuous(breaks = seq(-50, 50, 25),
                     labels = function(x)paste(x, "%")) +
  labs(x="Prevalence (%)", y="Age Group", title="Cardiovascular Diseases Prevalece Pyramid of Afghanistan",
       subtitle=paste("Total population with cardiovascular diseases:", format(sum(pyramid_data0$people), big.mark = ","))) +
  theme_minimal()

# Data for pyramid plot (total population)
filtered_pop_data <- pop %>%
  filter(country == "Afghanistan" & sex == "Both") 

pyramid_data1 <- joined %>%
  filter(condition == "Cardiovascular diseases" & country == "Afghanistan" & sex != "Both") %>%
  mutate(percTotal = case_when(sex == "Male" ~round((people/sum(filtered_pop_data$value))*100,2),
                             TRUE ~-round((people/sum(filtered_pop_data$value))*100,2)),
         signal = case_when(sex == "Male" ~1,
                            TRUE~-1))

# Produce total population pyrmaid plot
pyramid_plot2 <- pyramid_data1 %>%
  ggplot() +
  geom_bar(aes(x=percTotal, y=age, fill=sex), stat="identity")+
  scale_fill_manual(name="", values=c("#F2BC94", "#104c91")) +
  scale_x_continuous(breaks = seq(-0.2, 0.2, 0.1),
                     labels = function(x)paste(x, "%")) +
  labs(x="Population (%)", y="Age Group", title="Cardiovascular Diseases Population Pyramid of Afghanistan",
       subtitle=paste("Total population with cardiovascular diseases:", format(sum(pyramid_data1$people), big.mark = ","))) +
  theme_minimal()







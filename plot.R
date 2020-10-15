
# Load libraries
library("readxl")
library("magrittr")
library("janitor")
library("ggplot2")
library("dplyr")

# Read data
x <- read_excel("/Users/carinapeng/PAHO : WHO/who-noncommunicable-diseases/excel/Input tables for Carina_6Oct2020.xlsx", sheet = 3)

# Clean data
x <- x %>% 
  clean_names()

# create dummy data
don <- data.frame(
  x = rep(seq(2000,2005), 3),
  value = c(  75, 73, 68, 57, 36, 0, 15, 16, 17, 18, 19, 20, 10, 11, 15, 25, 45, 80),
  group = rep(c("A", "B", "C"), each=6)
)

x %>%
  filter(sex == "Male")

# Plot a stacked density plot
plot <- x %>% 
  filter(country == "Afghanistan", sex == "Male") +
  ggplot(aes(x=gbd_location_name, y=percentage_of_population, fill=condition)) +
  geom_area( ) +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  theme_ipsum() +
  theme(legend.position="none")

library(ggplot2)
library(markdown)
library(shiny)

library(readxl)
library(magrittr)
library(janitor)
library(ggplot2)
library(dplyr)
library(rhandsontable)

setwd("/Users/carinapeng/PAHO : WHO/who-noncommunicable-diseases")

# Load data
gbd <- read_excel("/Users/carinapeng/PAHO : WHO/who-noncommunicable-diseases/data/Input tables for Carina_6Oct2020.xlsx", sheet = 3) %>%
    clean_names()
pop <- read_excel("/Users/carinapeng/PAHO : WHO/who-noncommunicable-diseases/data/Input tables for Carina_6Oct2020.xlsx", sheet = 4) %>%
    clean_names()


# Define UI for data upload app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Comorbidities Tool"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            uiOutput("dropdown_country"),
            uiOutput("dropdown_sex")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Data file ----
            tabsetPanel( #type = "tabs",
                tabPanel("Welcome"),
                tabPanel("Results Table",
                         tableOutput("dataframe"),
                         rHandsontableOutput("dataframe1"),
                         br(),
                         actionButton("saveBtn", "Save")
                         )
                
            )
    
)))

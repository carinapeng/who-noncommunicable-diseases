
library(ggplot2)
library(markdown)
library(shiny)
library(tidyr)
library(readxl)
library(magrittr)
library(janitor)
library(ggplot2)
library(dplyr)
library(rhandsontable)

setwd("/Users/carinapeng/PAHO : WHO/who-noncommunicable-diseases")

gbd <- readRDS("gbd.rds")
pop <- readRDS("pop.rds")
joined <- readRDS("joined.rds")


# Define UI for data upload app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("PAHO / LSHTM"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            selectInput("selectdata", "Please select prevalence data",
                        c(Choice = "", list("GBD 2017" = "gbd2017", 
                             "GBD 2019" = "gbd2019",
                             "Enter your own" = "manual"))),
            selectInput("dropdown_country", "Select country", choices = c(Choice="", unique(gbd$country))),
            selectInput("dropdown_sex", "Select Sex", choices = c(Choice="", unique(gbd$sex)))
            # checkboxGroupInput("checkbox", "Please check the conditions to display", 
            #                    choices = list("Cardiovascular diseases" = "cardiovascular", 
            #                                   "Chronic kidney diseases" = "kidney", 
            #                                   "Chronic respiratory diseases" = "respiratory",
            #                                   "Cirhossis and other chronic liver disease" = "cirhossis",
            #                                   "Diabetes mellitus" = "diabetes",
            #                                   "Cancers with direct immunosuppression" = "cancer_dir",
            #                                   "Cancers with possible immunosuppression" = "cancer_poss",
            #                                   "HIV/AIDS" = "hiv_aids",
            #                                   "Non-latent TB" = "tb",
            #                                   "Chronic neurological disorders" = "neuro",
            #                                   "Sickle cell disorders" = "sickle_cell"
            #                                   ), 
            #                    selected = "cardiovascular")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Data file ----
            tabsetPanel( #type = "tabs",
                tabPanel("Welcome",
                         includeMarkdown(
                             "app/www/welcome.md"
                         )
                         ),
                tabPanel("Plots",
                         plotOutput("prevalence_plot"),
                         plotOutput("population_plot"),
                         plotOutput("facet_plot"),
                         plotOutput("increased_risk_plot")
                         ),
                
                tabPanel("Pyramid",
                         selectInput("pyramid_select", "Conditions", choices=c(Choose="", unique(gbd$condition)), selectize = FALSE),
                         plotOutput("pyramid_plot1"),
                         plotOutput("pyramid_plot2")
                         ),
                tabPanel("Joined",
                         tableOutput("test")),
                
                tabPanel("Results Table",
                         uiOutput("dataframe")
                         ),
                tabPanel("Editable Table",
                         rHandsontableOutput("dataframe1"),
                         br(),
                         actionButton("saveBtn", "Save")
                )
                
            )
            
    
)))

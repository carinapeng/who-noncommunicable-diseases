
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


# Define UI for data upload app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("PAHO / LSHTM"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            selectInput("selectdata", "Please select prevalence data",
                        list("GBD 2017" = "gbd2017", 
                             "GBD 2019" = "gbd2019",
                             "Enter your own" = "manual")),
            uiOutput("dropdown_country"),
            uiOutput("dropdown_sex")
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
                         withMathJax(includeMarkdown(
                             "app/www/welcome.md"
                         ))
                         ),
                tabPanel("Plots",
                         tableOutput("test"),
                         plotOutput("prevalence_plot"),
                         plotOutput("population_plot"),
                         plotOutput("facet_plot")
                         ),
                
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

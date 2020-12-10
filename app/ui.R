
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
library(viridis)
library(plotly)
library(openxlsx)
library(xlsx)

 gbd <- readRDS("data/gbd.rds")
 pop <- readRDS("data/pop.rds")
 joined <- readRDS("data/joined.rds")


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
                             "Upload your own" = "manual"))),
            selectInput("dropdown_country", "Select country", choices = c(Choice="", unique(gbd$country))),
            selectInput("dropdown_sex", "Select Sex", choices = c(Choice="", unique(gbd$sex))),
            fileInput("file1", "Choose Excel File",
                      accept = c(".xlsx"))
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Data file ----
            tabsetPanel( #type = "tabs",
                tabPanel("Welcome",
                         withMathJax(includeMarkdown("www/welcome.md")),
                         uiOutput("download_excel"),
                         tableOutput("test")
                         ),
                tabPanel("Prevalence",
                         withMathJax(includeMarkdown("www/methodology.md")),
                         plotlyOutput("prevalence_plot"),
                         hr(),
                         plotlyOutput("facet_plot")
                         ),
                
                tabPanel("Population",
                         withMathJax(includeMarkdown("www/population.md")),
                         plotlyOutput("population_plot"),
                         hr(),
                         plotlyOutput("increased_risk_plot")
                         ),
                
                tabPanel("Pyramid",
                         withMathJax(includeMarkdown("www/pyramid.md")),
                         selectInput("pyramid_select", "Conditions", choices=c(Choose="", unique(gbd$condition)), selectize = FALSE),
                         plotlyOutput("pyramid_plot2"),
                         uiOutput("pyramid_text2"),
                         hr()
                         # plotlyOutput("pyramid_plot1"),
                         # uiOutput("pyramid_text1"),
                         )
                
                # tabPanel("Results Table",
                #          uiOutput("dataframe")
                #          ),
                
                # tabPanel("Editable Table",
                #          rHandsontableOutput("dataframe1"),
                #          br(),
                #          actionButton("saveBtn", "Save")
                # )
                
            )
            
    
)))

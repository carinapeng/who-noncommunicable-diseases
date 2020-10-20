
library(ggplot2)
library(markdown)
library(shiny)

library(readxl)
library(magrittr)
library(janitor)
library(ggplot2)
library(dplyr)
library(rhandsontable)


# Define server logic to read selected file ----
server <- function(input, output) { 
    
    output$dropdown_country <- renderUI(
        selectInput("dropdown_country","Select country", choices= 
                        as.character(unique(gbd$country)))
    )
    
    output$dropdown_sex <- renderUI(
        selectInput("dropdown_sex","Select sex", choices= 
                        as.character(unique(gbd$sex)))
    )
    
    gbd_country <- reactive({
        req(input$dropdown_country)
        gbd_country <- gbd %>%
            filter(country == input$dropdown_country, sex == input$dropdown_sex)
    })
    
    pop_country <- reactive({
        req(input$dropdown_country)
        pop_country <- pop %>%
            filter(country == "Afghanistan" & sex == "Male") %>%
            select(upper_age, value)
            
    })
    
    output$dataframe <- renderTable(digits = 6,
                                    {
                                        head(gbd_country())
                                    })
    
    output$dataframe1 <- renderRHandsontable({
        rhandsontable(gbd_country())
    })
    
    observeEvent(input$saveBtn,
                 write.csv(hot_to_r(input$dataframe1), file = "data/EditedGDB.csv"))
    
    
  
}

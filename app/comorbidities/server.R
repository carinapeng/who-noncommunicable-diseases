
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
    
  # Load data
  gbd <- read_excel("/Users/carinapeng/PAHO : WHO/who-noncommunicable-diseases/data/Input tables for Carina_6Oct2020.xlsx", sheet = 3) %>%
    clean_names()
  pop <- read_excel("/Users/carinapeng/PAHO : WHO/who-noncommunicable-diseases/data/Input tables for Carina_6Oct2020.xlsx", sheet = 4) %>%
    clean_names()
    
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
            filter(country == input$dropdown_country, sex == input$dropdown_sex) %>%
            select(upper_age, value)
            
    })
    
    joined <- reactive({
        inner_join(gbd_country(), pop_country()) %>%
        mutate(people = value * percentage_of_population,
               perc = percentage_of_population * 100)
    })
    
    dataframe1 <- renderRHandsontable({
        rhandsontable(gbd_country())
    })
    
    #output$test <- renderTable({
        #joined()
    #})
    
    output$dataframe <- renderTable({
        if (input$selectdata == "gbd2017")
            return((gbd_country()))
        else if (input$selectdata == "gbd2019")
            return("Not available")
        else if (input$selectdata == "manual")
            return("Not available")
    })
    
    output$dataframe1 <- renderRHandsontable({
        rhandsontable(gbd_country()) %>%
        hot_cols(columnSorting = TRUE, 
                 colWidths = 100) %>%
        hot_rows(rowHeights = 20) %>%
        hot_heatmap(cols = 6, color_scale = c("#ff9d8b", "#dcedc1"))
    })
    
    observeEvent(input$saveBtn,
                 write.csv(hot_to_r(input$dataframe1), file = "data/EditedGBD.csv"))
    
    output$prevalence_plot <- renderPlot({
        joined() %>%
        ggplot(aes(x=upper_age, y=perc, color=condition)) +
        geom_line(size=2, alpha=1) +
        geom_point(size = 2, color = "black") +
        scale_color_viridis(discrete=TRUE, option = "magma") +
        theme_minimal() +
        labs(color = "Conditions",
             title = "Prevalence of underlying conditions by age") +
        ylab("Percentage of Population") +
        xlab("Age (years)") 
    })
    
    output$facet_plot <- renderPlot({
      joined() %>%
        ggplot(aes(x=upper_age, y=perc, color=condition)) +
        geom_line(size=2, alpha=1) +
        scale_color_viridis(discrete=TRUE, option = "magma") + 
        theme_minimal() +
        labs(color = "Conditions",
             title = "Prevalence of underlying conditions by age") +
        ylab("Percentage of Population") +
        xlab("Age (years)") +
        facet_wrap(~condition, scales="free")
    })
    
    output$population_plot <- renderPlot({
        joined() %>%
        ggplot(aes(x=upper_age, y=people, fill=condition)) +
        geom_area(alpha=0.6 , size=1, colour="black") +
        scale_fill_viridis(discrete = TRUE, option = "magma") +
        theme_minimal() +
        labs(fill = "Conditions",
             title="Population (millions) with underlying conditions by age",
             x = "Age (years)",
             y = "Population (millions)")
    })
  
}

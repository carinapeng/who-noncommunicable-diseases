
gbd <- readRDS("gbd.rds")
pop <- readRDS("pop.rds")
joined <- readRDS("joined.rds")

# Define server logic to read selected file ----
server <- function(input, output) { 
    
    # gbd_country <- reactive({
    #     req(input$dropdown_country)
    #     gbd_country <- gbd %>%
    #         filter(country == input$dropdown_country, sex == input$dropdown_sex)
    # })
    # 
    # pop_country <- reactive({
    #     req(input$dropdown_country)
    #     pop_country <- pop %>%
    #         filter(country == input$dropdown_country, sex == input$dropdown_sex) %>%
    #         select(upper_age, value)
    # })
            
   joined_country <- reactive({
     req(input$dropdown_country)
     req(input$dropdown_sex)
     joined %>%
       filter(country == input$dropdown_country, sex == input$dropdown_sex)
   })
    
    risk_df <- reactive({
      joined_country() %>%
        mutate(first = (1-percentage_of_population)) %>%
        group_by(lower_age) %>%
        mutate(prod = prod(first)) %>%
        mutate(final = (1-prod)) %>%
        mutate(risk_pop = final * value) %>%
        mutate(prevalence = round((final * 100),2))
    })
    
    output$test <- renderDataTable(joined)
    
    dataframe1 <- renderRHandsontable({
        rhandsontable(joined_country())
    })
    
    output$dataframe <- renderTable({
      if (input$selectdata == "gbd2017")
            return((joined_country()))
        else if (input$selectdata == "gbd2019")
            return("Not available")
        else if (input$selectdata == "manual")
            return("Not available")
    })
    
    output$dataframe1 <- renderRHandsontable({
        rhandsontable(joined_country()) %>%
        hot_cols(columnSorting = TRUE, 
                 colWidths = 100) %>%
        hot_rows(rowHeights = 20) %>%
        # hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_heatmap(cols = 7, color_scale = c("#ff9d8b", "#dcedc1"))
    })
    
    observeEvent(input$saveBtn,
                 write.csv(hot_to_r(input$dataframe1), file = "data/EditedGBD.csv"))
    
    output$prevalence_plot <- renderPlotly({
        risk_df() %>%
        ggplot() +
        geom_line(aes(x=age, y=perc, group = condition, color = condition), size=2, alpha=1) +
        # geom_point(aes(x=upper_age, y=perc, color=condition), color = "black") +
        geom_line(aes(x=age, y=prevalence, group = condition), size=2, alpha=1, color = "orange") +
        scale_color_viridis(discrete=TRUE, option = "magma") +
        theme_minimal() +
        labs(color = "Conditions",
             title = "Prevalence of underlying conditions by age") +
        ylab("Percentage of Population") +
        xlab("Age (years)") 
    })
    
    output$facet_plot <- renderPlotly({
      joined_country() %>%
        ggplot(aes(x=upper_age, y=perc, color=condition)) +
        geom_line(size=2, alpha=1) +
        scale_color_viridis(discrete=TRUE, option = "magma") + 
        theme_minimal() +
        labs(color = "Conditions",
             title = "Prevalence of underlying conditions by age") +
        ylab("Percentage of Population") +
        xlab("Age (years)") +
        facet_wrap(~condition, scales="fixed")
    })
    
    output$population_plot <- renderPlotly({
        joined_country() %>%
        ggplot(aes(x=upper_age, y=people, fill=condition)) +
        geom_area(alpha=0.6 , size=1, colour="black") +
        scale_fill_viridis(discrete = TRUE, option = "magma") +
        theme_minimal() +
        labs(fill = "Conditions",
             title="Population (millions) with underlying conditions by age",
             x = "Age (years)",
             y = "Population (millions)")
    })
    
    output$increased_risk_plot <- renderPlotly({
      risk_df() %>%
        ggplot() +
        geom_point(aes(x = upper_age, y = value), color = "black") +
        geom_line(aes(x = upper_age, y = value), size=1, alpha=1) +
        geom_line(aes(x = upper_age, y = risk_pop), color = "orange", size=1, alpha=1) +
        theme_minimal() +
        labs(title = "Population at increased risk of severe COVID-19 disease by age group") +
        ylab("Population") +
        xlab("Age (years)")
    })
    
    pyramid_data <- reactive({
      joined %>%
        filter(condition == input$pyramid_select & country == input$dropdown_country & sex != "Both") %>%
        mutate(popPerc = case_when(sex == "Male" ~round(percentage_of_population*100,2),
                                   TRUE ~-round(percentage_of_population*100,2)),
               signal = case_when(sex == "Male" ~1,
                                  TRUE~-1))
    })
    
    output$pyramid_plot1 <- renderPlotly({
    pyramid_data() %>%
        ggplot() +
        geom_bar(aes(x=popPerc, y=age, fill=sex), stat = "identity") +
        scale_fill_manual(name="", values=c("#F2BC94", "#104c91")) +
        scale_x_continuous(breaks = seq(-50, 50, 25),
                           labels = function(x)paste(x, "%")) +
        labs(
          x="Prevalence (%)", y="Age Group", 
          # title="Cardiovascular Diseases Prevalence Pyramid of Afghanistan",
          title = paste(input$pyramid_select, "Prevalence Pyramid of Afghanistan"),
        subtitle=paste("Total population with", input$pyramid_select, ":", format(sum(pyramid_data()$people), big.mark = ","))) +
        theme_minimal()
    })
    
    pyramid_data2 <- reactive({
      
      filtered_pop_data <- pop %>%
        filter(country == input$dropdown_country & sex == "Both") 
      
      joined1 <- joined %>%
        filter(condition == input$pyramid_select & country == input$dropdown_country & sex != "Both") %>%
        mutate(percTotal = case_when(sex == "Male" ~round((people/sum(filtered_pop_data$value))*100,2),
                                     TRUE ~-round((people/sum(filtered_pop_data$value))*100,2)),
               signal = case_when(sex == "Male" ~1,
                                  TRUE~-1))
      
      return(joined1)
    
    })
    
    output$pyramid_plot2 <- renderPlotly({
      pyramid_data2() %>%
        ggplot() +
        geom_bar(aes(x=percTotal, y=age, fill=sex), stat="identity")+
        scale_fill_manual(name="", values=c("#F2BC94", "#104c91")) +
        scale_x_continuous(breaks = seq(-0.2, 0.2, 0.1),
                           labels = function(x)paste(x, "%")) +
        labs(x="Population (%)", y="Age Group", 
             title=paste(input$pyramid_select, "Population Pyramid of Afghanistan"),
             subtitle=paste("Total population with", input$pyramid_select, ":", format(sum(pyramid_data2()$people), big.mark = ","))) +
        theme_minimal()
    })
    
  
}

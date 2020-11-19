
# Read GBD data, UN population data, and the joined dataset of the two
# GBD (2017 edition) prevalence for the year 2017 by country, age and sex
gbd <- readRDS("gbd.rds")
# UNPOP (2019 revision) population size for the year 2020 by country, age and sex
pop <- readRDS("pop.rds")
joined <- readRDS("joined.rds")

# Define variables:
# prev_perc = prevalence rate in percentage format



# Define server logic to read selected file ----
server <- function(input, output) { 
   
  # Subset joined dataset with the country and sex of interest         
   joined_country <- reactive({
     req(input$dropdown_country)
     req(input$dropdown_sex)
     joined %>%
       filter(country == input$dropdown_country, sex == input$dropdown_sex) %>%
       mutate(prev_perc = round(prev*100),2)
   })
    
    # Show readable data after filtered by country and sex of interest
    output$dataframe <- renderTable({
      if (input$selectdata == "gbd2017")
            return((joined_country()))
        else if (input$selectdata == "gbd2019")
            return("Not available")
        else if (input$selectdata == "manual")
            return("Not available")
    })
    
    # Create editable table with RHandsontable
    dataframe1 <- renderRHandsontable({
      rhandsontable(joined_country())
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
    
    # Calculate population with increased risk
    risk_df <- reactive({
      joined_country() %>%
        mutate(first_step = (1-prev)) %>%
        group_by(age) %>%
        # Create product of first step values
        mutate(prod = prod(first_step)) %>%
        # Create final value by subtracting the product value from 1
        mutate(final_value = (1-prod)) %>%
        mutate(risk_pop = final_value * pop_total) %>%
        mutate(risk_prev = round((final_value * 100),2))
    })
    
    output$prevalence_plot <- renderPlotly({
        risk_df() %>%
        ggplot() +
        geom_line(aes(x=age, y=prev_perc, group = condition, color = condition), size=2, alpha=1) +
        # Show curve of percent prevalence of increased risk
        geom_line(aes(x=age, y=risk_prev, group = condition), size=2, alpha=1, color = "orange") +
        scale_color_viridis(discrete=TRUE, option = "magma") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(color = "Conditions",
             title = "Prevalence of underlying conditions by age") +
        ylab("Percentage of Population") +
        xlab("Age (years)") 
    })
    
    output$facet_plot <- renderPlotly({
      joined_country() %>%
        ggplot(aes(x=age, y=prev_perc, group = condition, color=condition)) +
        facet_wrap(~condition, scales="fixed") +
        geom_line(size=2, alpha=1) +
        scale_color_viridis(discrete=TRUE, option = "magma") + 
        theme_minimal() +
        theme(axis.text.x = element_blank(),
              strip.text.x = element_blank(),
              legend.position = "bottom") +
        labs(color = "Conditions",
             title = "Prevalence of underlying conditions by age") +
        ylab("Percentage of Population") +
        xlab("Age (years)")
    })
    
    output$population_plot <- renderPlotly({
        joined_country() %>%
        ggplot(aes(x=age, y=pop_condition, group = condition, fill=condition)) +
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
        # Plot total population by age group
        geom_point(aes(x = age, y = pop_total), color = "black") +
        geom_line(aes(x = age, y = pop_total, group = condition), size=1, alpha=1) +
        # Plot population at increased risk
        geom_line(aes(x = age, y = risk_pop, group = condition), color = "orange", size=1, alpha=1) +
        theme_minimal() +
        labs(title = "Population at increased risk of severe COVID-19 disease by age group") +
        ylab("Population") +
        xlab("Age (years)")
    })
    
    pyramid_data <- reactive({
      joined %>%
        filter(condition == input$pyramid_select & country == input$dropdown_country & sex != "Both") %>%
        mutate(prev_perc = round(prev*100), 2) %>%
        mutate(prev_perc_sex = case_when(sex == "Male" ~prev_perc,
                                   TRUE ~-prev_perc),
               signal = case_when(sex == "Male" ~1,
                                  TRUE~-1))
    })
    
    output$pyramid_plot1 <- renderPlotly({
    pyramid_data() %>%
        ggplot() +
        geom_bar(aes(x=prev_perc_sex, y=age, fill=sex), stat = "identity") +
        scale_fill_manual(name="", values=c("#F2BC94", "#104c91")) +
        scale_x_continuous(breaks = c(-50, 50, 25)) +
        labs(
          x="Prevalence (%)", y="Age Group", 
          # title="Cardiovascular Diseases Prevalence Pyramid of Afghanistan",
          title = paste(input$pyramid_select, "Prevalence Pyramid of Afghanistan"),
        subtitle=paste("Total population with", input$pyramid_select, ":", format(sum(pyramid_data()$pop_total), big.mark = ","))) +
        theme_minimal()
    })
    
    pyramid_data2 <- reactive({
      
      filtered_pop_data <- pop %>%
        filter(country == input$dropdown_country & sex == "Both") 
      
      joined1 <- joined %>%
        filter(condition == input$pyramid_select & country == input$dropdown_country & sex != "Both") %>%
        mutate(pop_perc = case_when(sex == "Male" ~round((pop_condition/sum(filtered_pop_data$pop_total))*100,2),
                                     TRUE ~-round((pop_condition/sum(filtered_pop_data$pop_total))*100,2)),
               signal = case_when(sex == "Male" ~1,
                                  TRUE~-1))
      
      return(joined1)
    
    })
    
    output$pyramid_plot2 <- renderPlotly({
      pyramid_data2() %>%
        ggplot() +
        geom_bar(aes(x=pop_perc, y=age, fill=sex), stat="identity")+
        scale_fill_manual(name="", values=c("#F2BC94", "#104c91")) +
        scale_x_continuous(breaks = c(-0.2, 0, 0.2)) +
        labs(x="Population (%)", y="Age Group", 
             title=paste(input$pyramid_select, "Population Pyramid of Afghanistan"),
             subtitle=paste("Total population with", input$pyramid_select, ":", format(sum(pyramid_data2()$pop_total), big.mark = ","))) +
        theme_minimal()
    })
    
  
}

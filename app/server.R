
# Read GBD data, UN population data, and the joined dataset of the two
# GBD (2017 edition) prevalence for the year 2017 by country, age and sex
gbd <- readRDS("data/gbd.rds")
# UNPOP (2019 revision) population size for the year 2020 by country, age and sex
pop <- readRDS("data/pop.rds")
joined <- readRDS("data/joined.rds")

# Define variables:
# prev_perc = prevalence rate in percentage format



# Define server logic to read selected file ----
server <- function(input, output) { 
   
  # Subset joined dataset with the country and sex of interest         
   joined_country <- reactive({
     req(input$dropdown_country)
     req(input$dropdown_sex)
     joined %>%
       filter(country == input$dropdown_country, sex == input$dropdown_sex)
   })
    
    # Show readable data after filtered by country and sex of interest
    # output$dataframe <- renderTable({
    #   if (input$selectdata == "gbd2017")
    #         return((joined_country()))
    #     else if (input$selectdata == "gbd2019")
    #         return("Not available")
    #     else if (input$selectdata == "manual")
    #         return("Not available")
    # })
   
   base_data <- reactive({
     req(input$dropdown_country)
     req(input$dropdown_sex)
     
     if(!is.null(input$file1)) {
       return(read.xlsx(input$file1$datapath, 1))
     } else {
     return(joined_country())
     }
     
     
     
   })
   
   
   output$test <- renderTable(base_data())
    
    output$download_excel <- renderUI({
      if(input$selectdata == "manual") {
        downloadButton("outputFile", "Download template to Excel")
      }
    })
    
    output$outputFile <- downloadHandler(
      filename = function(){
        paste(input$dropdown_country, input$dropdown_sex, ".xlsx")
      },
      content = function(file) {
        write.xlsx(joined_country(), file)
      }
      
    )
      
      
    
    # 
    # # Create editable table with RHandsontable
    # dataframe1 <- renderRHandsontable({
    #   rhandsontable(joined_country())
    # })
    # 
    # output$dataframe1 <- renderRHandsontable({
    #     rhandsontable(joined_country()) %>%
    #     hot_cols(columnSorting = TRUE, 
    #              colWidths = 100) %>%
    #     hot_rows(rowHeights = 20) %>%
    #     # hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
    #     hot_heatmap(cols = 5, color_scale = c("#ff9d8b", "#dcedc1"))
    # })
    # 
    # observeEvent(input$saveBtn,
    #              write.csv(hot_to_r(input$dataframe1), file = "data/EditedGBD.csv"))
    
    # Calculate population with increased risk
    risk_df <- reactive({
      x <- base_data() %>%
        mutate(prev_perc = round((prev*100),2)) %>%
        mutate(first_step = (1-prev)) %>%
        group_by(age) %>%
        # Create product of first step values
        mutate(prod = prod(first_step)) %>%
        # Create final value by subtracting the product value from 1
        mutate(final_value = (1-prod)) %>%
        mutate(risk_pop = final_value * pop_total) %>%
        mutate(risk_prev = round((final_value * 100),2)) %>%
        mutate(risk_condition = "Increased Risk")
      
      x$age <- factor(
        x$age,
        levels = c(
          "Under 5",
          "5 to 9",
          "10 to 14",
          "15 to 19",
          "20 to 24",
          "25 to 29",
          "30 to 34",
          "35 to 39",
          "40 to 44",
          "45 to 49",
          "50 to 54",
          "55 to 59",
          "60 to 64",
          "65 to 69",
          "70 to 74",
          "75 to 79",
          "80 to 84",
          "85 to 89",
          "90 to 94",
          "95 plus"
        )
      )
      
      return(x)
      
    })
    
    output$onecondition_table <- renderRHandsontable(
      
      risk_df() %>%
        head(20) %>%
        select(country, age, sex, pop_total, risk_pop, risk_prev) %>%
        mutate(pop_total = format(round(as.numeric(pop_total), 0), big.mark=",")) %>%
        mutate(risk_pop = format(round(as.numeric(risk_pop), 0), big.mark=",")) %>%
        rename(
          Country = country,
          Age = age,
          Sex = sex,
          "Total Population" = pop_total,
          "Number people with at least 1 NCD" = risk_pop,
          "Percentage" = risk_prev
        )  %>%
        mutate(Percentage = paste(Percentage, "%")) %>%
        rhandsontable()
                                               )
    
    # Produce plot of prevalence of different conditions as well as prevalence of increased risk
    output$prevalence_plot <- renderPlotly({
        p1 <- risk_df() %>%
        ggplot() + 
        geom_line(aes(x=age, y=prev_perc, group = condition, color = condition), size=1, alpha=1) +
        # Show curve of percent prevalence of increased risk
        geom_line(aes(x=age, y=risk_prev, group = risk_condition), size=1, alpha=1, color = "orange") +
        scale_color_viridis(discrete=TRUE, option = "magma") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(color = "Conditions",
             title = "Prevalence of underlying conditions by age") +
        ylab("Prevalence (%)") +
        xlab("Age Group") 
      
      ggplotly(p1, tooltip = c("x", "y", "group"))
      
    })
    
    # Produce faceted prevalence plot
    output$facet_plot <- renderPlotly({
      p2 <- risk_df() %>%
        ggplot(aes(x=age, y=prev_perc, group = condition, color=condition)) +
        facet_wrap(~condition, scales="fixed") +
        geom_line(size=1, alpha=1) +
        scale_color_viridis(discrete=TRUE, option = "magma") + 
        theme_minimal() +
        theme(axis.text.x = element_blank(),
              strip.text.x = element_blank(),
              legend.position = "bottom") +
        labs(color = "Conditions",
             title = "Prevalence of underlying conditions by age") +
        ylab("Prevalence (%)") +
        xlab("Age Group")
      
      ggplotly(p2, tooltip = c("x", "y", "color"))
    })
    
    # Show population with underlying conditions
    output$population_plot <- renderPlotly({
        p3 <- risk_df() %>%
        ggplot(aes(x=age, y=pop_condition, group = condition, fill=condition)) +
        geom_area(alpha=0.6 , size=1, colour="black") +
        scale_fill_viridis(discrete = TRUE, option = "magma") +
        theme_minimal() +
        labs(fill = "Conditions",
             title="Population with underlying conditions by age",
             x = "Age (years)",
             y = "Population") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        ggplotly(p3, tooltip = c("x", "y", "fill"))
    })
    
    # Define colors for legend
    colors <- c("Total Population" = "black", "Increased Risk" = "orange")
    
    # Plot total population and population at increased risk
    output$increased_risk_plot <- renderPlotly({
      p4 <- risk_df() %>%
        ggplot() +
        # Plot total population by age group
        geom_line(aes(x = age, y = pop_total, group = condition, color = "Total Population"), size=1, alpha=1) +
        # Plot population at increased risk
        geom_line(aes(x = age, y = risk_pop, group = risk_condition, color = "Increased Risk"), size=1, alpha=1) +
        theme_minimal() +
        labs(title = "Population at increased risk of severe COVID-19 disease by age group",
             x = "Age Group",
             y = "Population",
             color = "") +
      scale_color_manual(values = colors) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p4, tooltip = c("x", "y", "color"))
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
      joined1$pop_condition <- round(joined1$pop_condition,0)
      
      return(joined1)
    
    })
    
    output$pyramid_plot2 <- renderPlotly({
      if(input$pyramid_select == "") {}
      else{
      p6 <- pyramid_data2() %>%
        ggplot() +
        geom_bar(aes(x=pop_perc, y=age, group = age, fill=sex, pop_condition=pop_condition), stat="identity", alpha = 0.7, color = "black")+
        scale_fill_manual(name="", values=c("#e9a3c9", "#a1d76a")) +
        scale_x_continuous(breaks = c(-0.2, 0, 0.2)) +
        labs(x="Population (%)", y="Age Group", 
             title=paste("Population Pyramid of", input$pyramid_select, "in", input$dropdown_country)) +
        theme_minimal()
      ggplotly(p6, tooltip = c("x", "group", "pop_condition"))
      }
    
    })
    
    output$pyramid_text2 <- renderUI(
      if(input$pyramid_select == "") {}
      else{
        paste("Total population with", input$pyramid_select, "is", format(sum(pyramid_data2()$pop_condition), big.mark = ","))}
    )
  
}

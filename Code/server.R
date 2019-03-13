## Server for National Park Visitation App
## Author: Meghan Bowen

# Define Server
server <- function(input, output) {
  
  
  ## FIRST OUTPUTS: TAB 2 (Yearly Visitation)
  
  ## Output 1: Plot of Year vs. # of Millions Visitors
  output$year_plot <- renderPlot({
    
    ggplot(filter(all_year_visitation,
                  ParkName == input$year_graph_choice)) +
      geom_point(aes(x=Year, y = Visitors_Mil)) +
      labs(x= "Year",
           y= "Number of Visitors \n (millions)",
           title = paste("Yearly Recreation Visitors to", input$year_graph_choice)) +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none",
            axis.title = element_text(size=16),
            axis.text = element_text(size=12))
    
  })
  
  ## Output 2: Brush Function to show Year & Visitor Values
  
  brush_df <- reactive({
    #for some reason, the outputs are showing decimal points, so for easier display changed Year & Visitors to integers
    all_year_visitation$Year <- as.integer(all_year_visitation$Year)
    all_year_visitation$RecreationVisitors <- as.integer(all_year_visitation$RecreationVisitors)
    
    brush_data <- all_year_visitation %>%
      filter(ParkName == input$year_graph_choice) %>% 
      select(-ParkName) %>% 
      plyr::rename(c('RecreationVisitors' = '# of Visitors'))
    
  }) 
  
  ####output: table with brushed values
  
  output$yr_brush <- renderTable({
    
    brushedPoints(brush_df(), input$plot_brush) 
    
  })
  
  
  
  
  ## SECOND OUTPUTS: HOLT-WINTERS PREDICTIONS   
  
  ## REACTIVE OUTPUT 1: For the HW Plot
  park_predictions <- reactive({
    
    park_filter <- all_month_visitation %>% 
      filter(ParkName == input$predict_choice) %>% 
      select(-ParkName)
    
    gather_up <- gather(park_filter, key = "Month", value = "VisitorCount", JAN:DEC)
    gather_up$Month <- as.factor(gather_up$Month)
    
    months <- factor(levels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))
    gather_up$Month <- fct_relevel(gather_up$Month, months)
    
    gather_park <- gather_up[order(gather_up$Year),]
    gather_park <- unite(gather_park,
                         Year_Month, 
                         c(Year, Month), 
                         remove = T)
    
    park_ts <- ts(gather_park$VisitorCount, frequency = 12, start = c(1979,1))
    
    park_hw <- HoltWinters(park_ts)
    park_hw_forecast <- forecast(park_hw, h = 60)
    
    return(park_hw_forecast)
  })
  
  
  ##Holt-Winters Predictions Plot
  output$predict_plot <- renderPlot({
    
    plot(park_predictions(),
         main = paste("Historic and Predicted Monthly Visitation to", input$predict_choice),
         xlab= "Year",
         ylab = "# of Visitors")
  })
  
  ##REACTIVE OUTPUT 2: For the table 
  predict_forecast <- reactive({
    
    park_filter <- all_month_visitation %>% 
      filter(ParkName == input$predict_choice) %>% 
      select(-ParkName)
    
    gather_up <- gather(park_filter, key = "Month", value = "VisitorCount", JAN:DEC)
    gather_up$Month <- as.factor(gather_up$Month)
    
    months <- factor(levels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))
    gather_up$Month <- fct_relevel(gather_up$Month, months)
    
    gather_park <- gather_up[order(gather_up$Year),]
    gather_park <- unite(gather_park,
                         Year_Month, 
                         c(Year, Month), 
                         remove = T)
    
    park_ts <- ts(gather_park$VisitorCount, frequency = 12, start = c(1979,1))
    
    park_hw <- HoltWinters(park_ts)
    park_hw_forecast <- forecast(park_hw, h = 60)
    
    return(park_hw_forecast)
  })
  
  predict_table <- reactive({
    
    pred_table <- as.data.frame(predict_forecast()) %>% 
      clean_names() %>% #from the janitor package, to make the names in snake_case for future renaming
      select(point_forecast) %>% 
      plyr::rename(c('point_forecast' = 'Mean Forecasted Visitors')) %>% 
      setDT(keep.rownames = "Month_Yr") %>% 
      separate(col = Month_Yr, into = c('Month', 'Year'), sep = " ") %>% 
      filter(Year %in% input$predict_check)
    
    pred_table$`Mean Forecasted Visitors` <- as.integer(pred_table$`Mean Forecasted Visitors`)
    return(pred_table)
  })
  
  
  ##Holt-Winters Predictions Table 
  output$HWTable <- renderTable({
    predict_table()
    
  },
  include.rownames = F)
  
  
  
  
  
  
  ## THIRD OUTPUTS: Travel Cost Information   
  
  ## REACTIVE OUTPUT 1: VALUE from inputs
  ### using mutate to calculate travel cost per row (on a monthly basis) based on inputs
  
  travel_one_month <- reactive({
    
    
    travel_filter <- np_travel_costs %>% 
      filter(ParkName == input$travel_park) %>% 
      filter(Month == input$travel_month) %>% 
      mutate(Travel_Cost = Entrance_Fee + # park entrance fee 
               if_else(input$travel_transpo == 1, Car_Trip, Fly_Trip) + #IF travelling by car, then return Car_Trip value, otherwise return Fly_Trip value
               if_else(input$travel_transpo == 1, 0, Addnl_Fly_Fee) + #IF travelling by car, do NOT return additional mileage to get from airport to park, otherwise return Addnl_Fly_Fee
               ifelse(input$travel_stay == 1, Camp_Day, input$travel_days*Hotel_Day) + #IF staying at a campsite, return that value, otherwise return the expression: # of days staying (input) TIMES hotel/lodge value
               Addnl_Boat_Fee) #BOAT FEE is 0 for all parks other than Channel Islands.
    
    travel_cost <- as.data.frame(travel_filter) %>% 
      select(Travel_Cost)
    
    return(travel_cost)
    
  })
  
  ####output: Travel Cost Value from outputs
  
  output$travel_value <- renderTable({
    travel_one_month()
  },
  include.colnames = F)
  
  
  
  ## REACTIVE TWO: Using Previous Forecasted Value for Month Selected
  
  travel_pred <- reactive({
    
    park_filter <- all_month_visitation %>% 
      filter(ParkName == input$travel_park) %>% 
      select(-ParkName)
    
    gather_up <- gather(park_filter, key = "Month", value = "VisitorCount", JAN:DEC)
    gather_up$Month <- as.factor(gather_up$Month)
    
    months <- factor(levels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))
    gather_up$Month <- fct_relevel(gather_up$Month, months)
    
    gather_park <- gather_up[order(gather_up$Year),]
    gather_park <- unite(gather_park,
                         Year_Month, 
                         c(Year, Month), 
                         remove = T)
    
    park_ts <- ts(gather_park$VisitorCount, frequency = 12, start = c(1979,1))
    
    park_hw <- HoltWinters(park_ts)
    park_hw_forecast <- forecast(park_hw, h = 60)
    
    return(park_hw_forecast)
  })
  
  travel_table <- reactive({
    
    trav_table <- as.data.frame(travel_pred()) %>% 
      clean_names() %>% #from the janitor package, to make the names in snake_case for future renaming
      select(point_forecast) %>% 
      plyr::rename(c('point_forecast' = 'Mean Forecasted Visitors')) %>% 
      setDT(keep.rownames = "Month_Yr") %>% 
      separate(col = Month_Yr, into = c('Month', 'Year'), sep = " ") %>% 
      filter(Month == input$travel_month)
    
    trav_table$`Mean Forecasted Visitors` <- as.integer(trav_table$`Mean Forecasted Visitors`)
    
    return(trav_table)
  })
  
  
  output$travel_predict <- renderTable({
    travel_table()
  })
  
  
  
  
  
}






# Run the application 
shinyApp(ui = ui, server = server)



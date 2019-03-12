# Load Packages
library(shiny)
library(tidyverse)
library(shinythemes)
library(ggfortify)
library(janitor)
library(data.table)
library(plotly)
library(tseries)
library(forecast)


# Load Data

## Read in Yearly Visitation DF
all_year_visitation <- read_csv("~/github/BowenShinyApp/all_year_visitation.csv")
all_year_visitation$ParkName <- as.factor(all_year_visitation$ParkName)

## Read in Monthly Visitation DF
all_month_visitation <- read_csv("~/github/BowenShinyApp/all_month_visitation.csv")
all_month_visitation$ParkName <- as.factor(all_month_visitation$ParkName)
all_month_visitation$Year <- as.factor(all_month_visitation$Year)

## Read in Travel Cost DF
np_travel_costs <- read_csv("~/github/BowenShinyApp/np_travel_costs.csv")
np_travel_costs <- as.data.frame(np_travel_costs)




# Define UI 
ui <- fluidPage(
   
   # Application title
   titlePanel("US National Park Visitation"),
   
   navbarPage("",
              theme = shinytheme("simplex"),
      
              
  # TAB 1: Summary of the App
              tabPanel("Summary",
                       p("Thanks for checking out my shiny app! This app evaluates visistor attendance to predict future visitor counts for ten United States National Parks. Additionally, this app calculates the estimated travel cost to a Park from southern California (Santa Barbara/Los Angeles). The following Parks are used in this app:"),
                       p(tags$ul(
                         tags$li("Arches"),
                         tags$li("Badlands"),
                         tags$li("Channel Islands"),
                         tags$li("Glacier"),
                         tags$li("Grand Teton"),
                         tags$li("Redwood"),
                         tags$li("Shenandoah"),
                         tags$li("Yellowstone"),
                         tags$li("Yosemite"),
                         tags$li("Zion")
                       )),
                       h3("What does past visitation look like?"),
                       p("Under the", tags$b("Historic Trends"), "tab, you can view overall trends in yearly attendance at a National Park of choice, up to 2018. The starting year will vary based on the relative age of the park and/or when the National Park Service first starting collecting visitation data for that park."),
                       h3("What will visitation be in the near future?"),
                       p("Under the", tags$b("Predicted Trends"), "tab, you can view a forecasted model of selected park attendance from 2019 to 2023. Each forecast is calculated using the Holt-Winters method of smoothing to take into account the seasonality of National Park visitation. A table is also presented for the selected National Park, which shows the average predicted attendance value for each month from 2019-2023."),
                       h3("How much does it cost to visit?"),
                       p("Under the", tags$b("Travel Costs"), "tab, you can view the cost of travelling and staying at a selected National Park. The travel cost will depend on your choices of travel month, housing, and transportation. The following parameters are used to calculate travel cost:"),
                       p(tags$ul(
                         tags$li(tags$em("Entrance Fee:"), "Price for National Park entrance (if applicable)"),
                         tags$li(tags$em("Campsite Rental:"), "Average price to rent a campsite at the Park"),
                         tags$li(tags$em("Lodging/Hotel Rate:"), "Average price to rent a room at a lodge or hotel near the Park"),
                         tags$li(tags$em("Car Mileage:"), "Roundtrip cost to drive from Santa Barbara to the Park (and back)"),
                         tags$li(tags$em("Plane Trip:"), "Roundtrip cost to fly from LAX to an airport nearest the Park (and back), including additional auto mileage cost"),
                         tags$li(tags$em("Additional Fee(s):"), "Any additional fees to get to the Park")
                       )),
                       tags$br(),
                       tags$br(),
                       p("See the" , tags$b("Metadata"), "tab for more information on data sources.")
                       ),
     
  
  
  
  # TAB 2: Graph Historic Vistiation Trends         
              tabPanel("Historic Trends",
                       
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("year_graph_choice",
                                       h4("Choose a National Park:"), 
                                       c("Arches", "Badlands", "Channel Islands", "Glacier", "Grand Teton", "Redwood", "Shenandoah", "Yellowstone", "Yosemite", "Zion")
                                       )),
                         
                         mainPanel(
                           h5("Select a National Park from the dropdown box to the left. You can also click and drag over point(s) on the graph to see the actual visitor counts to the selected Park."),
                           plotOutput(outputId = "year_plot",
                                      height = "450px",
                                      brush = brushOpts(id = "plot_brush")),
                           tags$br(),
                           h4("Click & Drag Output"),
                           tableOutput("yr_brush")
      
                       ))),
  
  
  
  
  # TAB 3: Forecast Monthly Visitation using Holt-Winters smoothing
              tabPanel("Predicted Trends",
                       
                       sidebarLayout(
                         sidebarPanel(
                               selectInput("predict_choice",
                                           label = h4("Choose a National Park:"), 
                                           choices = c("Arches", "Badlands", "Channel Islands", "Glacier", "Grand Teton", "Redwood", "Shenandoah", "Yellowstone", "Yosemite", "Zion") 
                                           ),
                               checkboxGroupInput("predict_check",
                                                  label = h4("Choose a Year:"),
                                                  choices = c(2019, 2020, 2021, 2022, 2023),
                                                  selected = 2019)),
                     
            
                       mainPanel(
                         h5("Select a National Park from the dropdown box to the left to see a graph with forecasted values for future monthly visitors, up to 5 years in the future (2019-2023). The shaded gray areas on the plot represent the range of possible values (lowest bound prediction to highest bound prediction). The blue line represents the average forecasted value."),
                         h5("The table below the graph shows the average predicted value for every month for your selected Park. You can choose a range of years with predictions using the checkbox."),
                         plotOutput(outputId = "predict_plot",
                                    height = "450px"),
                         tags$br(),
                         h4("Table with Predictions"),
                         tableOutput("HWTable"))
                       )),
              
              
  # TAB 4: Calculate Travel Cost to a Park using Inputs 
              tabPanel("Travel Costs",
                       
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("travel_park",
                                       label = h4("Choose a National Park:"), 
                                       choices = c("Arches", "Badlands", "Channel Islands", "Glacier", "Grand Teton", "Redwood", "Shenandoah", "Yellowstone", "Yosemite", "Zion")),
                           
                           tags$hr(),
                           
                           selectInput("travel_month", 
                                        label = h4("When to Stay?"),
                                        choice = list("January"="Jan", "February" = "Feb", "March" = "Mar", "April" = "Apr", "May", "June" = "Jun", "July" = "Jul", "August" = "Aug", "September" = "Sep", "October" = "Oct", "November" = "Nov", "December"="Dec")), #names in data are shortened, "long name" = "short name (in data)"
                           
                           tags$hr(),
                           
                           sliderInput("travel_days",
                                       label = h4("How long to Stay?"),
                                       min = 1,
                                       max = 21,
                                       value = 1),
                           
                           tags$hr(),
                           
                           radioButtons("travel_stay",
                                        label = h4("Where to Stay?"),
                                        choices = c("Campsite" = 1, "Hotel/Lodge" = 2)), 
                           
                           tags$hr(),
                           
                           radioButtons("travel_transpo",
                                        label = h4("How to Get There?"),
                                        choices = c("Car" = 1, "Plane" = 2))
                         ),
                         
                       
                       mainPanel(
                         h5("Select a National Park and month of travel using the dropdown boxes to the left -- you can also calculate the travel cost of a multi-day stay (up to 3 weeks) using the slider. Choose your preferred housing and method of travel using the buttons."), 
                         h5("Travel cost of your options will be updated with your choices. As a point of comparison, the predicted visitation values for the month at your chosen Park will also show up in the 'Predicted # of Visitors' table."),
                         p(tags$i("If an NA value is returned, you will have to select a different housing option -- some National Parks do not have campsites available in winter months.")),
                         tags$br(),
                         h4("Travel Cost ($)"),
                         tableOutput("travel_value"),
                         tags$hr(),
                         h4("Predicted # of Visitors"),
                         tableOutput("travel_predict"),
                         tags$hr(),
                         h4("Travel Cost for Every Month"),
                         tableOutput("travel_table")

                         ))
                       ),
  
  
  
  
  # TAB 5: Metadata
              tabPanel("Metadata",
                       h4("Author: Meghan Bowen"),
                       p("More information on forecasting and travel cost calculations available in my app repository on github:", tags$a(href = "https://github.com/megebowen/BowenShinyApp", "https://github.com/megebowen/BowenShinyApp")),
                       tags$br(),
                       tags$br(),
                       h4("Data Sources"),
                       h5(tags$i("Visitation Data")),
                       p("National Park Visitation Statistics:", tags$a(href = "https://irma.nps.gov/Stats/", "https://irma.nps.gov/Stats")),
                       tags$br(),
                       h5(tags$i("Travel Cost Data")),
                       p("Entrance Fees:", tags$a(href = "https://www.nps.gov/aboutus/entrance-fee-prices.htm", "https://www.nps.gov/aboutus/entrance-fee-prices.htm")),
                       p("Campsite Data:", tags$a(href = "https://www.nps.gov/subjects/camping/campground.htm", "https://www.nps.gov/subjects/camping/campground.htm")),
                       p("Lodging/Hotel Data:", tags$a(href = "https://www.nationalparkreservations.com/", "https://www.nationalparkreservations.com/")),
                       p("Gas Price & Mileage Data:", tags$br(), 
                         tags$a(href = "https://gasprices.aaa.com/state-gas-price-averages", "AAA Average US Gas Prices"), 
                         tags$br(), tags$a(href = "https://www.bts.gov/content/average-fuel-efficiency-us-light-duty-vehicles", "Average Fuel Efficiency of US Fleet"), 
                         tags$br(), tags$a(href = "maps.google.com", "Google Maps Distances")),
                       p("Plane Travel Data:", tags$a(href = "https://www.google.com/flights", "https://www.google.com/flights")),
                       p("Additional Fee Data:",
                         tags$br(),
                         tags$a(href =  "http://islandpackers.com/","Channel Islands Boat Trip Data"))
                       )
                       
              
              )

)







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
      mutate(Travel_Cost = input$travel_days * 
               (Entrance_Fee + # park entrance fee +
               if_else(input$travel_transpo == 1, Car_Trip, Fly_Trip) + #IF travelling by car, then return 2 times Car_Trip value, otherwise return 2 times Fly_Trip value
               if_else(input$travel_transpo == 1, 0, Addnl_Fly_Fee) + #IF travelling by car, do NOT return additional mileage to get from airport to park, otherwise return 2 times that value
               if_else(input$travel_stay == 1, Camp_Day, Hotel_Day) + #IF staying at a campsite, return that value, otherwise return the hotel/lodge value
               Addnl_Boat_Fee)) #BOAT FEE is 0 for all parks other than Channel Islands.
    
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
  
  
  ## REACTIVE THREE: TABLE with all monthly predictions
  
#  observe({
#    input$travel_month
#    input$travel_stay
#    input$travel_transpo
    
#    get(isolate(input$travel_park))
#  })
  
  travel_compare <- reactive({
    
    # same as above, except no filtering by PARK. see above for explanations
    travel_filter2 <- np_travel_costs %>% 
      mutate(Travel_Cost = Entrance_Fee + 
               if_else(input$travel_transpo == 1, Car_Trip, Fly_Trip) +
               if_else(input$travel_transpo == 1, 0, Addnl_Fly_Fee) +
               if_else(input$travel_stay == 1, Camp_Day, Hotel_Day) +
               Addnl_Boat_Fee) %>% 
      filter(Month == input$travel_month) %>% 
      select(ParkName, Month, Travel_Cost)
    
    return(travel_filter2)
  
  })
  
  ####output: Travel Cost Table for ALL MONTHS from outputs
  
  output$travel_table <- renderTable({
    
    travel_compare()
  })
  

    
   
}






# Run the application 
shinyApp(ui = ui, server = server)


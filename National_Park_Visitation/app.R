library(shiny)
library(tidyverse)
library(shinythemes)
library(ggfortify)
library(janitor)
library(plotly)
library(tseries)
library(forecast)

# Read in Yearly Visitation DF
all_year_visitation <- read_csv("~/github/BowenShinyApp/all_year_visitation.csv")
all_year_visitation$ParkName <- as.factor(all_year_visitation$ParkName)

# Read in Monthly Visitation DF
all_month_visitation <- read_csv("~/github/BowenShinyApp/all_month_visitation.csv")
all_month_visitation$ParkName <- as.factor(all_month_visitation$ParkName)
all_month_visitation$Year <- as.factor(all_month_visitation$Year)

# Read in Travel Cost DF
np_travel_costs <- read_csv("~/github/BowenShinyApp/np_travel_costs.csv")


# Define UI 
ui <- fluidPage(
   
   # Application title
   titlePanel("US National Park Visitation"),
   
   navbarPage("",
              theme = shinytheme("simplex"),
      
              
  # TAB 1: Summary of the App
              tabPanel("Summary",
                       h2(""),
                       p("Thanks for checking out my shiny app! I have collected data for recreation visits to ten United States National Parks:"),
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
                       h3("How have visitation rates changed over time?"),
                       p("Under the", tags$b("Historic Trends"), "tab, you can view overall trends in yearly attendance at a National Park of choice, up to 2018. The starting year will vary based on the relative age of the park and/or when the National Park Service first starting collecting visitation data for that park."),
                       h3("What will National Park visitation be in the future?"),
                       p("Under the", tags$b("Predicted Trends"), "tab, you can view a forecasted model of selected park attendance from 2019 to 2023. Each forecast is calculated using the Holt-Winters method of smoothing to take into account the seasonality of National Park visitation. A table is also presented for the selected National Park, which shows the average predicted attendance value for each month from 2019-2023."),
                       h3("How much does it cost to visit a National Park?"),
                       p("Under the", tags$b("Travel Costs"), "tab, you can view the cost of travelling and staying at a selected National Park. The following parameters are used to calculate travel cost:"),
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
                       p("See the" , tags$b("Metadata"), "tab for more information on data sources and travel cost parameter calculations.")
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
                           h3(textOutput("caption")),
                           plotOutput(outputId = "year_plot",
                                      height = "450px",
                                      hover = "plot_hover"),
                           verbatimTextOutput("yr_hover")
      
                       ))),
  
  
  
  
  # TAB 3: Forecast Monthly Visitation using Holt-Winters smoothing
              tabPanel("Predicted Trends",
                       
                       sidebarLayout(
                         sidebarPanel(
                               selectInput("predict_choice",
                                           label = h4("Choose a National Park:"), 
                                           choices = c("Arches", "Badlands", "Channel Islands", "Glacier", "Grand Teton", "Redwood", "Shenandoah", "Yellowstone", "Yosemite", "Zion") 
                                           )),
                     
            
                       mainPanel(
                         plotOutput(outputId = "predict_plot",
                                    height = "450px"),
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
                           
                           radioButtons("travel_stay",
                                        label = h4("Where to Stay?"),
                                        choices = c("Campsite" = 1, "Hotel/Lodge" = 2)), 
                           
                           tags$hr(),
                           
                           radioButtons("travel_transpo",
                                        label = h4("How to Get There?"),
                                        choices = c("Car" = 1, "Plane" = 2))
                         ),
                         
                       
                       mainPanel(
                         textOutput("travel_value")

                         ))
                       ),
  
  
  
  
  # TAB 5: Metadata info and Calculations
              tabPanel("Metadata",
                       h3("Data Sources")
                       )
                       
              
              )

)







# Define Server
server <- function(input, output) {
   

## FIRST OUTPUTS: TAB 2 (Yearly Visitation)
  
  ## Plot of Year vs. # of Millions Visitors
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
   
   ## Hover Function to show Year & Visitor Values
   output$yr_hover <- renderPrint({
     
     hover_fxn <- function(e) {
       if(is.null(e)) return ("NA")
    
      paste("Year", return(e$x),
            "Visitors", return(e$y))  
     }
     
     paste(hover_fxn(input$plot_hover))
   
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
   
  ##REACTIVE OUTPUT 2: For the table 
  predict_table <- reactive ({
    
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
    
    pred_table <- as.data.frame(park_hw_forecast) %>% 
      clean_names() %>% #from the janitor package, to make the names in snake_case for future renaming
      select(point_forecast) %>% 
      plyr::rename(c('point_forecast' = 'Mean Forecasted Value'))
    
    return(pred_table)
  })
  
  
  
  ##Holt-Winters Predictions Plot
   output$predict_plot <- renderPlot({
     
     plot(park_predictions(),
          main = paste("Historic and Predicted Visitation to", input$predict_choice, "\n (By Month)"),
          xlab= "Year",
          ylab = "# of Visitors")
   })
   
  ##Holt-Winters Predictions Table 
   output$HWTable <- renderTable({
     predict_table()
     
   },
   include.rownames = T)
   
   
   
##THIRD OUTPUTS: Travel Cost Information   
  
  ## REACTIVE OUTPUT 1: VALUE from inputs
  travel_value <- reactive({
    
    travel_filter <- np_travel_costs %>% 
      filter(ParkName == input$travel_park) %>% 
      filter(Month == input$travel_month) %>% 
      mutate(Travel_Cost = Entrance_Fee + 
               if_else(input$travel_transpo == 1, Car_Trip, Fly_Trip) +
               ifelse(input$travel_stay == 1, Camp_Day, Hotel_Day) +
               Addnl_Boat_Fee)

    return(travel_filter)

  })
    
    ######NEED TO WORK ON TRAVEL COST FORMULA
   
    ####output: Travel Cost Value from outputs
  
  output$travel_value <- renderPrint({
    travel_value()
  })
  
  ## TWO: TABLE with all monthly predictions
   
  
  ## THREE: GRAPH with cost over the year??
    
   
}






# Run the application 
shinyApp(ui = ui, server = server)


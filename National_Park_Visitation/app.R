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

# Read in HW Forecasting (DF?)

# Read in Travel Cost DF

# Define UI 
ui <- fluidPage(
   
   # Application title
   titlePanel("US National Park Visitation"),
   
   navbarPage("",
              theme = shinytheme("simplex"),
              
              tabPanel("Summary",
                       h2("XXXXX"),
                       p("Thanks for checking out my app!"),
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
              
              
              tabPanel("Travel Costs",
                       
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("travel_park",
                                       label = h4("Choose a National Park:"), 
                                       choices = c("Arches", "Badlands", "Channel Islands", "Glacier", "Grand Teton", "Redwood", "Shenandoah", "Yellowstone", "Yosemite", "Zion")),
                           
                           tags$hr(),
                           
                           selectInput("travel_month", 
                                        label = h4("When to Stay?"),
                                        choices = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")),
                           
                           tags$hr(),
                           
                           radioButtons("travel_stay",
                                        label = h4("Where to Stay?"),
                                        choices = c("Campsite", "Hotel/Lodge")),
                           
                           tags$hr(),
                           
                           radioButtons("travel_transpo",
                                        label = h4("How to Get There?"),
                                        choices = c("Car", "Plane"))
                         ),
                         
                       
                       mainPanel(
                         

                         ))
                       ),
              
              
              tabPanel("Metadata",
                       h3("Data Sources")
                       )
                       
              
              )

)







# Define Server
server <- function(input, output) {
   

## FIRST OUTPUTS: YEARLY GRAPH
  
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
  
  ## ONE: VALUE from inputs
   
  ## TWO: TABLE with all monthly predictions
   
  ## THREE: GRAPH with cost over the year??
    
   
}






# Run the application 
shinyApp(ui = ui, server = server)


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
   titlePanel("US National Park Visitation: Past and Future"),
   
   navbarPage("",
              
              tabPanel("Summary"),
              tabPanel("Historic Trends",
                       
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("year_graph_choice",
                                       "Choose a National Park:", 
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
                                           label = "Choose a National Park:", 
                                           choices = c("Arches", "Badlands", "Channel Islands", "Glacier", "Grand Teton", "Redwood", "Shenandoah", "Yellowstone", "Yosemite", "Zion") 
                                           )),
                     
            
                       mainPanel(
                         plotOutput(outputId = "predict_plot",
                                    height = "450px"),
                         tableOutput("HWTable"))
                       )),
              
              tabPanel("Travel Costs",
                       
                       mainPanel(
                         ))
                       
              
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
   
}






# Run the application 
shinyApp(ui = ui, server = server)


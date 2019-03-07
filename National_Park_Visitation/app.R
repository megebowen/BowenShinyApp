library(shiny)
library(tidyverse)
library(shinythemes)
library(ggfortify)
library(plotly)
library(tseries)
library(forecast)
library(gridExtra)

# Read in Yearly Visitation DF
all_year_visitation <- read_csv("~/github/BowenShinyApp/all_year_visitation.csv")
all_year_visitation$ParkName <- as.factor(all_year_visitation$ParkName)

# Read in Monthly Visitation DF

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
                               selectInput("select", inputId = "year_graph_choice",
                                           label = ("Choose a National Park:"), 
                                           choices = c("Arches", "Badlands", "Channel Islands", "Glacier", "Grand Teton", "Redwood", "Shenandoah", "Yellowstone", "Yosemite", "Zion") 
                                           )),
                     
            
                       mainPanel()
                       )),
              
              tabPanel("Travel Costs",
                       
                       mainPanel()
                       )
              
              )

)







# Define Server
server <- function(input, output) {
   
   output$year_plot <- renderPlot({
     
     ggplot(filter(all_year_visitation,
                             ParkName == input$year_graph_choice)) +
       geom_point(aes(x=Year, y = Visitors_Mil)) +
       labs(x= "Year",
            y= "Number of Visitors \n (millions)",
            title = paste("Yearly Visitors to", input$year_graph_choice)) +
       theme_classic() +
       theme(plot.title = element_text(hjust = 0.5),
             plot.subtitle = element_text(hjust = 0.5),
             legend.position = "none")
     
   })
   
   output$yr_hover <- renderPrint({
     
     hover_fxn <- function(e) {
       if(is.null(e)) return ("NA")
    
      paste("Year", return(e$x),
            "Visitors", return(e$y))  
     }
     
     paste(hover_fxn(input$plot_hover))
   
     })
}






# Run the application 
shinyApp(ui = ui, server = server)


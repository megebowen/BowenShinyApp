library(shiny)
library(tidyverse)
library(shinythemes)
library(ggfortify)
library(plotly)
library(tseries)
library(forecast)
library(gridExtra)

all_year_visitation <- read_csv("~/github/BowenShinyApp/all_year_visitation.csv")


# Define UI 
ui <- fluidPage(
   
   # Application title
   titlePanel("US National Park Visitation: Past and Future"),
   
   navbarPage("",
              
              tabPanel("Summary"),
              tabPanel("Historic Trends",
                       
                       sidebarLayout(
                         sidebarPanel(
                           selectInput(inputId = "year_graph_choice",
                                       label = ("Choose a National Park:"), 
                                       choices = c("Arches", "Badlands", "Channel Islands", "Glacier", "Grand Teton", "Redwood", "Shenandoah", "Yellowstone", "Yosemite", "Zion")
                                       ),
                           
                           hr(),
                           fluidRow(column(3, verbatimTextOutput("value")))
                         ),
                           
                           mainPanel(
                             plotlyOutput("year_plot")
                           )
                         
      
                       )),
              
              tabPanel("Predicted Trends",
                       
                       sidebarLayout(
                         sidebarPanel(
                               selectInput("select", inputId = "year_graph_choice",
                                           label = ("Choose a National Park:"), 
                                           choices = c("Arches", "Badlands", "Channel Islands", "Glacier", "Grand Teton", "Redwood", "Shenandoah", "Yellowstone", "Yosemite", "Zion") 
                                           ),
                               
                               hr(),
                               fluidRow(column(3, verbatimTextOutput("value")))
                         ),
                     
            
                       mainPanel()
                       )),
              
              tabPanel("Travel Costs")
              
              )
   
)








# Define Server
server <- function(input, output) {
  
   year    <- reactive({
     subset(all_year_visitation, ParkName %in% input$graph_year_choice)
   })
   
   output$year_plot <- renderPlotly({
     yr_plot<- ggplot(all_year_visitation) +
       geom_point(aes(x=Year, y=Visitors_Mil),
                  subset(ParkName == year)) +
       labs(x= "Year",
            y= "Number of Visitors \n (millions)",
            title = "Yearly Visitors to input$year",
            subtitle = "(1904-2018)") +
       theme_classic() +
       theme(plot.title = element_text(hjust = 0.5),
             plot.subtitle = element_text(hjust = 0.5),
             legend.position = "none")
     
     ggplotly(yr_plot)
   })
}






# Run the application 
shinyApp(ui = ui, server = server)


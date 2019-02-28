library(shiny)
library(tidyverse)
library(shinythemes)
library(ggfortify)
library(tseries)
library(forecast)
library(gridExtra)

# Define UI 
ui <- fluidPage(
   
   # Application title
   titlePanel("Historic National Park Visitation Data and Predictions"),
   
   navbarPage("TEXT",
              
              tabPanel("Summary"),
              tabPanel("Historic Trends",
                       
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("select", label = h3("National Park"), 
                                       choices = list("Arches" = 1, "Badlands" = 2, "Channel Islands" = 3, "Glacier" = 4, "Grand Teton" = 5, "Redwood" = 6, "Shenandoah" = 7, "Yellowstone" = 8, "Yosemite" = 9, "Zion" = 10), 
                                       selected = 1),
                           
                           hr(),
                           fluidRow(column(3, verbatimTextOutput("value")))
                           ,
                           
                           br(),

                           radioButtons("radio", label = h3("Month"),
                                        choices = list("January" = 1, "February" = 2, "March" = 3, "April" = 4, "May" = 5, "June" = 6, "July" = 7, "August" = 8, "September" = 9, "October" = 10, "November" = 11, "December" = 12), 
                                        selected = 1),
                           
                           hr(),
                           fluidRow(column(3, verbatimTextOutput("value")))
                         ),
      
                         mainPanel()
                       )),
              
              tabPanel("Predicted Trends",
                       
                       sidebarLayout(
                         sidebarPanel(
                               selectInput("select", label = h3("National Park"), 
                                           choices = list("Arches" = 1, "Badlands" = 2, "Channel Islands" = 3, "Glacier" = 4, "Grand Teton" = 5, "Redwood" = 6, "Shenandoah" = 7, "Yellowstone" = 8, "Yosemite" = 9, "Zion" = 10), 
                                           selected = 1),
                               
                               hr(),
                               fluidRow(column(3, verbatimTextOutput("value")))
                         ),
                     
            
                       mainPanel()
                       )),
              
              tabPanel("Travel Costs")),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)








# Define Server
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}






# Run the application 
shinyApp(ui = ui, server = server)


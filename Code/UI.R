## Packages, Data, and UI for National Park Visitation App
## Author: Meghan Bowen


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
                          h5("Select a National Park from the dropdown box to the left. The", tags$b("Plot"), "tab will show you a graph of all visitors from the first recorded year to 2018."),
                          h5("You can also click and drag over point(s) on the graph to see the actual visitor counts to the selected Park. This information can be viewed in the", tags$b("Click & Drag Output"), "tab."),
                          tabsetPanel(
                            tabPanel("Plot",
                                     plotOutput(outputId = "year_plot",
                                                height = "450px",
                                                brush = brushOpts(id = "plot_brush"))),
                            tabPanel("Click & Drag Output",
                                     tableOutput("yr_brush"))
                          )
                          
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
                          h5("Select a National Park from the dropdown box to the left. The", tags$b("Plot"), "tab will produce a graph with forecasted values for future monthly visitors up to the next five years (2019-2023)."),
                          h5("Use the checkbox to the left to select a year (or years) to view the average predicted forecast value for every month in the", tags$b("Prediction Table"), "tab."),
                          tabsetPanel(
                            tabPanel("Plot",
                                     plotOutput(outputId = "predict_plot",
                                                height = "450px"),
                                     h5("The shaded", tags$span(style = "color:lightslategray", "gray areas"), "on the plot represent the range of possible values (lowest bound prediction to highest bound prediction). The" , tags$span(style = "color:blue", "blue line"), "represents the average forecasted value.")),
                            tabPanel("Prediction Table",
                                     tableOutput("HWTable"))
                          )))),
             
             
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
                          p(tags$i("If an NA value is returned, you will have to select a different housing option -- some National Parks vary in their availability of campsites or lodges/hotels.")),
                          tags$br(),
                          h4("Travel Cost ($)"),
                          tableOutput("travel_value"),
                          tags$hr(),
                          h4("Predicted # of Visitors"),
                          tableOutput("travel_predict")
                          
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
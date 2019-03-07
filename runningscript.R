radioButtons("radio", label = h3("Month"),
             choices = list("January" = 1, "February" = 2, "March" = 3, "April" = 4, "May" = 5, "June" = 6, "July" = 7, "August" = 8, "September" = 9, "October" = 10, "November" = 11, "December" = 12), 
             selected = 1),

hr(),
fluidRow(column(3, verbatimTextOutput("value")))

ggplot(filter(marvel, ALIGN == input$side), aes(x = Year)) +
  geom_bar(aes(fill = SEX), position = "fill") +
  scale_fill_brewer(palette = "RdPu") +
  theme_dark()

ALIGN is column name

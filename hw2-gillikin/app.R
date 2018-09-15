
library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)


#load the dataset flights.csv of all the flights out of PIT during April 2018


flightData <- read.csv("flights.csv")



# Define UI for application that draws a histogram
ui <- navbarPage("PIT", 
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("airline",
                                          "Airline: ",
                                          choices = sort(unique(flightData$Airline)),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = c("American", "Southwest")),
                              sliderInput("Flights",
                                          "Flights:",
                                          min = min(flightData$Number, na.rm = T),
                                          max = max(flightData$Number, na.rm = T),
                                          value = c(min(flightData$Number, na.rm = T), max(flightData$Number, na.rm = T)),
                                          step = 1)
                            ),
                            # Output plot
                            mainPanel(
                              plotlyOutput("plot")
                            )
                          )
                 ),
                 # Data Table
                 tabPanel("Table",
                          fluidPage(DT::dataTableOutput("table"))
                 )
)

# Define server logic
server <- function(input, output) {
  swInput <- reactive({
    flights <- flightData %>%

    return(flights)
  })
  mwInput <- reactive({
    swInput() %>%
      melt(id = "name")
  })
  output$plot <- renderPlotly({
    dat <- swInput()
    ggplotly(
      ggplot(data = dat, aes(x = mass, y = height, color = species)) + 
        geom_point() +
        guides(color = FALSE)
      , tooltip = "text")
  })
  output$table <- DT::renderDataTable({
    flights <- swInput()
    
    subset(flights, select = c(Number))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

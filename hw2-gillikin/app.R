
library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)


#load the dataset flights.csv of all the flights out of PIT between January and April 2018

flightData <- read.csv("flights.csv")


# Define UI for application that draws a histogram
ui <- navbarPage("Flights from Pittsburgh International Airport, January - April 2018", 
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("airlineSelect",
                                          "Airline: ",
                                          choices = sort(unique(flightData$airline)),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = c("American", "Southwest"))),

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

  fdInput <- reactive({
    flights <- flightData
  #    filter(number >= input$numberSelect[1] & number <= input$numberSelect[2])
    
    if (length(input$airlineSelect) > 0 ) {
      flights <- subset(flights, airline %in% input$airlineSelect)
    }
    
    return(flights)
  })


  output$plot <- renderPlotly({
    dat <- fdInput()
    ggplotly(
      ggplot(data = dat, aes(x = airline, y = number, color = month)) + 
        geom_point() +
        guides(color = FALSE)
      , tooltip = "text")
  })
  output$table <- DT::renderDataTable({
    subset(fdInput(), select = c(month, airline, destination, number))
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

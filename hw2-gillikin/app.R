
library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)


#load the dataset flights.csv of all the flights out of PIT between January and April 2018

flightData <- read.csv("flights.csv")


# Define UI for application that draws a histogram
ui <- navbarPage("PIT", 
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("airline",
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
  swInput <- reactive({
    flights <- flightData
  })

  output$plot <- renderPlotly({
    dat <- swInput()
    ggplotly(
      ggplot(data = dat, aes(x = airline, y = number)) + 
        geom_point() +
        guides(color = FALSE)
      , tooltip = "text")
  })
  output$table <- DT::renderDataTable({
    DT::datatable(flightData[, input$show_vars, drop = FALSE])
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

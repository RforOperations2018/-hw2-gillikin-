
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
                              # Input: Text input to let the viewer decide what the plot should mean
                              textInput(inputId = "caption",
                                        label = "Your chance to name this plot:",
                                        value = "Data Summary"),
                              
                              # Input: Select box to make picking a favorite airline easier
                              selectInput("airlineSelect",
                                          "Now pick an Airline: ",
                                          choices = sort(unique(flightData$airline)),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = c("Air Canada", "JetBlue")),
                              # Input: Slider to select range for number of flights in a month
                              sliderInput("seatsSelect",
                                        "Number of seats a flights has in a month:",
                                        value = 20000,
                                        min = 1,
                                        max = 40000),
                              # reset action button for observation
                              actionButton("reset", "Reset Selection", icon = icon("refresh"))
                          ),

                            # Output plot
                            mainPanel(
                               # Output of text for the pick-your-own caption
                              h3(textOutput("caption", container = span)),
                              # Output of plot 1
                              plotlyOutput("plot1"),
                              # Output for plot 2
                              plotlyOutput("plot2"),
                              # Output for plot 3
                              plotlyOutput("plot3")
                            )
                          )
                 ),
                 # Data Table
                 tabPanel("Table",
                          inputPanel(
                            downloadButton("downloadData","Download FLight Data")
                          ),
                          fluidPage(DT::dataTableOutput("table"))
                 )
)

# Define server logic
server <- function(input, output, session = session) {
  # making thing reactive
  fdInput <- reactive({
    flights <- flightData
    if (length(input$airlineSelect) > 0 ) {
      flights <- subset(flights, airline %in% input$airlineSelect)
    }
    return(flights)
  })
  # making thing reactive?????
  fdaInput <- reactive({
    flights.a <- flightData
    filter(seats == input$seatsSelect[1])
    return(flights.a)
  })
  # Make-you-own caption
  output$caption <- renderText({
    input$caption
  })
  # Plot 1
  output$plot1 <- renderPlotly({
    dat <- fdInput()
    ggplotly(
      ggplot(data = dat, aes(x = month, y = number, color = airline)) + 
        geom_point() +
        guides(color = FALSE)
      , tooltip = "text")
  })
  # Plot 2
  output$plot2 <- renderPlotly({
    dat <- fdInput()
    ggplotly(
      ggplot(data = dat, aes(x = number, fill = month), environment = environment()) + 
        geom_histogram())
  })
  # Plot 3
  output$plot3 <- renderPlotly({
    dat <- fdInput()
    ggplotly(
      ggplot(data = dat, aes(x = month, y = seats, color = airline, fill = airline)) + 
        geom_point())
  })
  # Datatable
  output$table <- DT::renderDataTable({
    subset(fdInput(), select = c(month, airline, destination, number))
  })
  # Reset Selection of Data
  observeEvent(input$reset, {
    updateSelectInput(session, "airlineSelect", selected = c("Air Canada", "JetBlue"))
    showNotification("Now ready for takeoff...", type = "message")
  })
  # Download data in the datatable
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("flights-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(fdInput(), file)
    }
  )
}
# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")

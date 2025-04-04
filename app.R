library(shiny)
library(tidyverse)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Interactive Data Explorer"),

    sidebarLayout(
        sidebarPanel(
            # File upload input
            fileInput("dataFile", "Upload your data (CSV, Excel, etc.)",
                      accept = c(".csv", ".xlsx", ".xls")),

            # Only show these panels after data is loaded
            conditionalPanel(
                condition = "output.dataLoaded",

                # Variable selection
                selectInput("xVar", "X Variable:", choices = NULL),
                selectInput("yVar", "Y Variable:", choices = NULL),

                # Plot type selection
                selectInput("plotType", "Plot Type:",
                            choices = c("Scatter Plot", "Box Plot", "Histogram", "Bar Chart")),

                # Filter options would go here
                numericInput("filterMin", "Minimum Value:", 0),
                numericInput("filterMax", "Maximum Value:", 100)
            )
        ),

        mainPanel(
            # Data table output
            h3("Data Preview"),
            DTOutput("dataTable"),

            # Plot output
            h3("Visualization"),
            plotOutput("dataPlot", height = "400px"),

            # Summary statistics output
            h3("Summary Statistics"),
            verbatimTextOutput("dataSummary")
        )
    )
)

# Server logic
server <- function(input, output, session) {

    # Reactive value to store the uploaded dataset
    datasetInput <- reactive({
        req(input$dataFile)

        # Read the file based on extension
        ext <- tools::file_ext(input$dataFile$name)

        if(ext == "csv") {
            read.csv(input$dataFile$datapath)
        } else if(ext %in% c("xls", "xlsx")) {
            readxl::read_excel(input$dataFile$datapath)
        }
    })

    # Update variable selection choices when data is loaded
    observe({
        data <- datasetInput()
        updateSelectInput(session, "xVar", choices = names(data))
        updateSelectInput(session, "yVar", choices = names(data))
    })

    # Data preview table
    output$dataTable <- renderDT({
        head(datasetInput(), 10)
    })

    # Signal that data is loaded
    output$dataLoaded <- reactive({
        return(!is.null(datasetInput()))
    })
    outputOptions(output, "dataLoaded", suspendWhenHidden = FALSE)

    # Generate the plot based on user selections
    output$dataPlot <- renderPlot({
        # Get the data
        data <- datasetInput()

        # Apply filters
        if(!is.null(input$filterMin) && !is.null(input$filterMax) && !is.null(input$yVar)) {
            data <- data[data[[input$yVar]] >= input$filterMin &
                             data[[input$yVar]] <= input$filterMax, ]
        }

        # Create different plot types based on selection
        if(input$plotType == "Scatter Plot") {
            ggplot(data, aes_string(x = input$xVar, y = input$yVar)) +
                geom_point() +
                theme_minimal()
        } else if(input$plotType == "Box Plot") {
            ggplot(data, aes_string(x = input$xVar, y = input$yVar)) +
                geom_boxplot() +
                theme_minimal()
        } else if(input$plotType == "Histogram") {
            ggplot(data, aes_string(x = input$yVar)) +
                geom_histogram() +
                theme_minimal()
        } else if(input$plotType == "Bar Chart") {
            ggplot(data, aes_string(x = input$xVar, y = input$yVar)) +
                geom_bar(stat = "identity") +
                theme_minimal()
        }
    })

    # Display summary statistics
    output$dataSummary <- renderPrint({
        data <- datasetInput()
        summary(data[, c(input$xVar, input$yVar)])
    })
}

# Run the app
shinyApp(ui, server)

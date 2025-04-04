library(shiny)
library(tidyverse)
library(DT)
library(plotly)
library(readxl)
library(shinythemes)
library(colourpicker)
library(shinyWidgets)

ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  titlePanel("Enhanced Interactive Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      fileInput("dataFile", "Upload your data (CSV, Excel, etc.)",
        accept = c(".csv", ".xlsx", ".xls")
      ),

      # Only show these panels after data is loaded
      conditionalPanel(
        condition = "output.dataLoaded",

        # Tabbed interface for control options
        tabsetPanel(
          tabPanel(
            "Plot Options",
            selectInput("xVar", "X Variable:", choices = NULL),
            selectInput("yVar", "Y Variable:", choices = NULL),
            selectInput("colorVar", "Color By (optional):", choices = NULL, selected = NULL),
            pickerInput("plotType", "Plot Type:",
              choices = c("Scatter Plot", "Box Plot", "Histogram", "Bar Chart", "Line Chart", "Density Plot"),
              selected = "Scatter Plot",
              choicesOpt = list(
                content = c(
                  "<i class='fa fa-circle-o'></i> Scatter Plot",
                  "<i class='fa fa-cube'></i> Box Plot",
                  "<i class='fa fa-bar-chart'></i> Histogram",
                  "<i class='fa fa-tasks'></i> Bar Chart",
                  "<i class='fa fa-line-chart'></i> Line Chart",
                  "<i class='fa fa-area-chart'></i> Density Plot"
                )
              )
            ),

            # Plot customization
            colourInput("plotColor", "Base Color", value = "#0072B2"),
            sliderInput("pointSize", "Point Size", min = 1, max = 10, value = 3),
            checkboxInput("smoothLine", "Add Trend Line", value = FALSE)
          ),
          tabPanel(
            "Filters",
            uiOutput("dynamicFilters") # Will be populated based on selected variables
          ),
          tabPanel(
            "Transformations",
            selectInput("transform", "Transform Y Variable:",
              choices = c("None", "Log", "Square Root", "Z-Score", "Min-Max Scaling"),
              selected = "None"
            )
          )
        ),

        # Download options
        downloadButton("downloadPlot", "Download Plot"),
        downloadButton("downloadData", "Download Filtered Data")
      )
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "Data Explorer",
          fluidRow(
            column(
              12,
              h3("Interactive Visualization"),
              plotlyOutput("dataPlotly", height = "500px"),
              hr(),
              h3("Data Preview"),
              DTOutput("dataTable")
            )
          )
        ),
        tabPanel(
          "Statistics",
          fluidRow(
            column(
              12,
              h3("Summary Statistics"),
              verbatimTextOutput("dataSummary"),
              hr(),
              conditionalPanel(
                condition = "output.hasNumericVars",
                h3("Correlation Analysis"),
                plotlyOutput("correlationPlot", height = "500px")
              )
            )
          )
        ),
        tabPanel(
          "About",
          fluidRow(
            column(
              12,
              h3("About This Tool"),
              p("This interactive data explorer allows you to upload and analyze your own datasets."),
              p("Built with Shiny, Plotly, and the tidyverse for R."),
              h4("Features:"),
              tags$ul(
                tags$li("Interactive visualizations with zoom, pan, and hover capabilities"),
                tags$li("Multiple plot types to explore different aspects of your data"),
                tags$li("Dynamic filtering based on variable types"),
                tags$li("Data transformations for better visualization"),
                tags$li("Summary statistics and correlation analysis"),
                tags$li("Download options for plots and filtered data")
              )
            )
          )
        )
      )
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

    if (ext == "csv") {
      read.csv(input$dataFile$datapath, stringsAsFactors = TRUE)
    } else if (ext %in% c("xls", "xlsx")) {
      readxl::read_excel(input$dataFile$datapath) %>%
        mutate(across(where(is.character), as.factor))
    }
  })

  # Transformed dataset
  transformedData <- reactive({
    req(datasetInput(), input$yVar, input$transform)

    data <- datasetInput()

    # Only apply transformations to numeric columns
    if (is.numeric(data[[input$yVar]])) {
      if (input$transform == "Log") {
        # Handle zeros and negative values
        if (min(data[[input$yVar]], na.rm = TRUE) <= 0) {
          data[[paste0(input$yVar, "_transformed")]] <- log(data[[input$yVar]] - min(data[[input$yVar]], na.rm = TRUE) + 1)
        } else {
          data[[paste0(input$yVar, "_transformed")]] <- log(data[[input$yVar]])
        }
      } else if (input$transform == "Square Root") {
        # Handle negative values
        if (min(data[[input$yVar]], na.rm = TRUE) < 0) {
          data[[paste0(input$yVar, "_transformed")]] <- sqrt(data[[input$yVar]] - min(data[[input$yVar]], na.rm = TRUE))
        } else {
          data[[paste0(input$yVar, "_transformed")]] <- sqrt(data[[input$yVar]])
        }
      } else if (input$transform == "Z-Score") {
        data[[paste0(input$yVar, "_transformed")]] <- scale(data[[input$yVar]])
      } else if (input$transform == "Min-Max Scaling") {
        data[[paste0(input$yVar, "_transformed")]] <- (data[[input$yVar]] - min(data[[input$yVar]], na.rm = TRUE)) /
          (max(data[[input$yVar]], na.rm = TRUE) - min(data[[input$yVar]], na.rm = TRUE))
      } else {
        data[[paste0(input$yVar, "_transformed")]] <- data[[input$yVar]]
      }
    } else {
      data[[paste0(input$yVar, "_transformed")]] <- data[[input$yVar]]
    }

    return(data)
  })

  # Filtered dataset
  filteredData <- reactive({
    req(transformedData())
    data <- transformedData()

    # Dynamic filtering will be applied here
    # This will be expanded based on the filter inputs

    return(data)
  })

  # Update variable selection choices when data is loaded
  observe({
    data <- datasetInput()

    # Get all column names
    all_vars <- names(data)

    # Identify numeric columns
    numeric_vars <- names(data)[sapply(data, is.numeric)]

    # Update selections
    updateSelectInput(session, "xVar", choices = all_vars)
    updateSelectInput(session, "yVar", choices = all_vars)

    # Include NULL option for the color variable
    updateSelectInput(session, "colorVar", choices = c("None" = "", all_vars))

    # Create dynamic filters based on variable types
    output$dynamicFilters <- renderUI({
      filter_inputs <- lapply(1:min(5, length(all_vars)), function(i) {
        var_name <- all_vars[i]

        if (is.numeric(data[[var_name]])) {
          # Numeric filter
          min_val <- min(data[[var_name]], na.rm = TRUE)
          max_val <- max(data[[var_name]], na.rm = TRUE)

          sliderInput(
            inputId = paste0("filter_", var_name),
            label = paste0("Filter by ", var_name),
            min = min_val,
            max = max_val,
            value = c(min_val, max_val)
          )
        } else if (is.factor(data[[var_name]]) || is.character(data[[var_name]])) {
          # Categorical filter
          checkboxGroupInput(
            inputId = paste0("filter_", var_name),
            label = paste0("Filter by ", var_name),
            choices = unique(data[[var_name]]),
            selected = unique(data[[var_name]])
          )
        }
      })

      # Return a div with all filters
      do.call(tagList, filter_inputs)
    })
  })

  # Data preview table
  output$dataTable <- renderDT({
    datatable(head(filteredData(), 10),
      options = list(scrollX = TRUE, pageLength = 5),
      rownames = FALSE
    ) %>%
      formatStyle(columns = names(filteredData()), fontSize = "90%")
  })

  # Signal that data is loaded
  output$dataLoaded <- reactive({
    return(!is.null(datasetInput()))
  })
  outputOptions(output, "dataLoaded", suspendWhenHidden = FALSE)

  # Check if dataset has numeric variables
  output$hasNumericVars <- reactive({
    data <- datasetInput()
    return(any(sapply(data, is.numeric)))
  })
  outputOptions(output, "hasNumericVars", suspendWhenHidden = FALSE)

  # Generate the interactive plot based on user selections
  output$dataPlotly <- renderPlotly({
    # Get the data
    data <- filteredData()

    # Determine Y variable name based on transformation
    y_var <- if (input$transform != "None") paste0(input$yVar, "_transformed") else input$yVar

    # Base plot title
    plot_title <- paste(input$plotType, "of", input$yVar, "vs", input$xVar)
    if (input$transform != "None") {
      plot_title <- paste0(plot_title, " (", input$transform, " transformed)")
    }

    # Create different plot types based on selection
    if (input$plotType == "Scatter Plot") {
      # Create base ggplot
      p <- ggplot(data, aes_string(x = input$xVar, y = y_var)) +
        theme_minimal(base_size = 14) +
        labs(title = plot_title)

      # Add color by variable if selected
      if (input$colorVar != "") {
        p <- ggplot(data, aes_string(x = input$xVar, y = y_var, color = input$colorVar)) +
          theme_minimal(base_size = 14) +
          labs(title = plot_title)
      } else {
        p <- p + geom_point(size = input$pointSize, color = input$plotColor)
      }

      # Add trend line if requested
      if (input$smoothLine) {
        p <- p + geom_smooth(method = "loess", se = TRUE)
      }
    } else if (input$plotType == "Box Plot") {
      p <- ggplot(data, aes_string(x = input$xVar, y = y_var)) +
        geom_boxplot(fill = input$plotColor) +
        theme_minimal(base_size = 14) +
        labs(title = plot_title)

      # Add color by if selected
      if (input$colorVar != "") {
        p <- ggplot(data, aes_string(x = input$xVar, y = y_var, fill = input$colorVar)) +
          geom_boxplot() +
          theme_minimal(base_size = 14) +
          labs(title = plot_title)
      }
    } else if (input$plotType == "Histogram") {
      p <- ggplot(data, aes_string(x = y_var)) +
        geom_histogram(fill = input$plotColor, color = "white", bins = 30) +
        theme_minimal(base_size = 14) +
        labs(title = paste("Histogram of", input$yVar))
    } else if (input$plotType == "Bar Chart") {
      # For bar charts, we need to summarize the data
      if (is.numeric(data[[y_var]])) {
        summary_data <- data %>%
          group_by_at(input$xVar) %>%
          summarise(mean_value = mean(!!sym(y_var), na.rm = TRUE)) %>%
          ungroup()

        p <- ggplot(summary_data, aes_string(x = input$xVar, y = "mean_value")) +
          geom_bar(stat = "identity", fill = input$plotColor) +
          theme_minimal(base_size = 14) +
          labs(title = paste("Mean", input$yVar, "by", input$xVar), y = paste("Mean", input$yVar))
      } else {
        # If not numeric, just count occurrences
        p <- ggplot(data, aes_string(x = input$xVar)) +
          geom_bar(fill = input$plotColor) +
          theme_minimal(base_size = 14) +
          labs(title = paste("Count of", input$xVar))
      }
    } else if (input$plotType == "Line Chart") {
      # For line charts, data should be ordered
      p <- ggplot(data, aes_string(x = input$xVar, y = y_var, group = 1)) +
        geom_line(color = input$plotColor, size = 1.5) +
        geom_point(color = input$plotColor, size = input$pointSize) +
        theme_minimal(base_size = 14) +
        labs(title = plot_title)
    } else if (input$plotType == "Density Plot") {
      p <- ggplot(data, aes_string(x = y_var)) +
        geom_density(fill = input$plotColor, alpha = 0.5) +
        theme_minimal(base_size = 14) +
        labs(title = paste("Density Plot of", input$yVar))

      # Add color by if selected
      if (input$colorVar != "") {
        p <- ggplot(data, aes_string(x = y_var, fill = input$colorVar)) +
          geom_density(alpha = 0.5) +
          theme_minimal(base_size = 14) +
          labs(title = paste("Density Plot of", input$yVar))
      }
    }

    # Convert to plotly for interactivity
    ggplotly(p, tooltip = "all") %>%
      layout(autosize = TRUE)
  })

  # Correlation plot for numeric variables
  output$correlationPlot <- renderPlotly({
    data <- datasetInput()

    # Keep only numeric columns
    numeric_data <- data[sapply(data, is.numeric)]

    # Need at least 2 numeric columns
    req(ncol(numeric_data) >= 2)

    # Calculate correlation matrix
    cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

    # Prepare data for plotting
    cor_data <- as.data.frame(as.table(cor_matrix))
    names(cor_data) <- c("Var1", "Var2", "Correlation")

    # Create heatmap
    p <- ggplot(cor_data, aes(x = Var1, y = Var2, fill = Correlation)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Correlation Matrix") +
      coord_fixed()

    ggplotly(p)
  })

  # Display summary statistics
  output$dataSummary <- renderPrint({
    req(input$xVar, input$yVar)
    data <- filteredData()

    # Get columns to summarize
    cols_to_summarize <- c(input$xVar, input$yVar)
    if (input$colorVar != "") {
      cols_to_summarize <- c(cols_to_summarize, input$colorVar)
    }

    # Add transformed variable if applicable
    if (input$transform != "None") {
      cols_to_summarize <- c(cols_to_summarize, paste0(input$yVar, "_transformed"))
    }

    # Filter to just the columns we care about
    summary_data <- data[, intersect(cols_to_summarize, names(data)), drop = FALSE]
    summary(summary_data)
  })

  # Download handlers
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("plot-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Create the plot
      p <- ggplotly(output$dataPlotly())
      plotly::save_image(p, file = file, width = 800, height = 600)
    }
  )

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered-data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filteredData(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)

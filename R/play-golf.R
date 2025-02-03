####################################
# TCHINDA JOEL DEBOSTON #
####################################

# Import libraries
library(shiny)
library(shinythemes)
library(data.table)
library(RCurl)
library(randomForest)

# Read data
weather <- read.csv(text = getURL("https://raw.githubusercontent.com/dataprofessor/data/master/weather-weka.csv"))
weather$play <- as.factor(weather$play)  # Ensure target is categorical

# Build model
model <- randomForest(play ~ ., data = weather, ntree = 500, mtry = 4, importance = TRUE)

####################################
# User Interface                   #
####################################
ui <- fluidPage(theme = shinytheme("united"),
                
                # Page header
                headerPanel('Play Golf?'),
                
                # Input values
                sidebarPanel(
                  HTML("<h3>Input parameters</h3>"),
                  
                  selectInput("outlook", label = "Outlook:", 
                              choices = list("Sunny" = "sunny", "Overcast" = "overcast", "Rainy" = "rainy"), 
                              selected = "Rainy"),
                  sliderInput("temperature", "Temperature:", min = 64, max = 86, value = 70),
                  sliderInput("humidity", "Humidity:", min = 65, max = 96, value = 90),
                  selectInput("windy", label = "Windy:", choices = list("Yes" = TRUE, "No" = FALSE), selected = TRUE),
                  
                  actionButton("submitbutton", "Submit", class = "btn btn-primary")
                ),
                
                mainPanel(
                  tags$label(h3('Status/Output')), # Status/Output Text Box
                  verbatimTextOutput('contents'),
                  tableOutput('tabledata') # Prediction results table
                )
)

####################################
# Server                           #
####################################
server <- function(input, output, session) {
  
  # Reactive function for prediction
  datasetInput <- reactive({
    req(input$submitbutton)  # Ensure button is clicked before proceeding
    
    # Create test data with correct factor levels
    test <- data.frame(
      outlook = factor(input$outlook, levels = levels(weather$outlook)),
      temperature = as.numeric(input$temperature),
      humidity = as.numeric(input$humidity),
      windy = as.logical(input$windy)
    )
    
    # Make prediction
    prediction <- predict(model, test)
    probabilities <- round(predict(model, test, type = "prob"), 3)
    
    # Return dataframe for output
    data.frame(Prediction = prediction, Probabilities = probabilities)
  })
  
  # Status Output
  output$contents <- renderPrint({
    if (input$submitbutton > 0) { 
      isolate("Calculation complete.") 
    } else {
      "Server is ready for calculation."
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    req(input$submitbutton)  # Ensure button click
    datasetInput()
  })
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)

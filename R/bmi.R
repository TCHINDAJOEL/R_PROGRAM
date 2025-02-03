############################################
# TCHINDA Joël Deboston#
############################################

library(shiny)
library(shinythemes)

####################################
# User Interface                   #
####################################
ui <- fluidPage(theme = shinytheme("united"),
                navbarPage("BMI Calculator:",
                           
                           tabPanel("Home",
                                    # Input values
                                    sidebarPanel(
                                      HTML("<h3>Input parameters</h3>"),
                                      sliderInput("height", 
                                                  label = "Height (cm):", 
                                                  value = 175, 
                                                  min = 40, 
                                                  max = 250),
                                      sliderInput("weight", 
                                                  label = "Weight (kg):", 
                                                  value = 70, 
                                                  min = 20, 
                                                  max = 200),
                                      
                                      actionButton("submitbutton", 
                                                   "Submit", 
                                                   class = "btn btn-primary")
                                    ),
                                    
                                    mainPanel(
                                      tags$label(h3('Status/Output')), # Status/Output Text Box
                                      verbatimTextOutput('contents'),
                                      verbatimTextOutput('bmi_status'),
                                      verbatimTextOutput('bmi_recommendation'),
                                      tableOutput('tabledata') # Results table
                                    ) # mainPanel()
                           ), #tabPanel(), Home
                           
                           tabPanel("About", 
                                    titlePanel("About"), 
                                    div(includeMarkdown("about.md"), 
                                        align="justify")
                           ) #tabPanel(), About
                ) # navbarPage()
) # fluidPage()

####################################
# Server                           #
####################################
server <- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    if (input$height <= 0 || input$weight <= 0) {
      return(data.frame(BMI = "Invalid input"))
    }
    
    bmi <- input$weight / ((input$height / 100) ^ 2)
    bmi_df <- data.frame(BMI = round(bmi, 2))
    return(bmi_df)
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton > 0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # BMI Status Text Box
  output$bmi_status <- renderPrint({
    if (input$submitbutton > 0) {
      bmi <- input$weight / ((input$height / 100) ^ 2)
      status <- ifelse(bmi < 18.5, "Underweight",
                       ifelse(bmi < 24.9, "Normal weight",
                              ifelse(bmi < 29.9, "Overweight", "Obese")))
      paste("Your BMI category is:", status)
    }
  })
  
  # BMI Recommendation
  output$bmi_recommendation <- renderPrint({
    if (input$submitbutton > 0) {
      bmi <- input$weight / ((input$height / 100) ^ 2)
      min_weight <- round(18.5 * ((input$height / 100) ^ 2), 1)
      max_weight <- round(24.9 * ((input$height / 100) ^ 2), 1)
      status <- ifelse(bmi < 18.5, "You may need to gain weight to reach a healthy BMI range.",
                       ifelse(bmi < 24.9, "You are in a healthy BMI range. Keep up your current lifestyle!",
                              ifelse(bmi < 29.9, "You may need to lose some weight for a healthier BMI.", "It's recommended to lose weight to improve your health.")))
      paste(status, "Your recommended weight range is between", min_weight, "kg and", max_weight, "kg.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton > 0) { 
      isolate(datasetInput()) 
    } 
  })
}

####################################
# Create Shiny App                 #
####################################
shinyApp(ui = ui, server = server)

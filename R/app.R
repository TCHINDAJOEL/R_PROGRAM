####################################
# TCHINDA JOEL DEBOSTON #
####################################

# Load R packages
library(shiny)
library(shinythemes)

# Define UI
ui <- fluidPage(
  theme = shinytheme("superhero"),
  navbarPage(
    "FIRST",
    
    tabPanel("Main page",
             sidebarPanel(
               tags$h3("Input:"),
               textInput("txt1", "Given Name:", ""),
               textInput("txt2", "Surname:", ""),
               actionButton("submit", "Submit"),
               br(), br(),
               verbatimTextOutput("txtout")
             ),
             
             mainPanel(
               h1("YOUR NAME"),
               textOutput("fullname")
             )
    ),
    
    tabPanel("Calculator",
             sidebarPanel(
               numericInput("num1", "Enter first number:", value = 0),
               numericInput("num2", "Enter second number:", value = 0),
               selectInput("operation", "Select operation:",
                           choices = c("Add" = "add",
                                       "Subtract" = "sub",
                                       "Multiply" = "mul",
                                       "Divide" = "div")),
               actionButton("calc", "Calculate"),
               br(), br(),
               verbatimTextOutput("calc_result")
             )
    ),
    
    tabPanel("Calendar",
             sidebarPanel(
               dateInput("date", "Select a date:", value = Sys.Date()),
               verbatimTextOutput("selected_date")
             )
    )
  )
)

# Define server function  
server <- function(input, output, session) {
  
  observeEvent(input$submit, {
    output$fullname <- renderText({
      if (input$txt1 == "" | input$txt2 == "") {
        return("Please enter both given name and surname.")
      }
      paste("Full Name:", input$txt1, input$txt2)
    })
  })
  
  observeEvent(input$calc, {
    result <- switch(input$operation,
                     "add" = input$num1 + input$num2,
                     "sub" = input$num1 - input$num2,
                     "mul" = input$num1 * input$num2,
                     "div" = ifelse(input$num2 == 0, "Cannot divide by zero", input$num1 / input$num2))
    
    output$calc_result <- renderText({ paste("Result:", result) })
  })
  
  output$selected_date <- renderText({
    paste("Selected Date:", input$date)
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)

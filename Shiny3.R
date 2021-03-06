#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  # *Input() functions,
  # App title ----
  titlePanel("Shiny Text"),
  
  # *Output() functions
  # Sidebar layout with an input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "dataset",
                  label = "Choose a dataset:",
                  choices = c("rock", "pressure", "cars")),
      
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   value = 10)
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),
      
      
      # Output: HTML table with requested number of observations ----
      tableOutput("view")
      
      
    )
  )
)


# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  
  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  
  # Show the first "n" observations ----
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
  
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)

 


# Run the application 
shinyApp(ui = ui, server = server)


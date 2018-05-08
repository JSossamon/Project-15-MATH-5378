#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

Cylinders <- mtcars$cyl
Transmission <- mtcars$am
Gears <- mtcars$gear

# Define UI for miles per gallon app ----
ui <- fluidPage(
  
   # Application title ----
   titlePanel("Miles Per Gallon"),
   
   # Sidebar layout with input and output definitions ----
   sidebarLayout(
     
     # Sidebar panel for inputs ---- 
     sidebarPanel(
       
       # Input: Selector for variable to plot against mpg ----
       selectInput("variable", "Variable:",
                 c("Cylinders" = "Cylinders",
                   "Transmission" = "Transmission",
                   "Gears" = "Gears")),
       
       # Input: Checkbox for whether outliers should be included ----
       checkboxInput("outliers", "Show outliers", TRUE)
       
       
    ),
      
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: formatted text for caption ----
      h3(textOutput("caption")),
      
      # Output: plot of the requested variable against mpg ----
      plotOutput("mpgPlot")
      
    )
  )
)

# For data pre-processing ----
# Tweak the "am" variable to have nicer factor labels;
# since this doesn't rely on any user inputs, we can do this
# just one time at startup and then use the value
# throughout the lifetime of the app.
mpgData <- mtcars


mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the 
  # output$caption and output$mpgPlot functions
  x <- "Miles per Gallon versus"
  
  formulaText <- reactive({
    paste(x,input$variable)
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  # Generate a plot of the requested variable against mpg ----
  # and only exclude outliers if requested
  output$mpgPlot <- renderPlot({
    boxplot(as.formula(formulaText()),
            data = mpgData,
            outline = input$outliers,
            col = "#75AADB", pch = 19,
            xlab = input$variable, ylab = "Miles Per Gallon",
            main = "Boxplot for Statistical Comparisons")
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)


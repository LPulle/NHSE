## If R version < 4.0 you need to set this option
options(stringsAsFactors = FALSE)


## Load packages - check if installed first - if not install them
if (!require(conflicted)) install.packages("conflicted")
library(conflicted)
conflicted::conflict_prefer("filter", "dplyr") # prefer dplyr for filter function
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
if (!require(rvest)) install.packages("rvest")
library(rvest)
if (!require(shiny)) install.packages("shiny")
library(shiny)

# Data pre-processing ----
# Tweak the "am" variable to have nicer factor labels -- since this
# doesn't rely on any user inputs, we can do this once at startup
# and then use the value throughout the lifetime of the app
Year <- seq(2011, 2050, by = 1)
Month <- seq(1, 12, by = 1)
Method <- c("web", "folder")

#mpgData <- mtcars
#mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))


# Define UI for miles per gallon app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Amb CSV Checker"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for variable to plot against mpg ----
      selectInput("Var1", "Method:", Method),
      selectInput("Var2", "Year:", Year),
      selectInput("Var3", "Month:", Month)
      
      # Input: Checkbox for whether outliers should be included ----
      #checkboxInput("outliers", "Show outliers", TRUE)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      
      # Output: Plot of the requested variable against mpg ----
      plotOutput("mpgPlot")
      
    )
  )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$mpgPlot functions
  formulaText <- reactive({
    paste("mpg ~", input$variable)
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
            col = "#75AADB", pch = 19)
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
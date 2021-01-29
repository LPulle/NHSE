## If R version < 4.0 you need to set this option
if(paste(R.Version()$major, R.Version()$minor, sep=".") < 4) 
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
## Functions and values for the input selectors
## input selectors
Extract <- c("ambsys", "ambco")
Year <- seq(2020, 2050, by = 1)
Month <- seq(1, 12, by = 1)

## Functions
## Find the latest amb url from website
readambweb <- function(x) {
  html <- xml2::read_html("https://www.england.nhs.uk/statistics/statistical-work-areas/ambulance-quality-indicators/")
    css_selector <- paste0(switch(x, ambco = "[href*='AmbCO']", ambsys = "[href*='AmbSYS']"), "[href$='.csv']")
    url <- html %>%
      html_node(css_selector) %>%
      html_attr("href")
    read.csv(url)
}

## Custom round function - R uses IEE 754 rules which we don't want
rnd <- function(x) trunc(x + sign(x) * 0.5)

#mpgData <- mtcars
#mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))


# Define UI for miles per gallon app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Amb CSV web checker"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for variable to plot against mpg ----
      selectInput("Var1", "Extract:", Extract),
      selectInput("Var2", "Year:", Year),
      selectInput("Var3", "Month:", Month),
      
      # Input: Checkbox for whether outliers should be included ----
      #checkboxInput("outliers", "Show outliers", TRUE)
      
      # Input: actionButton() to defer the rendering of output ----
      # until the user explicitly clicks the button (rather than
      # doing it immediately when inputs change). This is useful if
      # the computations required to render output are inordinately
      # time-consuming.
      actionButton("update", "Check CSV")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      textOutput("selected_var"),
          tableOutput("contents")
      
    )
  )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  # Note that we use eventReactive() here, which depends on
  # input$update (the action button), so that the output is only
  # updated when the user clicks the button
  output$selected_var <- renderText({ 
    paste("You have selected", input$Var1, "; Year:", input$Var2, "; Month:", input$Var3)
  })
  
  
  datasetInput <- eventReactive(input$update, {
    extract <- input$Var1
    yearno <- input$Var2
    monthno <- input$Var3
    readambweb(extract) %>% dplyr::filter(Year == yearno, Month == monthno)
  })
  
  output$contents <- renderTable({
    amb_filtered <- datasetInput()
    return(head(amb_filtered))
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
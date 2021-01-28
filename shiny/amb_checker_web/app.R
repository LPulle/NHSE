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
Extract <- c("ambsys", "ambco")
Year <- seq(2011, 2050, by = 1)
Month <- seq(1, 12, by = 1)

## Find the latest amb url from website
readambweb <- function(x) {
  html <- xml2::read_html("https://www.england.nhs.uk/statistics/statistical-work-areas/ambulance-quality-indicators/")
  if (extract %in% c("ambco", "ambsys")) {
    css_selector <- paste0(switch(x, ambco = "[href*='AmbCO']", ambsys = "[href*='AmbSYS']"), "[href$='.csv']")
    url <- html %>%
      html_node(css_selector) %>%
      html_attr("href")
    read.csv(url)
  } else {
  }
}

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
      selectInput("Var3", "Year:", Year),
      selectInput("Var4", "Month:", Month),
      
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
      
      # Output: Header + summary of distribution ----
      h4("Summary"),
      verbatimTextOutput("summary"),
      
      # Output: Header + table of distribution ----
      h4("Observations"),
      tableOutput("view")
      
    )
  )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  # Note that we use eventReactive() here, which depends on
  # input$update (the action button), so that the output is only
  # updated when the user clicks the button
  amb_filtered <- readambweb(extract) %>% dplyr::filter(Year == yearno, Month == monthno)
  
  #datasetInput <- eventReactive(input$update, {
  #  switch(input$dataset,
  #         "rock" = rock,
  #         "pressure" = pressure,
  #         "cars" = cars)
  #}, ignoreNULL = FALSE)
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Show the first "n" observations ----
  # The use of isolate() is necessary because we don't want the table
  # to update whenever input$obs changes (only when the user clicks
  # the action button)
  output$view <- renderTable({
    head(datasetInput(), n = isolate(input$obs))
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
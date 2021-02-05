library(shiny)

## Data for the input selectors
Extract <- c("ambsys", "ambco")

# Define UI  ----
shinyUI(fluidPage(
  # App title ----
  titlePanel("Amb CSV web checker"),


  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    fluidRow(
      column(
        width = 5, offset = 0,
        sidebarPanel(
          # Input: Selector for variable Extract ----
          selectInput("Var1", "Extract:", Extract),

          # Input: Checkbox for whether outliers should be included ----
          # checkboxInput("outliers", "Show outliers", TRUE)

          # Input: actionButton() to defer the rendering of output ----
          # until the user explicitly clicks the button (rather than
          # doing it immediately when inputs change). This is useful if
          # the computations required to render output are inordinately
          # time-consuming.
          actionButton("update", "Check CSV")
        )
      )
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Data file ----
      textOutput("Invalid"),
      tableOutput("contents"),
      width = 12
    )
  )
))

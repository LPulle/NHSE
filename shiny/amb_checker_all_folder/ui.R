library(shiny)

## Data for the input selectors
Extract <- c("ambsys", "ambco")

# Define UI  ----
shinyUI(fluidPage(
  # App title ----
  titlePanel("Amb CSV Folder Checker"),


  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    fluidRow(
      column(
        width = 5, offset = 0,
        sidebarPanel(
          # Input: Selector for variable Extract ----
          selectInput("Var1", "Extract:", Extract),

          # Input: Select a file ----
          fileInput("file1", "Choose CSV File",
            multiple = FALSE,
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          ),

          # Horizontal line ----
          tags$hr(),

          # Input: Checkbox if file has header ----
          checkboxInput("header", "Header", TRUE),

          # Input: Select separator ----
          radioButtons("sep", "Separator",
            choices = c(
              Comma = ",",
              Semicolon = ";",
              Tab = "\t"
            ),
            selected = ","
          ),

          # Input: Select quotes ----
          radioButtons("quote", "Quote",
            choices = c(
              None = "",
              "Double Quote" = '"',
              "Single Quote" = "'"
            ),
            selected = '"'
          ),

          # Horizontal line ----
          tags$hr(),
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

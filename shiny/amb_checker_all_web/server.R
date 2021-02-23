options(stringsAsFactors = FALSE)
## Load packages - check if installed first - if not install them
# if (!require(conflicted)) install.packages("conflicted")
library(conflicted)
conflicted::conflict_prefer("filter", "dplyr") # prefer dplyr for filter function
# if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
# if (!require(rvest)) install.packages("rvest")
library(rvest)
# if (!require(shiny)) install.packages("shiny")
library(shiny)

# Define server logic  ----
shinyServer(function(input, output) {

  # Return the requested dataset ----
  # Note that we use eventReactive() here, which depends on
  # input$update (the action button), so that the output is only
  # updated when the user clicks the button


  datasetInput <- eventReactive(input$update, {
    extract <- input$Var1
    processdata(extract)
  })


  ## Render a table using datasetInput as html and put it into output$contents
  output$contents <- renderTable(
    {
      amb_filtered <- datasetInput()
    },
    type = "html",
    bordered = TRUE,
    striped = TRUE,
    align = "c"
  )

  ## Count the invalid records and render as text into output$Invalid
  output$Invalid <- renderText({
    print(paste0(
      "There are ", sum(colSums(datasetInput()[, grep("false", names(datasetInput()))])),
      " invalid data item(s)"
    ))
  })
})

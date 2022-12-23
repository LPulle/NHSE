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
## Load some base data so the server functions can load
load(url("https://github.com/LPulle/NHSE-Analytics/blob/master/shiny/amb_checker_all_web/initialdata.rda?raw=true"))

## Data for the input selectors
Extract <- c("ambsys", "ambco")
Year <- seq(2020, 2050, by = 1)
Month <- seq(1, 12, by = 1)

## Custom function to Find the latest amb url from NHSE website
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

## Preload ambsys lookup tables
weightedlookup <- read.csv("https://raw.githubusercontent.com/LPulle/NHSE-Analytics/master/shiny/amb_checker_all_web/weightedlookup.csv")
meanlookup <- read.csv("https://raw.githubusercontent.com/LPulle/NHSE-Analytics/master/shiny/amb_checker_all_web/meanlookup.csv")
summedlookup <- read.csv("https://raw.githubusercontent.com/LPulle/NHSE-Analytics/master/shiny/amb_checker_all_web/summedlookup.csv")

## Custom round function - R uses IEE 754 rules which we don't want
rnd <- function(x) trunc(x + sign(x) * 0.5)

## Custom NumberOfDays function to get the last day of the month
NumberOfDays <- function(date) {
  return(as.numeric(format(as.Date(paste0(format(date, format = "%Y"), formatC(ifelse(format(date, format = "%m") == "12", 0, as.numeric(format(date, format = "%m"))) + 1, width = 2, format = "d", flag = "0"), "01"), "%Y%m%d") - 1, format = "%d")))
}

## Main Code as custom function
processdata <- function(extract) {
  amb <- readambweb(extract) %>% dplyr::filter(Year >= 2019)

## Remove commas from the numeric columns if there are any
metrics <- names(amb)
for(i in (6:length(metrics))) {amb[,i] <- gsub(",", "", amb[,i])}
	
  ## This is the full amb_checker_all script:

  ## Determine the periods required
  periods <- paste(amb$Year, amb$Month, sep = "-") %>% unique()
  periods1 <- periods

  ## Create data frame to hold test outputs
  output <- data.frame(
    Period = character(),
    SummedTrue = numeric(),
    SummedFalse = numeric(),
    MeanedTrue = numeric(),
    MeanedFalse = numeric(),
    WeightedTrue = numeric(),
    WeightedFalse = numeric(),
    RegionSummedTrue = numeric(),
    RegionSummedFalse = numeric(),
    RegionMeanedTrue = numeric(),
    RegionMeanedFalse = numeric(),
    RegionWeightedTrue = numeric(),
    RegionWeightedFalse = numeric(),
    SingleTrue = numeric(),
    SingleFalse = numeric(),
    OtherValFalse = numeric(),
    Dashes = numeric(),
    Records = numeric()
  )

  ## Loop through whole process for each period
  for (period in (1:(length(periods1)))) {
    yearmonth <- periods1[period]
    output[period, ] <- c(yearmonth, rep(0, each = 17))

    amb_filtered <- amb %>% dplyr::filter(paste(Year, Month, sep = "-") == yearmonth) %>% arrange(Year,Month,Org.Code)
    output$Records[period]<- nrow(amb_filtered)

    ## Check for dashes - if this is not zero needs to be fixed in the file
    dashes <- grep("–|-", amb_filtered)

    ## Convert strings to numeric and cleanse (first 5 columns not to be changed)
    amb_filtered[, -c(1:5)] <- suppressWarnings(apply(amb_filtered[, -c(1:5)], 2, as.numeric))
    amb_filtered[is.na(amb_filtered)] <- 0

    ## Identify region and all england rows
    regions <- grep("Y[0-9][0-9]", amb_filtered$Org.Code)
    england <- which(amb_filtered$Org.Code == "Eng")

    ## Identify columns which are to be weighted summed and meaned
    ## - There is no pattern for ambsys so has to be hard coded
    ## - Ambco we can use some logic to identify the weighted columns, then everything else is summed

    ## weighted
    if (extract == "ambco") {
      weighted <- union(
        which(substring(names(amb_filtered), nchar(names(amb_filtered)), nchar(names(amb_filtered))) == "m"),
        which(substring(
          names(amb_filtered), nchar(names(amb_filtered)) - 1,
          nchar(names(amb_filtered))
        ) %in% c("50", "90"))
      )
      weightedlookup <- data.frame(cbind(num = names(amb_filtered[, weighted]), denom = paste0(substring(names(amb_filtered[, weighted]), 1, 2), "n")))
    } else if (extract == "ambsys") {
      weightedlookup <- weightedlookup
      weighted <- which(names(amb_filtered) %in% weightedlookup$num)
    } else {
      print("Unknown Parameter: Extract")
    }

    ## summed
    if (extract == "ambco") {
      summed <- which(names(amb_filtered) %in% names(amb_filtered)[-weighted][-c(1:5)])
    } else if (extract == "ambsys") {
	summedlookup <- summedlookup
      summed <- which(names(amb_filtered) %in% summedlookup$x)
    } else {
      print("Unknown Parameter: Extract")
    }

    ## mean - this only applies to ambsys
    meanlookup <- meanlookup
    meaned <- which(names(amb_filtered) %in% meanlookup$metric)


    ## Map trusts to regions
    RegionMap <- cbind(
      amb_filtered[-regions, c("Org.Code", "Org.Name")],
      amb_filtered[match(amb_filtered$Region, amb_filtered$Org.Code), c("Org.Code", "Org.Name")] %>%
        dplyr::filter(!is.na(Org.Code))
    )
    names(RegionMap)[c(3:4)] <- c("Region.Code", "Region.Name")

    ## Regions with more than 1 trust
    Regions.gt.1trust <- data.frame(sort(table(RegionMap$Region.Name), decreasing = T)) %>% dplyr::filter(Freq > 1)
    rc <- unique(RegionMap$Region.Code[which(RegionMap$Region.Name %in% Regions.gt.1trust$Var1)])

    ## Single regions
    RegionMapSingle <- RegionMap[-which(RegionMap$Region.Code %in% rc), ][-england, ]


    ## Run calculations to compare trust data to england
    ## Summed Columns
    amb_filtered_summed <- cbind(
      data.frame(total = apply(amb_filtered[-regions, summed][-england, ], 2, sum)),
      t(amb_filtered[england, c(summed)])
    )
    names(amb_filtered_summed)[2] <- "England"

    ## meaned columns
    if (extract == "ambsys") {
      amb_filtered_meaned <- sapply(c(1:length(meaned)), function(x) {
        sum(amb_filtered[-regions, meanlookup$num[
          match(
            names(amb_filtered[-regions, meaned][-england, ]),
            meanlookup$metric
          )
        ]][-england, ][, x]) / sum(amb_filtered[-regions, meanlookup$denom[
          match(
            names(amb_filtered[-regions, meaned][-england, ]),
            meanlookup$metric
          )
        ]][-england, ][, x])
      }) %>%
        rnd() %>%
        data.frame() %>%
        cbind(., t(amb_filtered[england, meaned]))
      names(amb_filtered_meaned) <- c("total", "England")
    } else { # do nothing
    }

    ## weighted columns
    ## Do the weighted average by sum product for each trust
    weightedt <- sapply(c(1:length(weighted)), function(x) {
      sum(amb_filtered[-regions, weighted][-england, ][, x] *
        amb_filtered[-regions, weightedlookup$denom[match(
          names(amb_filtered[-regions, weighted][-england, ]),
          weightedlookup$num
        )]][-england, ][, x])
    })
    weightede <- as.numeric(amb_filtered[england, weightedlookup$denom[match(names(amb_filtered[england, weighted]), weightedlookup$num)]])
    ## divide by the england figure and combine calculated and value in download into data frame
    if (extract == "ambsys") {
      amb_filtered_weighted <- cbind(data.frame(total = rnd(weightedt / weightede)), t(amb_filtered[england, weighted]))
    } else {
      amb_filtered_weighted <- cbind(data.frame(total = weightedt / weightede), t(amb_filtered[england, weighted]))
    }
    names(amb_filtered_weighted)[2] <- "England"
    ## old periods don't have the weighted metrics - set to 0 if they error out
    amb_filtered_weighted[is.na(amb_filtered_weighted)] <- 0

    ## Loop through each region with more than 1 trust for summed columns then repeat for the weighted ones
    mylist <- vector("list", length = length(rc)) # summed
    mylistw <- vector("list", length = length(rc)) # weighted
    mylistm <- vector("list", length = length(rc)) # meaned

    i <- 2
    for (i in (1:length(rc))) {
      j <- rc[i]
      k <- which(amb_filtered$Org.Code %in% RegionMap$Org.Code[which(RegionMap$Region.Code == rc[i])])
      m <- which(amb_filtered$Org.Code == j)

      ## summed columns
      mylist[[i]] <- cbind(data.frame(total = apply(amb_filtered[k, summed], 2, sum)), region = t(amb_filtered[m, summed]))

      ## meaned columns - only for ambsys
      if (extract == "ambsys") {
        mylistm[[i]] <- sapply(c(1:length(meaned)), function(x) {
          sum(amb_filtered[k, meanlookup$num[
            match(
              names(amb_filtered[k, meaned]),
              meanlookup$metric
            )
          ]][, x]) / sum(amb_filtered[k, meanlookup$denom[
            match(
              names(amb_filtered[k, meaned]),
              meanlookup$metric
            )
          ]][, x])
        }) %>%
          rnd() %>%
          data.frame() %>%
          cbind(., t(amb_filtered[m, meaned]))
      } else { # do nothing
      }

      ## weighted columns
      weightedew <- sapply(c(1:length(weighted)), function(x) {
        sum(amb_filtered[k, weighted][, x] * amb_filtered[k, weightedlookup$denom[match(
          names(amb_filtered[k, weighted]),
          weightedlookup$num
        )]][, x])
      })
      weightedtw <- as.numeric(amb_filtered[m, weightedlookup$denom[match(names(amb_filtered[m, weighted]), weightedlookup$num)]])
      mylistw[[i]] <- if (extract == "ambsys") {
        cbind(rnd(weightedew / weightedtw), t(amb_filtered[m, weighted]))
      } else {
        cbind(weightedew / weightedtw, t(amb_filtered[m, weighted]))
      }
    }

    # rm(i,j,k,m)

    ## add names to each list for all levels
    names(mylist) <- rc
    mylist <- lapply(seq_along(mylist), function(i) {
      colnames(mylist[[i]]) <- c(names(mylist)[i], "Region")
      mylist[[i]]
    })

    if (extract == "ambsys") {
      names(mylistm) <- rc
      mylistm <- lapply(seq_along(mylistm), function(i) {
        colnames(mylistm[[i]]) <- c(names(mylistm)[i], "Region")
        mylistm[[i]]
      })
    } else { # do nothing
    }

    names(mylistw) <- rc
    mylistw <- lapply(seq_along(mylistw), function(i) {
      colnames(mylistw[[i]]) <- c(names(mylistw)[i], "Region")
      mylistw[[i]]
    })

    ## patch NaN with 0
    mylistw <- rapply(mylistw, f = function(x) ifelse(is.nan(x), 0, x), how = "replace")
    
    ## additional validation checks for inconsistencies
    if(extract == "ambco") {
      ## If ambco
      valuesgtlt <- data.frame(
        num =
          c(
            "P1b","K4b","M4b","M3n","R3s","R4s","R1r","R2r","R5b"
          ),
        denom =
          c(
            "P1n","K4n","M4n","M1n","R3n","R4n","R1n","R2n","R5n"
          )
      )
      
      ## Create a list object mylistgtlt
      mylistgtlt <- vector("list", length = nrow(valuesgtlt)) # greaterthanlessthan
      
      ## Loop through valuesgtlt
      i <- 1
      for (i in (1:nrow(valuesgtlt))) {
        num <- valuesgtlt$num[i]
        denom <- valuesgtlt$denom[i]
        numi <- which(names(amb_filtered) == num)
        denomi <- which(names(amb_filtered) == denom)
        mylistgtlt[[i]] <- which(amb_filtered[,numi] > amb_filtered[,denomi])
      }
      
      ## Set gtltv to length of mylistlt[[1]]
      gtltv <- length(mylistgtlt[[1]])
      
      ## Pick out the values in mylistgtlt and cat them into gtltv then sum the total
      for (j in (2:nrow(valuesgtlt))) {
        gtltv <- c(gtltv, length(mylistgtlt[[j]]))
        gtltv <- sum(gtltv)
      }
      
    } else {
      ## If ambsys
      # we don't have equivalences for ambsys but we have a list of items that should equal
      # each other
      if(extract == "ambsys") {
        xyz <- 0
        
        ## A7 = A17+A56
        xyz <- amb_filtered[which(amb_filtered$A7 != amb_filtered$A17+amb_filtered$A56), c(
          "Year", "Month", "Org.Code", "A7", "A17", "A56")] %>% 
          mutate(Total=A17+A56) %>% nrow()
        
        ## A8 = A115+A74+A78
        xyz <- xyz + amb_filtered[which(
          amb_filtered$A8 != amb_filtered$A115+amb_filtered$A74+amb_filtered$A78 && 
            amb_filtered$A74+amb_filtered$A78 > 0), 
          c("Year", "Month", "Org.Code", "A8", "A115", "A74", "A78")] %>%
          mutate(Total = A115+A74+A78) %>% nrow()
        
        ## A10 = A119+A75+A79
        xyz <- xyz + amb_filtered[which(
          amb_filtered$A10 != amb_filtered$A119+amb_filtered$A75+amb_filtered$A79 && 
            amb_filtered$A119+amb_filtered$A75+amb_filtered$A79 > 0 
        )
        , c("Year", "Month", "Org.Code", "A10", "A119", "A75", "A79")] %>%
          mutate(Total = A119+A75+A79) %>% nrow()
        
        ## A24 = A116+A82+A94
        xyz <- xyz+ amb_filtered[which(
          amb_filtered$A24 != amb_filtered$A116+amb_filtered$A82+amb_filtered$A94 && 
            amb_filtered$A116+amb_filtered$A82+amb_filtered$A94 > 0 
        )
        , c("Year", "Month", "Org.Code", "A24", "A116", "A82", "A94")] %>%
          mutate(Total = A116+A82+A94) %>% nrow()
        
        ## A30 = A120+A85+A97
        xyz <- xyz+amb_filtered[which(
          amb_filtered$A30 != amb_filtered$A120+amb_filtered$A85+amb_filtered$A97 && 
            amb_filtered$A120+amb_filtered$A85+amb_filtered$A97 > 0 
        )
        , c("Year", "Month", "Org.Code", "A30", "A120", "A85", "A97")] %>%
          mutate(Total = A120+A85+A97) %>% nrow()
        
        ## A56 = A53+A54+A55
        xyz <- xyz+amb_filtered[which(
          amb_filtered$A56 != amb_filtered$A53+amb_filtered$A54+amb_filtered$A55 && 
            amb_filtered$A53+amb_filtered$A54+amb_filtered$A55 > 0 
        )
        , c("Year", "Month", "Org.Code", "A56", "A53", "A54", "A55")] %>%
          mutate(Total = A53+A54+A55) %>% nrow()
        
        ## A17 = A18+A19+A21+A22
        xyz <- xyz+amb_filtered[which(
          amb_filtered$A17 != amb_filtered$A18+amb_filtered$A19+amb_filtered$A21+amb_filtered$A22 && 
            amb_filtered$A18+amb_filtered$A19+amb_filtered$A21+amb_filtered$A22 > 0 
        )
        , c("Year", "Month", "Org.Code", "A17", "A18", "A19", "A21", "A22")] %>%
          mutate(Total = A18+A19+A21+A22) %>% nrow()
        gtltv <- xyz
      }
    }
    
    ## gtltv may break in the future so set it to 0 for now
    ## comment this out if it is neded again
    gtltv <- 0

    ####################################
    ## Tests
    ####################################

    ## test summed columns = england
    output[period, 2] <-
      data.frame(table(amb_filtered_summed$total == amb_filtered_summed$England)) %>%
      filter(Var1 == T) %>%
      select(Freq)
    output[period, 3] <-
      if (nrow(data.frame(table(amb_filtered_summed$total == amb_filtered_summed$England)) %>%
        filter(Var1 == F)) > 0) {
        data.frame(table(amb_filtered_summed$total == amb_filtered_summed$England)) %>%
          filter(Var1 == F) %>%
          select(Freq)
      } else {
        0
      }

    ## test meaned columns = england
    output[period, 4] <-
      if (extract == "ambsys") {
        data.frame(table(amb_filtered_meaned[, 1] == amb_filtered_meaned[, 2])) %>%
          filter(Var1 == T) %>%
          select(Freq)
      } else {
        0
      }

    output[period, 5] <-
      if (extract == "ambsys") {
        if (nrow(data.frame(table(amb_filtered_meaned[, 1] == amb_filtered_meaned[, 2])) %>%
          filter(Var1 == F)) > 0) {
          data.frame(table(amb_filtered_meaned[, 1] == amb_filtered_meaned[, 2])) %>%
            filter(Var1 == F) %>%
            select(Freq)
        } else {
          0
        }
      } else {
        0
      }


    ## test weighted columns = england rounded to 0 dps for ambsys use all.equal function for ambco
    if (extract == "ambsys") {
      output[period, 6] <-
        data.frame(table(rnd(amb_filtered_weighted$total) == rnd(amb_filtered_weighted$England))) %>%
        filter(Var1 == T) %>%
        select(Freq)
    } else {
      output[period, 6] <-
        # data.frame(table(format(amb_filtered_weighted$total, scientific = T, digits = 7) == format(amb_filtered_weighted$England, scientific = T, digits = 7))) %>%
        data.frame(table(mapply(function(x, y) {
          isTRUE(all.equal(x, y))
        }, amb_filtered_weighted$total, amb_filtered_weighted$England))) %>%
        filter(Var1 == T) %>%
        select(Freq)
    }

    if (extract == "ambsys") {
      output[period, 7] <-
        if (nrow(data.frame(table(rnd(amb_filtered_weighted$total) == rnd(amb_filtered_weighted$England))) %>%
          filter(Var1 == F)) > 0
        ) {
          data.frame(table(rnd(amb_filtered_weighted$total) == rnd(amb_filtered_weighted$England))) %>%
            filter(Var1 == F) %>%
            select(Freq)
        } else {
          0
        }
    } else {
      output[period, 7] <-
        # if (nrow(data.frame(table(format(amb_filtered_weighted$total, scientific = T, digits = 7) == format(amb_filtered_weighted$England, scientific = T, digits = 7))) %>%
        if (nrow(data.frame(table(mapply(function(x, y) {
          isTRUE(all.equal(x, y))
        }, amb_filtered_weighted$total, amb_filtered_weighted$England))) %>%
          filter(Var1 == F)) > 0) {
          # data.frame(table(format(amb_filtered_weighted$total, scientific = T, digits = 7) == format(amb_filtered_weighted$England, scientific = T, digits = 7))) %>%
          data.frame(table(mapply(function(x, y) {
            isTRUE(all.equal(x, y))
          }, amb_filtered_weighted$total, amb_filtered_weighted$England))) %>%
            filter(Var1 == F) %>%
            select(Freq)
        } else {
          0
        }
    }

    ## test summed columns match region
    testregions <- data.frame(lapply(c(1:length(mylist)), function(i) mylist[[i]][, 1] == mylist[[i]][, 2]))
    names(testregions) <- rc # c("Region1", "Region2", "Region3")
    output[period, 8] <-
      data.frame(table(t(testregions))) %>%
      filter(Var1 == T) %>%
      select(Freq)
    
    output[period, 9] <-
      if (nrow(data.frame(table(t(testregions))) %>%
               filter(Var1 == F)) > 0) {
        data.frame(table(t(testregions))) %>%
          filter(Var1 == F) %>%
          select(Freq)
      } else {
        0
      }
    
    ## test meaned columns match region
    if (extract == "ambsys") {
      testregionsm <- data.frame(lapply(c(1:length(mylistm)), function(i) mylistm[[i]][, 1] == mylistm[[i]][, 2]))
      names(testregionsm) <- rc # c("Region1", "Region2", "Region3")
      output[period, 10] <-
        data.frame(table(t(testregionsm))) %>%
        filter(Var1 == T) %>%
        select(Freq)
      output[period, 11] <-
        if (nrow(data.frame(table(t(testregionsm))) %>%
                 filter(Var1 == F)) > 0) {
          data.frame(table(t(testregionsm))) %>%
            filter(Var1 == F) %>%
            select(Freq)
        } else {
          0
        }
    } else {
      0
    }
    
    
    testregionsw <- data.frame(lapply(
      c(1:length(mylistw)),
      function(i) {
        mapply(function(x, y) {
          isTRUE(all.equal(x, y))
        }, mylistw[[i]][, 1], mylistw[[i]][, 2])
      }
    ))
    
    names(testregionsw) <- rc # c("Region1", "Region2", "Region3")
    output[period, 12] <-
      data.frame(table(t(testregionsw))) %>%
      filter(Var1 == T) %>%
      select(Freq)
    
    output[period, 13] <-
      if (nrow(data.frame(table(t(testregionsw))) %>%
               filter(Var1 == F)) > 0) {
        data.frame(table(t(testregionsw))) %>%
          filter(Var1 == F) %>%
          select(Freq)
      } else {
        0
      }
    
    ## test single trust to single region
    RegionMapSingle <- RegionMap[-which(RegionMap$Region.Code %in% rc), ][-england, ]
    SingleRegion <- amb_filtered[which(amb_filtered$Org.Code %in% RegionMapSingle$Org.Code), -c(1:4)] %>%
      arrange(Org.Name) %>%
      select(-1) == amb_filtered[which(amb_filtered$Org.Code %in% RegionMapSingle$Region.Code), -c(1:4)] %>%
      arrange(Org.Name) %>%
      select(-1)

    output[period, 14] <- data.frame(`TRUE` = length(which(SingleRegion == T)))
    output[period, 15] <- data.frame(`FALSE` = length(which(SingleRegion == F)))
    output[period, 16] <- sum(gtltv)
  }

  output[period, 17] <- if (length(dashes) == 0) 0 else length(dashes)

  ## reformat outpput and check false columns total
  fc <- grep("false", names(output))
  output[, -1] <- suppressWarnings(apply(output[, -1], 2, as.integer))

  output$Period <- as.Date(paste(output$Period,
    NumberOfDays(as.Date(paste(output$Period, "01", sep = "-"), "%Y-%m-%d")),
    sep = "-"
  ), "%Y-%m-%d") %>% as.character()

  output <- output %>%
    arrange(desc(Period)) %>%
    data.frame()

  ## Return output to the go into output$datasetInput
  return(output)
}

# Define UI  ----
ui <- fluidPage(
  # App title ----
  titlePanel("Amb CSV web checker"),


  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    fluidRow(
      column(
        width = 5, offset = 0,
        sidebarPanel(
          # Input: Selector for Extract ----
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
      # textOutput("dashes"),
      tableOutput("contents"),
      width = 12
    )
  )
)

# Define server logic  ----
server <- function(input, output) {

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
      amb_filtered <- datasetInput()[,c(1,3,5,7,9,11,13,15,16,17,18)]
    },
    type = "html",
    bordered = TRUE,
    striped = TRUE,
    align = "c"
  )

  ## Count the invalid records and render as text into output$Invalid
  output$Invalid <- renderText({
    print(paste0(
      "There are ", sum(colSums(datasetInput()[, grep("False", names(datasetInput()))])),
      " invalid data item(s)"
    ))
  })
}

# Create Shiny app ----
shinyApp(ui, server)

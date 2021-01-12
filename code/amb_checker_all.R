## If R version < 4.0 you need to set this option
options(stringsAsFactors = FALSE)
## Clear Existing Objects
## This is lazy but ensures everything is clear when we run
rm(list = ls(all = T))

## Load packages - check if installed first - if not install them
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
if (!require(rvest)) install.packages("rvest")
library(rvest)
detach("package:dplyr")
library(dplyr) # ensure dplyr is loaded last in case i forget to prefix the filter function

## Custom round function - R uses IEE 754 rules which we don't want
rnd <- function(x) trunc(x + sign(x) * 0.5)

## Parameters - change these and will cascade through
extract <- "ambco" # ambco or ambsys
method <- "web" # web or folder
folder <- "folderpath here" # can leave blank if using web method

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

## Get the data
if (method == "web") {
  ## Read the csv from web using function and filter
  amb <- readambweb(extract)
} else if (method == "folder") {
  ## Read the whole time series csv from file
  amb <- read.csv(paste0(folder, extract, "_filtered.csv"))
} else {
  print("Unknown Parameter: Method")
}

## Convert strings to numeric and cleanse (first 5 columns not to be changed)
amb[, -c(1:5)] <- suppressWarnings(apply(amb[, -c(1:5)], 2, as.numeric))
amb[is.na(amb)] <- 0

## Determine the periods required
periods <- paste(amb$Year, amb$Month, sep = "-") %>% unique()
periods1 <- periods

## Create data frame to hold test outputs
output <- data.frame(
  Period = character(),
  test1true = numeric(),
  test1false = numeric(),
  test2true = numeric(),
  test2false = numeric(),
  test3true = numeric(),
  test3false = numeric(),
  test4true = numeric(),
  test4false = numeric(),
  test5true = numeric(),
  test5false = numeric(),
  test6true = numeric(),
  test6false = numeric(),
  test7true = numeric(),
  test7false = numeric()
)

## Loop through whole process for each period
for (period in (1:(length(periods1)))) {
  yearmonth <- periods1[period]
  output[period, ] <- c(yearmonth, rep(0, each = 14))

  amb_filtered <- amb %>% dplyr::filter(paste(Year, Month, sep = "-") == yearmonth)

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
    weightedlookup <- data.frame(
      num =
        c(
          "A4", "A5", "A6", "A16", "A26", "A29", "A32", "A35", "A38", "A52", "A84", "A87",
          "A90", "A93", "A96", "A99", "A102", "A105", "A109", "A114", "A118", "A122"
        ),
      denom =
        c(
          "A1", "A1", "A1", "A13", "A8", "A9", "A10", "A11", "A12", "A49", "A74", "A75",
          "A76", "A77", "A78", "A79", "A80", "A81", "A106", "A1", "A115", "A119"
        )
    )
    weighted <- which(names(amb_filtered) %in% weightedlookup$num)
  } else {
    print("Unknown Parameter: Extract")
  }

  ## summed
  if (extract == "ambco") {
    summed <- which(names(amb_filtered) %in% names(amb_filtered)[-weighted][-c(1:5)])
  } else if (extract == "ambsys") {
    summed <- which(names(amb_filtered) %in%
      c(
        "A0", "A1", "A2", "A7", "A8", "A9", "A10", "A11", "A12", "A13", "A14", "A18", "A19", "A20", "A21", "A22", "A23", "A24", "A27", "A30",
        "A33", "A36", "A49", "A50", "A53", "A56", "A57", "A58", "A59", "A60", "A61", "A62", "A65", "A68", "A71", "A74", "A75", "A76", "A77", "A78",
        "A79", "A80", "A81", "A82", "A85", "A88", "A91", "A94", "A97", "A100", "A103", "A106", "A107", "A110", "A111", "A112", "A113", "A115", "A116",
        "A119", "A120", "A17", "A39", "A40", "A41", "A42", "A43", "A44", "A45", "A46", "A47", "A48", "A54", "A55"
      ))
  } else {
    print("Unknown Parameter: Extract")
  }

  ## mean - this only applies to ambsys
  meanlookup <- data.frame(
    metric =
      c(
        "A3", "A15", "A25", "A28", "A31", "A34", "A37", "A51", "A63", "A66", "A69", "A72", "A83", "A86", "A89", "A92", "A95", "A98",
        "A101", "A104", "A108", "A117", "A121"
      ),
    num =
      c(
        "A2", "A14", "A24", "A27", "A30", "A33", "A36", "A50", "A62", "A65", "A68", "A71", "A82", "A85", "A88", "A91", "A94", "A97",
        "A100", "A103", "A107", "A116", "A120"
      ),
    denom =
      c(
        "A1", "A13", "A8", "A9", "A10", "A11", "A12", "A49", "A58", "A59", "A60", "A61", "A74", "A75", "A76", "A77", "A78", "A79",
        "A80", "A81", "A106", "A115", "A119"
      )
  )
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
      #data.frame(table(format(amb_filtered_weighted$total, scientific = T, digits = 7) == format(amb_filtered_weighted$England, scientific = T, digits = 7))) %>%
      data.frame(table(mapply(function(x, y) {isTRUE(all.equal(x, y))}, amb_filtered_weighted$total, amb_filtered_weighted$England))) %>%
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
      #if (nrow(data.frame(table(format(amb_filtered_weighted$total, scientific = T, digits = 7) == format(amb_filtered_weighted$England, scientific = T, digits = 7))) %>%
      if (nrow(data.frame(table(mapply(function(x, y) {isTRUE(all.equal(x, y))}, amb_filtered_weighted$total, amb_filtered_weighted$England))) %>%
               filter(Var1 == F)) > 0) {
        #data.frame(table(format(amb_filtered_weighted$total, scientific = T, digits = 7) == format(amb_filtered_weighted$England, scientific = T, digits = 7))) %>%
        data.frame(table(mapply(function(x, y) {isTRUE(all.equal(x, y))}, amb_filtered_weighted$total, amb_filtered_weighted$England))) %>%
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
    data.frame(table(rbind(testregions[, 1], testregions[, 2], testregions[, 3]))) %>%
    filter(Var1 == T) %>%
    select(Freq)

  output[period, 9] <-
    if (nrow(data.frame(table(rbind(testregions[, 1], testregions[, 2], testregions[, 3]))) %>%
      filter(Var1 == F)) > 0) {
      data.frame(table(rbind(testregions[, 1], testregions[, 2], testregions[, 3]))) %>%
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
      data.frame(table(rbind(testregionsm[, 1], testregionsm[, 2], testregionsm[, 3]))) %>%
      filter(Var1 == T) %>%
      select(Freq)
    output[period, 11] <-
      if (nrow(data.frame(table(rbind(testregionsm[, 1], testregionsm[, 2], testregionsm[, 3]))) %>%
        filter(Var1 == F)) > 0) {
        data.frame(table(rbind(testregionsm[, 1], testregionsm[, 2], testregionsm[, 3]))) %>%
          filter(Var1 == F) %>%
          select(Freq)
      } else {
        0
      }
  } else {
    0
  }

  ## test weighted columns match region

  #testregionsw <- data.frame(lapply(
  #  c(1:length(mylistw)),
  #  function(i) 
  #    format(mylistw[[i]][, 1], scientific = T, digits = 7) == format(mylistw[[i]][, 2], scientific = T, digits = 7)
  #))
  
  testregionsw <- data.frame(lapply(
    c(1:length(mylistw)),
    function(i) 
    mapply(function(x, y) {isTRUE(all.equal(x, y))}, mylistw[[i]][, 1], mylistw[[i]][, 2])
  ))

  names(testregionsw) <- rc # c("Region1", "Region2", "Region3")
  output[period, 12] <-
    data.frame(table(rbind(testregionsw[, 1], testregionsw[, 2], testregionsw[, 3]))) %>%
    filter(Var1 == T) %>%
    select(Freq)

  output[period, 13] <-
    if (nrow(data.frame(table(rbind(testregionsw[, 1], testregionsw[, 2], testregionsw[, 3]))) %>%
      filter(Var1 == F)) > 0) {
      data.frame(table(rbind(testregionsw[, 1], testregionsw[, 2], testregionsw[, 3]))) %>%
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
}

## Final output sorted by period
output %>%
  arrange(1) %>%
  data.frame()

## reformat outpput and check false columns total
fc <- grep("false", names(output))
output[, -1] <- suppressWarnings(apply(output[, -1], 2, as.numeric))
sum(colSums(output[, fc]))

## If there are any rows where a false test > 0 you should run that period manually using amb_checker.R

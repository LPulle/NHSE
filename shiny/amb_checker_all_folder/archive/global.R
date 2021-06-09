options(stringsAsFactors = FALSE)
# Data pre-processing ----
## Load some base data so the server functions can load
load("/srv/shiny-server/amb_checker_all_folder/initialdata.rda")

## Data for the input selectors
Extract <- c("ambsys", "ambco")

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

## Custom round function - R uses IEE 754 rules which we don't want
rnd <- function(x) trunc(x + sign(x) * 0.5)

## Custom NumberOfDays function to get the last day of the month
NumberOfDays <- function(date) {
  return(as.numeric(format(as.Date(paste0(format(date, format = "%Y"), formatC(ifelse(format(date, format = "%m") == "12", 0, as.numeric(format(date, format = "%m"))) + 1, width = 2, format = "d", flag = "0"), "01"), "%Y%m%d") - 1, format = "%d")))
}

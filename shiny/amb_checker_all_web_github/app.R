## Wrapper for app.R on github
## This is what will get published and be run
source('https://raw.githubusercontent.com/LPulle/NHSE-Analytics/master/shiny/amb_checker_all_web_github/app_source.R')
shinyApp(ui, server)

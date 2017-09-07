#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)

source("../utils_compare_credit_calc.R")

TestListPaths <- list(list(Source = 'NPPD_NPPD', Sink = 'SPRM_SPRM'),
                      list(Source = 'SPRM_SPRM', Sink = 'NPPD_NPPD'),
                      list(Source = 'AECI', Sink = 'SPA'),
                      list(Source = 'SPA', Sink = 'AECI'),
                      list(Source = 'GRDA.GREC3', Sink = 'WR.NSW'),
                      list(Source = 'WR.NSW', Sink = 'GRDA.GREC3'),
                      list(Source = 'GRDA.GREC3', Sink = 'NPPD_NPPD'),
                      list(Source = 'NPPD_NPPD', Sink = 'GRDA.GREC3')
)

# getting the cached data
lstResults <- list()
lstResults2017 <- readRDS('../lstCompareCreditCalc2017.RDS')
lstResults2016 <- readRDS('../lstCompareCreditCalc2016.RDS')
lstResults <- c(lstResults2017, lstResults2016)

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Comparing the Credit Calculation to the Proposed Calculation"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
        selectInput("uiPeriod", "Period", choices = c("Jun_16", "Jul_16", "Aug_16", "Sep_16", "Fall_16", "Winter_16", "Spring_17",
                                                      "Jun_17", "Jul_17", "Aug_17"),
                           multiple = FALSE),
        selectInput("uiOnOrOff", "Time Of Use", choices = c("OFF", "ON"),
                    multiple = FALSE),
        selectInput("uiPathSelect", "List of Paths", choices = c("Test", "Annual Process"))
      ),

      # Show a plot of the generated distribution
      mainPanel(

         plotOutput("plotCompare", brush = "plotCompare_brush"),
         rhandsontable::rHandsontableOutput("tblBrushed"),
         plotOutput("plotPercentage")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  reactLstData <- reactive({
    periodName <- input[['uiPeriod']]
    onOrOff <- input[['uiOnOrOff']]

    # lstPaths <- TestListPaths
    # if ( input[['uiPathSelect']] == 'Annual Process' ){
    #   lstPathsAnnual <- getListPathsFromAnnualResults()
    #   lstPaths <- lstPathsAnnual
    # }

    #lstCmp <- cmpRefPriceToDayAheadCongestion(lstPaths = lstPaths, periodName = periodName, onOrOff = onOrOff)
    lstCmp <- lstResults[[paste(periodName, onOrOff)]]
  })

   output$plotCompare <- renderPlot({
     lstData <- reactLstData()
     lstData[['gg']]
   })

   output$tblBrushed <- rhandsontable::renderRHandsontable({
     lstData <- reactLstData()
     rhandsontable::rhandsontable(brushedPoints(lstData[['cmp']], input$plotCompare_brush))

   })

   output$plotPercentage <- renderPlot({
     lstData <- reactLstData()
     lstCounts <- lstData[['lstCounts']]
     dfToPlot <- do.call(rbind, purrr::map(lstCounts, data.frame))
     dfToPlot[['label']] <- paste0(round(100*dfToPlot[['numPct']], digits = 1), "%")

     rngX <- max(c(dfToPlot[['x']], -dfToPlot[['x']]), na.rm = TRUE) * 1.5
     rngY <- max(c(dfToPlot[['y']], -dfToPlot[['y']]), na.rm = TRUE) * 1.5

     ggplot(dfToPlot) + geom_text(mapping = aes(x = x, y = y, label = label)) +
       theme_bw() +
       geom_point(mapping = aes(x, y), alpha = I(0.2)) +
       geom_vline(xintercept = 0, alpha = I(0.2)) + geom_hline(yintercept = 0, alpha = I(0.2)) +
       scale_x_continuous(name = "DaCongest less CurrentRefPrice in $/MWh", limits = c(-rngX, rngX))  +
       scale_y_continuous(name = "DaCongest less ProposedRefPrice in $/MWh", limits = c(-rngY, rngY)) +
       geom_abline(intercept = 0, slope = 1, alpha = I(0.2))
   })
}

# Run the application
shinyApp(ui = ui, server = server)


# assess the over and under estimation of the Reference Price and the Proposed Price
library(shiny)
library(dplyr)
library(ggplot2)

source("utils_compare_credit_calc.R")

py <- 2017

# override the LocalDriveRefPrice to PY2016
vecOnOrOffs <- c('OFF', 'ON')

# determine the paths

if ( py  == 2016 ) {

  vecPeriods <- c('Jun_16', 'Jul_16', 'Aug_16', 'Sep_16', 'Fall_16', 'Winter_16', 'Spring_17')
  lstPaths <- getListPathsFromAnnualResults2016()
  LocalDriveRefPrice <- "//fs2/world/SMD/Staff/SPP_Auction_Revenue_Rights/PY_2016_2017_ARR_Registration/TCR_REFERENCE_PRICES"

} else if ( py == 2017 ) {

  vecPeriods <- c('Jun_17', 'Jul_17', 'Aug_17'
                  #, 'Sep_17', 'Fall_17', 'Winter_17', 'Spring_18'
  )
  lstPaths <- getListPathsFromAnnualResults2017()
  LocalDriveRefPrice <- "//fs2/world/SMD/Staff/SPP_Auction_Revenue_Rights/PY_2017_2018_ARR_Registration/TCR_REFERENCE_PRICES"

} else {

  stop( "cannot understand the planning year requested or py is not initialized")
}





lstPeriods <- purrr::map(1:14, function(nth) {
  list(periodName = vecPeriods[1 + floor((nth - 1) / 2)],
       onOrOff = vecOnOrOffs[ 1 + ((nth -1) %% 2) ] )
})

vecPeriodNames <- purrr::map_chr(lstPeriods, function(lstPeriodDef)
  paste(lstPeriodDef[['periodName']], lstPeriodDef[['onOrOff']])
)

lstResult <- purrr::map(lstPeriods, function(lstPeriodDef) {

  periodName <- lstPeriodDef[['periodName']]
  onOrOff <- lstPeriodDef[['onOrOff']]

  lstCmp <- cmpRefPriceToDayAheadCongestion(lstPaths = lstPaths, periodName = periodName, onOrOff = onOrOff)
})
names(lstResult) <- vecPeriodNames

saveRDS(object = lstResult, file = paste0('lstCompareCreditCalc', py, '.RDS'))

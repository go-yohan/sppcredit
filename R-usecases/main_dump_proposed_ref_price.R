library(dplyr)
library(tidyr)

# lstPaths <- list(list(Source = 'WFEC_CAPROCK_SOLAR',  Sink = 'WFEC_WFEC'))
# periodName <- 'Aug_17'
# onOrOff <- 'OFF'

lstResult <- readRDS('lstCompareCreditCalc2016.RDS')
vecPeriods <- c('Jun_16', 'Jul_16', 'Aug_16', 'Sep_16', 'Fall_16', 'Winter_16', 'Spring_17')

lstResult <- readRDS('lstCompareCreditCalc2017.RDS')
vecPeriods <- c('Jun_17', 'Jul_17', 'Aug_17', 'Sep_17', 'Fall_17', 'Winter_17', 'Spring_18')

vecOnOrOff <- c('OFF', 'ON')

for ( periodName in vecPeriods ) {
  for ( onOrOff in vecOnOrOff ) {
    nm <- paste(periodName, onOrOff)
    dfData <- lstResult[[nm]][['cmp']]
    dfData <- dfData %>%tidyr::separate(col = Path, into = c("Source", "Sink"), sep = " -- ")
    message(nrow(dfData))
    fileName <- paste('ProposedRefPrice', periodName, onOrOff, sep = '_')
    fileName <- paste0(fileName, '.csv')
    readr::write_csv(x = dfData, path = fileName)
  }
}

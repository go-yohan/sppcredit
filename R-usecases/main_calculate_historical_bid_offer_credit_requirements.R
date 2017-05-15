library(dplyr)
library(ggplot2)

LocalDriveRefPrice <- "//fs2/world/SMD/Staff/SPP_Auction_Revenue_Rights/PY_2016_2017_ARR_Registration/TCR_REFERENCE_PRICES"
LocalDriveDaPrice <- "//fs2/world/Analytics/Apps/Qlikview/FTR/SPP_DATA/Markets/DA/LMP_By_SETTLEMENT_LOC"

# look at the annual bid curve and calculate the credit requirements

#example:
LocalRootDirHistoricalBid <- "//fs2/world/Analytics/Apps/Qlikview/FTR/SPP_TCR_Annual_Auctions"

vecPeriods <- c('Jun_16', 'Jul_16', 'Aug_16', 'Sep_16', 'Fall_16', 'Winter_16', 'Spring_17')
vecTous <- c("OFF", "ON")

# vecPeriods <- c('Jun_16')
# vecTous <- c('OFF')
lstByPeriod <- purrr::map(vecPeriods, function(periodName) {
  fileName <- getHistoricalBidFileName(periodName)
  if (is.null(fileName)) {
    stop("cannot understand the periodName: ", periodName)
  }
  fullFilePath <- file.path(LocalRootDirHistoricalBid, fileName)
  dfBid <- readSppHistoricalBidFile(fullFilePath)

  # get the unique list of paths
  dfPaths <- unique(dplyr::select(dfBid, Source, Sink))
  lstPaths <- purrr::transpose(dfPaths)


  # get the reference price for both ON and OFF
  lstOnOrOff <- purrr::map(vecTous, function(onOrOff) {
    print(onOrOff)

    # calculate the ideal worst case value
    dfStatsWorstCase <- calcIdealWorstCaseValueByPath(lstPaths = lstPaths, periodName = periodName, onOrOff = onOrOff, ftpRoot = LocalDriveDaPrice, numHours = NULL,
                                                      vecQuantiles = c(0.5, 0.05, 0.02))

    # get the reference price
    dfRefPrice <- getDfRefPriceByPeriod(periodName = periodName, onOrOff = onOrOff, ftpRoot = LocalDriveRefPrice, lstPaths = lstPaths)
    # nrow(dfRefPrice)
    # unique(dfRefPrice$Class)

    # join the reference price information
    dfBidSpecClass <- dfBid %>% filter(Class == onOrOff) %>%
      left_join(dfRefPrice) %>%
      left_join(dfStatsWorstCase)

    # calculate the credit requirements
    lstBidsSpecClass <- purrr::transpose(dfBidSpecClass)

    lstBidsSpecClass <- purrr::map(1:length(lstBidsSpecClass), function(bidEntryInd) {
      print(bidEntryInd)
      bidEntry <- lstBidsSpecClass[[bidEntryInd]]
      bidEntry[['CREDIT_REQUIRED']] <- calcBidCreditNeeded(bidEntry = bidEntry, bidOffset = bidEntry[['PRODUCT_REFERENCE_PRICE']], netting = TRUE)

      # calculate the credit requirement for a percentile
      bidEntry[['CREDIT_REQUIRED_STATS0.02']] <- calcBidCreditNeeded(bidEntry = bidEntry, bidOffset = bidEntry[['PeriodQ0.02']], netting = TRUE)
      bidEntry[['CREDIT_REQUIRED_STATS0.05']] <- calcBidCreditNeeded(bidEntry = bidEntry, bidOffset = bidEntry[['PeriodQ0.05']], netting = TRUE)

      bidEntry
    })

    list(Class = onOrOff,
         Bids = lstBidsSpecClass,
         Proxy1 = sum(dfBidSpecClass[['YEAR_1_PROXY_PRICE_IND']] == 'Y', na.rm = TRUE),
         Proxy2 = sum(dfBidSpecClass[['YEAR_2_PROXY_PRICE_IND']] == 'Y', na.rm = TRUE),
         CreditRequired = purrr::map_dbl(lstBidsSpecClass, function(bidEntry) bidEntry[['CREDIT_REQUIRED']]),
         CreditRequiredStats02 = purrr::map_dbl(lstBidsSpecClass, function(bidEntry) bidEntry[['CREDIT_REQUIRED_STATS0.02']]),
         CreditRequiredStats05 = purrr::map_dbl(lstBidsSpecClass, function(bidEntry) bidEntry[['CREDIT_REQUIRED_STATS0.05']])
    )
  })

  names(lstOnOrOff) <- vecTous
  lstOnOrOff
})
names(lstByPeriod) <- vecPeriods

lstCreditRequiredByPeriods = list()
vecLabels <- c('CreditRequired', 'CreditRequiredStats02', 'CreditRequiredStats05')
vecRefPriceMethods <- c('Current', 'Stats 2%-tile', 'Stats 5%-tile')
for (labelInd in 1:length(vecLabels)) {
  refPriceMethod <- vecRefPriceMethods[labelInd]
  label <- vecLabels[labelInd]

  for ( periodName in vecPeriods ) {
    for (classDef in vecTous) {
      vecCredit <- lstByPeriod[[periodName]][[classDef]][[label]]
      lstCreditRequiredByPeriods[[1+length(lstCreditRequiredByPeriods)]] <-
        list(
          RefPriceMethod = refPriceMethod,
          Period = periodName,
          Class = classDef,
          CreditRequirements = sum(vecCredit[vecCredit > 0], na.rm = TRUE)
      )
    }
  }
}

dfCreditRequiredByPeriods <- tibble::tibble(purrr::transpose(lstCreditRequiredByPeriods))






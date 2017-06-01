library(dplyr)
library(ggplot2)

LocalDriveRefPrice <- "//fs2/world/SMD/Staff/SPP_Auction_Revenue_Rights/PY_2016_2017_ARR_Registration/TCR_REFERENCE_PRICES"
LocalDriveDaPrice <- "//fs2/world/Analytics/Apps/Qlikview/FTR/SPP_DATA/Markets/DA/LMP_By_SETTLEMENT_LOC"

# look at the annual bid curve and calculate the credit requirements

levelPeriodClass <- function(dfData, vecPeriods, vecTous = c("OFF", "ON"), colPeriodClassName = 'PeriodClass') {
  dfData[[colPeriodClassName]] <- factor(dfData[[colPeriodClassName]],
                                    levels = paste(rep(vecPeriods, each = 2), rep(vecTous, times = 7), sep = "_"))
  dfData

}

#example:
LocalRootDirHistoricalBid <- "//fs2/world/Analytics/Apps/Qlikview/FTR/SPP_TCR_Annual_Auctions"

vecPeriods <- c('Jun_16', 'Jul_16', 'Aug_16', 'Sep_16', 'Fall_16', 'Winter_16', 'Spring_17')
vecTous <- c("OFF", "ON")
vecMethodTypes <- c('CreditRequired', 'CreditRequiredStats05', 'CreditRequiredStats02')

# vecPeriods <- c('Jun_16')
# vecTous <- c('OFF')
calcCreditRequirementsAllPeriods <- function() {
  lstByPeriod <- purrr::map(vecPeriods, function(periodName) {
    message("Evaluating Period: ", periodName)
    fileName <- getHistoricalBidFileName(periodName)
    if (is.null(fileName)) {
      stop("cannot understand the periodName: ", periodName)
    }
    fullFilePath <- file.path(LocalRootDirHistoricalBid, fileName)
    dfBid <- readSppHistoricalBidFile(fullFilePath)

    # get the unique list of paths
    dfPaths <- unique(dplyr::select(dfBid, Source, Sink))
    lstPaths <- purrr::transpose(dfPaths)

    # calculate actual value
    periodInfo <- getPeriodInfo(periodName)
    dateRange <- c(periodInfo[['FromDate']], periodInfo[['ToDate']])
    if ( periodInfo[['ToDate']] > lubridate::now()) {
      dateRange[2] <- lubridate::as_date(lubridate::now())
    }
    dfActualValue <- getDfSppDaCongestOnPaths(lstPaths = lstPaths, dateRange = dateRange, ftpRoot = LocalDriveDaPrice)
    cal <- getRtoCalendar("SPP", fromDate = as.character(dateRange[1]), toDate = as.character(dateRange[2]), props = c("GMTIntervalEnd", "TIMEOFUSE"))
    dfActualValue <- dfActualValue %>% left_join(cal %>% select(GMTIntervalEnd, Class = TIMEOFUSE))


    # get the reference price for both ON and OFF
    lstOnOrOff <- purrr::map(vecTous, function(onOrOff) {
      print(onOrOff)
      numHours <- sum(cal[['TIMEOFUSE']] == onOrOff)

      # calculate the ideal worst case value
      dfStatsWorstCase <- calcIdealWorstCaseValueByPath(lstPaths = lstPaths, periodName = periodName, onOrOff = onOrOff, ftpRoot = LocalDriveDaPrice, numHours = numHours,
                                                        vecQuantiles = c(0.5, 0.05, 0.02))

      # get the reference price
      dfRefPrice <- getDfRefPriceByPeriod(periodName = periodName, onOrOff = onOrOff, ftpRoot = LocalDriveRefPrice, lstPaths = lstPaths)
      # nrow(dfRefPrice)
      # unique(dfRefPrice$Class)


      # join the reference price information
      dfBidSpecClass <- dfBid %>% filter(Class == onOrOff) %>%
        left_join(dfRefPrice) %>%
        left_join(dfStatsWorstCase) %>%
        left_join(dfActualValue %>% filter(Class == onOrOff) %>% select(Source, Sink, CONGEST) %>% group_by(Source, Sink) %>% summarise(CONGEST = sum(CONGEST)))

      # calculate the credit requirements
      lstBidsSpecClass <- purrr::transpose(dfBidSpecClass)

      lstBidsSpecClass <- purrr::map(1:length(lstBidsSpecClass), function(bidEntryInd) {
        print(bidEntryInd)
        bidEntry <- lstBidsSpecClass[[bidEntryInd]]

        lstPq <- getBidPriceQuantityPair(bidEntry)
        vecMw <- lstPq[['MW']]

        bidEntry[['CREDIT_REQUIRED']] <- calcBidCreditNeeded(bidEntry = bidEntry, bidOffset = bidEntry[['PRODUCT_REFERENCE_PRICE']], netting = TRUE)

        # calculate the credit requirement for a percentile
        bidEntry[['CREDIT_REQUIRED_STATS0.02']] <- calcBidCreditNeeded(bidEntry = bidEntry, bidOffset = bidEntry[['PeriodQ0.02']], netting = TRUE)
        bidEntry[['CREDIT_REQUIRED_STATS0.05']] <- calcBidCreditNeeded(bidEntry = bidEntry, bidOffset = bidEntry[['PeriodQ0.05']], netting = TRUE)

        bidEntry[['CREDIT_REQUIRED_ACTUAL']] <- calcBidCreditNeeded(bidEntry = bidEntry, bidOffset = bidEntry[['CONGEST']], netting = TRUE)

        bidEntry
      })

      list(Class = onOrOff,
           Bids = lstBidsSpecClass,
           Proxy1 = sum(dfBidSpecClass[['YEAR_1_PROXY_PRICE_IND']] == 'Y', na.rm = TRUE),
           Proxy2 = sum(dfBidSpecClass[['YEAR_2_PROXY_PRICE_IND']] == 'Y', na.rm = TRUE),
           CreditRequired = purrr::map_dbl(lstBidsSpecClass, function(bidEntry) bidEntry[['CREDIT_REQUIRED']]),
           CreditRequiredStats02 = purrr::map_dbl(lstBidsSpecClass, function(bidEntry) bidEntry[['CREDIT_REQUIRED_STATS0.02']]),
           CreditRequiredStats05 = purrr::map_dbl(lstBidsSpecClass, function(bidEntry) bidEntry[['CREDIT_REQUIRED_STATS0.05']]),
           CreditRequiredActual = purrr::map_dbl(lstBidsSpecClass, function(bidEntry) bidEntry[['CREDIT_REQUIRED_ACTUAL']])
      )
    })

    names(lstOnOrOff) <- vecTous
    lstOnOrOff
  })
  names(lstByPeriod) <- vecPeriods

  save(lstByPeriod, file = 'lstByPeriod.RData')
}

#-- uncomment if need to rerun the calculation, otherwise use the cached data.
#-- calcCreditRequirementsAllPeriods()
load('lstByPeriod.RData')


# gather the credit requirements for each period
lstCreditRequiredByPeriods = list()
vecLabels <- c('CreditRequired', 'CreditRequiredStats02', 'CreditRequiredStats05', 'CreditRequiredActual')
vecRefPriceMethods <- c('Current', 'Stats 2%-tile', 'Stats 5%-tile', 'Actual')
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

lstT <- purrr::transpose(lstCreditRequiredByPeriods)
lstT <- purrr::map(lstT, function(theCol) unlist(theCol))
names(lstT) <- c('RefPriceMethod', 'Period', 'Class', 'CreditRequirements')
dfCreditRequiredByPeriods <- as.data.frame(lstT) %>% tidyr::unite(PeriodClass, Period, Class)
dfCreditRequiredByPeriods <- levelPeriodClass(dfData = dfCreditRequiredByPeriods, vecPeriods = vecPeriods, vecTous = vecTous, colPeriodClassName = 'PeriodClass')
dfCreditRequiredByPeriods[['CreditRequirementsInMillion']] <- dfCreditRequiredByPeriods[['CreditRequirements']] /1e6

gg <- ggplot(dfCreditRequiredByPeriods) +
  geom_col(aes(x = PeriodClass, y = CreditRequirementsInMillion, fill = RefPriceMethod), position = "dodge") +
  theme_bw() +
  ylab("Credit Requirements in M$" ) +
  xlab("Period Class") +
  ggplot2::scale_fill_manual(values = TEAColors) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(filename = 'Compare_Credit_Requirements.png', plot = gg, width = 6, height= 6, units = "in")


# While this is a good reference, it is not a fair comparison. I found that the Actual is higher than Credit Required
# not on the paths that are required to post credit by the original Reference Price calculation
# rather it is due to a path that was not required to post credit (because of high positive congestion in the past)
# but then resulted in high negative congestion. e.g. SPPSOUTH_HUB to OKGETALOGAWIND Winter_16 OFF
tst <- lstByPeriod[['Winter_16']] [['OFF']]
vecActual <- tst[['CreditRequiredActual']]
vecCredit <- tst[['CreditRequired']]
max(vecActual - vecCredit)
vecDiff <- vecActual - vecCredit
ind <- which(vecDiff == max(vecDiff))
bidEntry <- tst[['Bids']][[ind]]

# as such, to be a fair comparison to the original reference price calculation, we need to just look at the paths
# that are required to post credit by the current methodology


lstCreditRequiredByPeriods = list()
indToEval <- list()
vecLabels <- c('CreditRequired', 'CreditRequiredStats02', 'CreditRequiredStats05', 'CreditRequiredActual')
vecRefPriceMethods <- c('Current', 'Stats 2%-tile', 'Stats 5%-tile', 'Actual')
for (labelInd in 1:length(vecLabels)) {
  refPriceMethod <- vecRefPriceMethods[labelInd]
  label <- vecLabels[labelInd]

  for ( periodName in vecPeriods ) {

    if (is.null(indToEval[[periodName]])) {
      indToEval[[periodName]] <- list()
    }

    for (classDef in vecTous) {
      ind <- indToEval[[periodName]][[classDef]]
      vecCredit <- lstByPeriod[[periodName]][[classDef]][[label]]

      # save the indeces where the vecCredit is positive to be used for later
      if (is.null(ind)) {
        ind <- vecCredit > 0
        indToEval[[periodName]][[classDef]] <- ind
      }

      lstCreditRequiredByPeriods[[1+length(lstCreditRequiredByPeriods)]] <-
        list(
          RefPriceMethod = refPriceMethod,
          Period = periodName,
          Class = classDef,
          CreditRequirements = sum(vecCredit[ind], na.rm = TRUE)
        )
    }
  }
}

lstT <- purrr::transpose(lstCreditRequiredByPeriods)
lstT <- purrr::map(lstT, function(theCol) unlist(theCol))
names(lstT) <- c('RefPriceMethod', 'Period', 'Class', 'CreditRequirements')
dfCreditRequiredByPeriods <- as.data.frame(lstT) %>% tidyr::unite(PeriodClass, Period, Class)
dfCreditRequiredByPeriods <- levelPeriodClass(dfData = dfCreditRequiredByPeriods, vecPeriods = vecPeriods, vecTous = vecTous, colPeriodClassName = 'PeriodClass')
dfCreditRequiredByPeriods[['CreditRequirementsInMillion']] <- dfCreditRequiredByPeriods[['CreditRequirements']] /1e6

gg <- ggplot(dfCreditRequiredByPeriods) +
  geom_col(aes(x = PeriodClass, y = CreditRequirementsInMillion, fill = RefPriceMethod), position = "dodge") +
  theme_bw() +
  ylab("Credit Requirements in M$" ) +
  xlab("Period Class") +
  ggplot2::scale_fill_manual(values = TEAColors) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(filename = 'Compare_Credit_Requirements_FAIR.png', plot = gg, width = 6, height= 6, units = "in")

# here it shows that there are many periods where Actual is actually higher than the proposed 2%-tile and 5%-tile reference price calculation.
# I think an even more fair comparison is to look at the distribution on the difference across each bid.

periodName <- 'Sep_16'
classDef <- 'ON'

#  need to calculate how much I am over-collateralizing by # bids (and percentage) and by $ amount (and percentage)
getRelevantBids <- function(periodName, classDef) {
  tst <- lstByPeriod[[periodName]] [[classDef]]
  # look only at the bids / offers that require credits using the current reference price methodology
  ind <- tst[['CreditRequired']] > 0

  tst[['Bids']][ind]
}

calcOverUnder <- function(periodName, classDef, as_data_frame = TRUE, excludeNewPaths = FALSE) {
  tst <- lstByPeriod[[periodName]] [[classDef]]
  # look only at the bids / offers that require credits using the current reference price methodology
  ind <- tst[['CreditRequired']] > 0

  vecActual <- tst[['CreditRequiredActual']][ind]
  lstOverUnder <- purrr::map(vecMethodTypes, function(methodType) {
    vecCredit <- tst[[methodType]][ind]
    vecDiffCredit <- vecActual - vecCredit

    indUnderCollat <- vecDiffCredit > 0
    indOverCollat <- vecDiffCredit < 0
    indEqualCollat <- vecDiffCredit == 0

    if ( excludeNewPaths == TRUE ) {
      vecYear1Proxy <- purrr::map_chr(tst[['Bids']][ind], function(bidEntry) {bidEntry[['YEAR_1_PROXY_PRICE_IND']]})
      vecYear2Proxy <- purrr::map_chr(tst[['Bids']][ind], function(bidEntry) {bidEntry[['YEAR_2_PROXY_PRICE_IND']]})
      indNewPaths <- is.na(vecYear1Proxy) | vecYear1Proxy == 'Y'

      indUnderCollat <- indUnderCollat & !indNewPaths
      indOverCollat <- indOverCollat & !indNewPaths
    }

    list(
      MethodType = methodType,

      OverCollatDollar = sum(vecDiffCredit [indOverCollat]),
      OverCollatBids = sum(indOverCollat),
      OverCollatBidsPct = sum(indOverCollat) / length(indOverCollat),
      OverCollatVector = vecDiffCredit [indOverCollat],

      UnderCollatDollar = sum(vecDiffCredit [indUnderCollat]),
      UnderCollatBids = sum(indUnderCollat),
      UnderCollatBidsPct = sum(indUnderCollat) / length(indUnderCollat),
      UnderCollatVector = vecDiffCredit[indUnderCollat],

      EqualCollatBids = sum(indEqualCollat),
      EqualCollatBidsPct = sum(indEqualCollat) / length(indEqualCollat),

      # vector of Over and Under
      OverUnderVec = vecDiffCredit
    )
    })
  names(lstOverUnder) <- vecMethodTypes

  # return the list if not requesting a data frame.
  if ( !as_data_frame ) {
    return (lstOverUnder)
  }

  # only gather the data that is scalar, so we can transform to a data frame
  lstOverUnderToDataFrame <- purrr::map(lstOverUnder, function(lst) {
    lst[c('MethodType', 'OverCollatDollar', 'OverCollatBids', 'OverCollatBidsPct',
          'UnderCollatDollar', 'UnderCollatBids', 'UnderCollatBidsPct',
          'EqualCollatBids', 'EqualCollatBidsPct')]
  })

  dfData <- do.call(rbind, purrr::map(lstOverUnderToDataFrame, as.data.frame))
  rownames(dfData) <- NULL
  dfData
}



dfOverUnder <- data.frame()
for (periodName in vecPeriods) {
  for (classDef in vecTous) {
    dfOverUnder <- rbind(dfOverUnder,
                         data.frame(Period = periodName, Class = classDef, calcOverUnder(periodName, classDef)))
  }
}
readr::write_csv(x = dfOverUnder, path = 'df_over_under.csv')

# can we come up with a metric that compares how much over-collateralization is saved for every under-collateralization exposure
dfOverUnderSumm <- dfOverUnder %>% group_by(MethodType) %>%
  summarise(
    OverCollatDollar = sum(OverCollatDollar),
    UnderCollatDollar = sum(UnderCollatDollar),
    OverCollatBids = sum(OverCollatBids),
    UnderCollatBids = sum(UnderCollatBids)
  )
readr::write_csv(x = dfOverUnderSumm, path = 'df_over_under_summary.csv')

dfOverUnder <- dfOverUnder %>% tidyr::unite(PeriodClass, Period, Class)
dfOverUnder <- levelPeriodClass(dfData = dfOverUnder, vecPeriods = vecPeriods, vecTous = vecTous, colPeriodClassName = 'PeriodClass')


# graph the dfOverUnder
gg <- ggplot(dfOverUnder) +
  geom_col(aes(x = PeriodClass, y = UnderCollatDollar, fill = MethodType), position = "dodge") +
  geom_col(aes(x = PeriodClass, y = OverCollatDollar, fill = MethodType), position = "dodge") +
  theme_bw() +
  ylab("Over or Under in $" ) +
  xlab("Period Class") +
  ggplot2::scale_fill_manual(values = TEAColors) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "top") +
  geom_abline(slope = 0, intercept = 0)
gg
ggsave(filename = "Compare_over_under_collateralization.png", width = 10, height = 6, units = "in")

gg <- ggplot(dfOverUnder) +
  geom_col(aes(x = PeriodClass, y = OverCollatBidsPct * 100, fill = MethodType), position = "dodge") +
  theme_bw() +
  ylab("Bids/Offers that are Over-Collateralized in Pct" ) +
  xlab("Period Class") +
  ggplot2::scale_fill_manual(values = TEAColors) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "top") +
  geom_abline(slope = 0, intercept = 0) +
  scale_y_continuous(limits = c(0, 100))
gg
ggsave(filename = "Compare_over_collateralization_in_PctBids.png", width = 10, height = 6, units = "in")

gg <- ggplot(dfOverUnder) +
  geom_col(aes(x = PeriodClass, y = UnderCollatBidsPct * 100, fill = MethodType), position = "dodge") +
  theme_bw() +
  ylab("Bids/Offers that are Under-Collateralized in Pct" ) +
  xlab("Period Class") +
  ggplot2::scale_fill_manual(values = TEAColors) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "top") +
  geom_abline(slope = 0, intercept = 0) +
  scale_y_continuous(limits = c(0, 100))
gg
ggsave(filename = "Compare_under_collateralization_in_PctBids.png", width = 10, height = 6, units = "in")

gg <- ggplot(dfOverUnder) +
  geom_col(aes(x = PeriodClass, y = EqualCollatBidsPct * 100, fill = MethodType), position = "dodge") +
  theme_bw() +
  ylab("Bids/Offers that are Under-Collateralized in Pct" ) +
  xlab("Period Class") +
  ggplot2::scale_fill_manual(values = TEAColors) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "top") +
  geom_abline(slope = 0, intercept = 0) +
  scale_y_continuous(limits = c(0, 100))
gg
ggsave(filename = "Compare_equal_collateralization_in_PctBids.png", width = 10, height = 6, units = "in")

# take a look at the details of the under and over collateralization by bid
lstOverUnderByPeriod <- purrr::map(vecPeriods, function(periodName) {
  lstOverUnderByClassDef <- purrr::map(vecTous, function(classDef) {
    lst <- calcOverUnder(periodName, classDef, as_data_frame = FALSE)

    # construct the data frame that shows the distribution of over and under collateralization
    dfToPlotUnderOver <- NULL
    for (methodType in vecMethodTypes) {
      dfToPlotUnderOver[[methodType]] <- lst[[methodType]] [['OverUnderVec']]
    }

    dfToPlotUnderOver <- as.data.frame(dfToPlotUnderOver)
    xylim <- c(-4e5, 4e5)
    gg <- ggplot(dfToPlotUnderOver, aes(x = CreditRequiredStats05, y = CreditRequired)) + geom_point(alpha = I(0.3)) +
      geom_abline(slope = 1, intercept = 0, color = I("red"), alpha = I(0.4)) +
      theme_bw() +
      ylab("CreditRequired: Over or Under in $" ) +
      xlab("CreditRequiredStats05: Over or Under in $" ) +
      ggplot2::scale_fill_manual(values = TEAColors) +
      geom_abline(slope = 0, intercept = 0, alpha = I(0.4)) +
      geom_vline(xintercept = 0, alpha + I(0.4)) +
      labs(title = paste("Over-Under Collateralization by Bid:", periodName, classDef)) +
      coord_fixed(ratio = 1, xlim = xylim, ylim = xylim)

    filename <- paste0(paste("Compare_over_under_collateralization_by_bid", periodName, classDef, sep = "_"), ".PNG")
    ggsave(filename = filename, plot = gg, width = 10, height = 6, units = "in")

    # find the extreme
    vecDiff <- dfToPlotUnderOver[['CreditRequiredStats05']] - dfToPlotUnderOver[['CreditRequired']]

    # case 1. highest under-collateralization in CreditRequiredStats05 compared to CreditRequired
    # i.e. max(CreditRequiredStats05 - CreditRequired)
    indUnderUnder <- dfToPlotUnderOver[['CreditRequiredStats05']] > 0  &  dfToPlotUnderOver[['CreditRequired']] > 0
    amtUnderCollatGreatestDev <- max(vecDiff[indUnderUnder])
    indUnderCollatGreatestDev <- which(vecDiff == amtUnderCollatGreatestDev)
    bidEntryUnderUnderGreatestDev <- getRelevantBids(periodName, classDef)[[indUnderCollatGreatestDev]]

    # case 2. highest over-collateralization in Credit Required compared to CreditRequiredStats05
    indOverOver <- dfToPlotUnderOver[['CreditRequiredStats05']] < 0  &  dfToPlotUnderOver[['CreditRequired']] < 0
    amtOverCollatGreatestDev <- max(vecDiff[indOverOver])
    indOverCollatGreatestDev <- which(vecDiff == amtOverCollatGreatestDev)
    bidEntryOverOverGreatestDev <- getRelevantBids(periodName, classDef)[[indOverCollatGreatestDev]]

    # case 2b. highest over-collateralization in CreditRequiredStats05 compared to CreditRequired
    amtOverCollatSmallestDev <- min(vecDiff[indOverOver])
    indOverCollatSmallestDev <- which(vecDiff == amtOverCollatSmallestDev)
    bidEntryOverOverSmallestDev <- getRelevantBids(periodName, classDef)[[indOverCollatSmallestDev]]

    # case 3. under-collateralization under CreditRequiredStats05, but over-collateralized under CreditRequired
    indUnderOver <- dfToPlotUnderOver[['CreditRequiredStats05']] > 0  &  dfToPlotUnderOver[['CreditRequired']] < 0
    amtUnderOverCollatGreatestDev <- max(vecDiff[indUnderOver])
    indUnderOverCollatGreatestDev <- which(vecDiff == amtUnderOverCollatGreatestDev)
    bidEntryUnderOverGreatestDev <- getRelevantBids(periodName, classDef)[[indUnderOverCollatGreatestDev]]

    # case 4. over-collateralization under CreditRequiredStats05, but under-collateralized under CreditRequired
    indOverUnder <- dfToPlotUnderOver[['CreditRequiredStats05']] < 0  &  dfToPlotUnderOver[['CreditRequired']] > 0
    amtOverUnderCollatGreatestDev <- max(vecDiff[indOverUnder])
    indOverUnderCollatGreatestDev <- which(vecDiff == amtOverUnderCollatGreatestDev)
    bidEntryOverUnderGreatestDev <- getRelevantBids(periodName, classDef)[[indOverUnderCollatGreatestDev]]

    indEqualOver <- dfToPlotUnderOver[['CreditRequiredStats05']] == 0  &  dfToPlotUnderOver[['CreditRequired']] < 0
    amtEqualOverCollatGreatestDev <- max(vecDiff[indEqualOver])
    indEqualOverCollatGreatestDev <- which(vecDiff == amtEqualOverCollatGreatestDev)
    bidEntryEqualOverGreatestDev <- getRelevantBids(periodName, classDef)[[indEqualOverCollatGreatestDev]]


    list(
      gg = gg,
      UnderUnder = list(AmountDev = amtUnderCollatGreatestDev, Bid = bidEntryUnderUnderGreatestDev),
      OverOver = list(AmountDev = amtOverCollatGreatestDev, Bid = bidEntryOverOverGreatestDev),
      OverOverSmallest = list(AmountDev = amtOverCollatSmallestDev, Bid = bidEntryOverOverSmallestDev),
      UnderOver = list(AmountDev = amtUnderOverCollatGreatestDev, Bid = bidEntryUnderOverGreatestDev),
      OverUnder = list(AmountDev = amtOverUnderCollatGreatestDev, Bid = bidEntryOverUnderGreatestDev),
      EqualOver = list(AmountDev = amtEqualOverCollatGreatestDev, Bid = bidEntryEqualOverGreatestDev)
    )
  })
  names(lstOverUnderByClassDef) <- vecTous
  lstOverUnderByClassDef
})
names(lstOverUnderByPeriod) <- vecPeriods


# drilling down a particular period
tst <- lstOverUnderByPeriod[['Winter_16']][['OFF']]
tst[['UnderUnder']][['AmountDev']]
tst[['UnderUnder']][['Bid']]
tst[['OverOver']][['AmountDev']]
tst[['OverOver']][['Bid']]
tst[['EqualOver']][['Bid']]

###-----------------------------------------------

# is over under the same if we exclude the paths that do not have enough history, i.e. both year1 and year2 data do not exist
dfOverUnder <- data.frame()
for (periodName in vecPeriods) {
  for (classDef in vecTous) {
    dfOverUnder <- rbind(dfOverUnder,
                         data.frame(Period = periodName, Class = classDef, calcOverUnder(periodName, classDef, excludeNewPaths = TRUE)))
  }
}
readr::write_csv(x = dfOverUnder, path = 'df_over_under_nonewpaths.csv')

# can we come up with a metric that compares how much over-collateralization is saved for every under-collateralization exposure
dfOverUnderSumm <- dfOverUnder %>% group_by(MethodType) %>%
  summarise(
    OverCollatDollar = sum(OverCollatDollar),
    UnderCollatDollar = sum(UnderCollatDollar),
    OverCollatBids = sum(OverCollatBids),
    UnderCollatBids = sum(UnderCollatBids)
  )
readr::write_csv(x = dfOverUnderSumm, path = 'df_over_under_summary_nonewpaths.csv')

dfOverUnder <- dfOverUnder %>% tidyr::unite(PeriodClass, Period, Class)
dfOverUnder <- levelPeriodClass(dfData = dfOverUnder, vecPeriods = vecPeriods, vecTous = vecTous, colPeriodClassName = 'PeriodClass')

# graph the dfOverUnder
gg <- ggplot(dfOverUnder) +
  geom_col(aes(x = PeriodClass, y = UnderCollatDollar, fill = MethodType), position = "dodge") +
  geom_col(aes(x = PeriodClass, y = OverCollatDollar, fill = MethodType), position = "dodge") +
  theme_bw() +
  ylab("Over or Under in $" ) +
  xlab("Period Class") +
  ggplot2::scale_fill_manual(values = TEAColors) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "top") +
  geom_abline(slope = 0, intercept = 0)
gg
ggsave(filename = "Compare_over_under_collateralization_nonwqpaths.png", width = 10, height = 6, units = "in")

gg <- ggplot(dfOverUnder) +
  geom_col(aes(x = PeriodClass, y = OverCollatBidsPct * 100, fill = MethodType), position = "dodge") +
  theme_bw() +
  ylab("Bids/Offers that are Over-Collateralized in Pct" ) +
  xlab("Period Class") +
  ggplot2::scale_fill_manual(values = TEAColors) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "top") +
  geom_abline(slope = 0, intercept = 0) +
  scale_y_continuous(limits = c(0, 100))
gg
ggsave(filename = "Compare_over_under_collateralization_in_PctBids_nonewpaths.png", width = 10, height = 6, units = "in")


library(dplyr)
library(ggplot2)

LocalDriveDaPrice <- "//fs2/world/Analytics/Apps/Qlikview/FTR/SPP_DATA/Markets/DA/LMP_By_SETTLEMENT_LOC"
LocalDriveRefPrice <- "//fs2/world/SMD/Staff/SPP_Auction_Revenue_Rights/PY_2017_2018_ARR_Registration/TCR_REFERENCE_PRICES"
CachedData <- new.env()

TestListPaths <- list(list(Source = 'NPPD_NPPD', Sink = 'SPRM_SPRM'),
                      list(Source = 'SPRM_SPRM', Sink = 'NPPD_NPPD'),
                      list(Source = 'AECI', Sink = 'SPA'),
                      list(Source = 'SPA', Sink = 'AECI'),
                      list(Source = 'GRDA.GREC3', Sink = 'WR.NSW'),
                      list(Source = 'WR.NSW', Sink = 'GRDA.GREC3'),
                      list(Source = 'GRDA.GREC3', Sink = 'NPPD_NPPD'),
                      list(Source = 'NPPD_NPPD', Sink = 'GRDA.GREC3')
)


getListPathsFromAnnualResults <- function() {
  if ( is.null(CachedData[["ResultLtcr"]]) ) {
    ftpPath <- "ftp://pubftp.spp.org/TCR/HistoricalAllocationResults/2017_Annual_LTCR_Allocation_99_PROD/2017_Annual_LTCR_Allocation_99_PROD_TCR-FROM-LTCR_RESULT.csv"
    CachedData[['ResultLtcr']] <- readr::read_csv(ftpPath)
  }

  if ( is.null(CachedData[["ResultAuction"]])) {
    ftpPath <- "ftp://pubftp.spp.org/TCR/HistoricalAuctionResults/2016_Annual_TCR_Auction_99_PROD/2016_Annual_TCR_Auction_99_PROD_AUCTION_RESULT.csv"
    CachedData[["ResultAuction"]] <- readr::read_csv(ftpPath)
  }

  dfPaths <- rbind(unique(CachedData[['ResultLtcr']]) %>% select(Source, Sink),
                   unique(CachedData[['ResultAuction']]) %>% select(Source, Sink))
  dfPaths <- unique(dfPaths)

  lstPaths <- purrr::transpose(dfPaths)
}

showRefPriceCalcMatchesRefPrice <- function(lstPaths = TestListPaths, periodName = 'Jun_17', onOrOff = 'OFF') {
  dfCalc <- calcRefPriceSpp(lstPaths = lstPaths, periodName = periodName, onOrOff = onOrOff, ftpRoot = LocalDriveDaPrice)
  dfRefPrice <- getDfRefPriceByPeriod(periodName = periodName, onOrOff = onOrOff, ftpRoot = LocalDriveRefPrice, lstPaths = lstPaths)

  # compare the calculated Ref Price dfCalc$RefPrice  to  the downloaded reference price dfRefPrice$HOURLY_REFERENCE_PRICE
  dfCmp <- dfCalc  %>% select(Source, Sink, RefPrice) %>%
    left_join(dfRefPrice %>% select(Source, Sink, HOURLY_REFERENCE_PRICE)) %>%
    tidyr::unite(Path, Source, Sink, sep = ' -- ')

  gg <- ggplot2::ggplot(dfCmp)  + geom_point(aes(x = RefPrice, y = HOURLY_REFERENCE_PRICE), alpha = (0.5)) +
    geom_abline(intercept = 0, slope = 1, alpha = I(0.5), color = I("red"))

  list(cmp = dfCmp, gg = gg)
}


cmpRefPriceToIdealCalculationOnWorstValue <- function(lstPaths = TestListPaths, periodName = 'Jun_17', onOrOff = 'OFF') {
  dfRefPrice <- getDfRefPriceByPeriod(periodName = periodName, onOrOff = onOrOff, ftpRoot = LocalDriveRefPrice, lstPaths = lstPaths)

  # calculate the number of hours once
  numHours <- getNumHours(periodName = periodName, onOrOff = onOrOff)

  # set the dateRange to historical data one year ago
  dfWorst <- calcIdealWorstCaseValueByPath(lstPaths, periodName, onOrOff, ftpRoot = ftpRoot, numHours = numHours)

  dfCmp <- dfRefPrice  %>% select(Source, Sink, HOURLY_REFERENCE_PRICE) %>%
    left_join(dfWorst %>% select(Source, Sink, Period5PctPerMwh)) %>%
    tidyr::unite(Path, Source, Sink, sep = ' -- ')

  gg <- ggplot2::ggplot(dfCmp)  + geom_point(aes(x = Period5PctPerMwh, y = HOURLY_REFERENCE_PRICE), alpha = (0.5)) +
    geom_abline(intercept = 0, slope = 1, alpha = I(0.5), color = I("red"))

  list(cmp = dfCmp, gg = gg)
}

# COMPARING published reference price to the calculated reference price
lstPathsAnnual <- getListPathsFromAnnualResults()
lstCmp <- showRefPriceCalcMatchesRefPrice(lstPaths = lstPathsAnnual)
lstCmp[['gg']]
dfCmp <- lstCmp[['cmp']]
dfCmp[order(dfCmp[['HOURLY_REFERENCE_PRICE']]), ] %>% head()
dfCmp[order(-dfCmp[['HOURLY_REFERENCE_PRICE']]), ] %>% head()
ggsave(filename = 'Compare_Calc_Vs_RefPrice.png', plot = lstCmp[['gg']], width = 6, height= 6, units = "in")


# COMPARING to an ideal worst-case value
lstCmpToWorst <- cmpRefPriceToIdealCalculationOnWorstValue(lstPaths = lstPathsAnnual)
lstCmpToWorst[['gg']]
dfCmpToWorst <- lstCmpToWorst[['cmp']]

dfCmpToWorst[order(dfCmpToWorst[['HOURLY_REFERENCE_PRICE']]),]
ggsave(filename = 'Compare_Statistical_Worstcase_Vs_RefPrice.png', plot = lstCmpToWorst[['gg']], width = 6, height= 6, units = "in")

# # 4 paths that are almost exactly 0
# dfRefPriceAll <- getDfRefPriceByPeriod(periodName = periodName, onOrOff = onOrOff, ftpRoot = LocalDriveRefPrice, lstPaths = NULL)
#
# # Look at the reference price on all paths and see
# indAroundZeroNewPaths <- (!is.na(dfRefPriceAll[['HOURLY_REFERENCE_PRICE']]) &
#                             dfRefPriceAll[['YEAR_1_PROXY_PRICE_IND']] == 'Y' &
#                             dfRefPriceAll[['YEAR_2_PROXY_PRICE_IND']] == 'Y' &
#                             dfRefPriceAll[['HOURLY_REFERENCE_PRICE']] <= 1e-4 &
#                             dfRefPriceAll[['HOURLY_REFERENCE_PRICE']] >= -1e-4)
# print.data.frame(unique(dfRefPriceAll[indAroundZeroNewPaths, c('Source')]))
#
# ind <- indAroundZeroNewPaths & dfRefPriceAll[['Source']] == "SECI.FPLP.GRAYWIND"
# print.data.frame(dfRefPriceAll[ind, ])
#
# pick the what seems to be a long path
lstPathsInterest <- list(
  list(Source = "OKGECENTWIND", Sink = "OKGESNRWIND"),
  list(Source = "OKGEKEENANWIND", Sink = "CSWSLEEPINGBEAR"),
  list(Source = "SECI.FPLP.GRAYWIND", Sink = "WAUE.BEPM.BRADY1"),
  list(Source = "OKGESNRWIND", Sink = "WR.MW.GMEC.MW"),
  list(Source = "INDN_MWE_SMKY2", Sink = "CSWMAJESTICWIND")
  )

dateRange <- lubridate::as_date(c('2017-01-01', '2017-05-01'))
onOrOff <- "OFF"
dfCongest <- getDfSppDaCongestOnPaths(lstPaths = lstPathsInterest, dateRange = dateRange, ftpRoot = LocalDriveDaPrice, toOverrideNaWithMarketWideAverageMcc = FALSE)
dfCongest <- dfCongest %>% left_join(getRtoCalendar("SPP", fromDate = as.character(dateRange[1]), toDate = as.character(dateRange[2]),
                                                    props = c("GMTIntervalEnd", "TIMEOFUSE")) %>%
                                       select(GMTIntervalEnd, Class = TIMEOFUSE))

ind <- dfCongest[['Class']] == onOrOff
dfCongest <- dfCongest[ind, ]

vecQuantiles <- c(0.05, 0.10, 0.25, 0.50)
theSource <-  "SECI.FPLP.GRAYWIND"
theSink <- "WAUE.BEPM.BRADY1"
vecCongest <- unlist(dfCongest %>% filter(Source == theSource, Sink == theSink) %>% select(CONGEST))
c(quantile(vecCongest, probs = vecQuantiles), Mean = mean(vecCongest))
# gg <- dfCongest %>% filter(Source == "OKGECENTWIND", Sink == "OKGESNRWIND") %>% ggplot()
# gg + geom_histogram(aes(x = CONGEST))
#
# quantile(unlist(dfCongest %>% filter(Source == "OKGESNRWIND", Sink == "WR.MW.GMEC.MW") %>% select(CONGEST)), probs = vecQuantiles)
# gg <- dfCongest %>% filter(Source == "OKGESNRWIND", Sink == "WR.MW.GMEC.MW") %>% ggplot()
# gg + geom_histogram(aes(x = CONGEST))

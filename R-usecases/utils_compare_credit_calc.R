CachedData <- new.env()
LocalDriveDaPrice <- "//fs2/world/Analytics/Apps/Qlikview/FTR/SPP_DATA/Markets/DA/LMP_By_SETTLEMENT_LOC"
LocalDriveRefPrice <- "//fs2/world/SMD/Staff/SPP_Auction_Revenue_Rights/PY_2017_2018_ARR_Registration/TCR_REFERENCE_PRICES"

getListPathsFromAnnualResults2017 <- function() {
  if ( is.null(CachedData[["ResultLtcr2017"]]) ) {
    ftpPath <- "ftp://pubftp.spp.org/TCR/HistoricalAllocationResults/2017_Annual_LTCR_Allocation_99_PROD/2017_Annual_LTCR_Allocation_99_PROD_TCR-FROM-LTCR_RESULT.csv"
    CachedData[['ResultLtcr2017']] <- readr::read_csv(ftpPath)
  }

  if ( is.null(CachedData[["ResultAuction2017"]])) {
    ftpPath <- "ftp://pubftp.spp.org/TCR/HistoricalAuctionResults/2017_Annual_TCR_Auction_99_PROD/2017_Annual_TCR_Auction_99_PROD_AUCTION_RESULT.csv"
    CachedData[["ResultAuction2017"]] <- readr::read_csv(ftpPath)
  }

  dfPaths <- rbind(unique(CachedData[['ResultLtcr2017']]) %>% select(Source, Sink),
                   unique(CachedData[['ResultAuction2017']]) %>% select(Source, Sink))
  dfPaths <- unique(dfPaths)

  lstPaths <- purrr::transpose(dfPaths)
}

getListPathsFromAnnualResults2016 <- function() {
  if ( is.null(CachedData[["ResultLtcr2016"]]) ) {
    ftpPath <- "ftp://pubftp.spp.org/TCR/HistoricalAllocationResults/2016_Annual_LTCR_Allocation_99_PROD/2016_Annual_LTCR_Allocation_99_PROD_TCR-FROM-LTCR_RESULT.csv"
    CachedData[['ResultLtcr2016']] <- readr::read_csv(ftpPath)
  }

  if ( is.null(CachedData[["ResultAuction2016"]])) {
    ftpPath <- "ftp://pubftp.spp.org/TCR/HistoricalAuctionResults/2016_Annual_TCR_Auction_99_PROD/2016_Annual_TCR_Auction_99_PROD_AUCTION_RESULT.csv"
    CachedData[["ResultAuction2016"]] <- readr::read_csv(ftpPath)
  }

  dfPaths <- rbind(unique(CachedData[['ResultLtcr2016']]) %>% select(Source, Sink),
                   unique(CachedData[['ResultAuction2016']]) %>% select(Source, Sink))
  dfPaths <- unique(dfPaths)

  lstPaths <- purrr::transpose(dfPaths)
}

getListPathsFromRefPriceFile <- function(periodName = 'Jun_17', onOrOff = 'OFF') {
  dfRefPrice <- getDfRefPriceByPeriod(periodName = periodName, onOrOff = onOrOff, ftpRoot = LocalDriveRefPrice, lstPaths = NULL)
  dfPaths <- dfRefPrice[c('Source', 'Sink')]
  lstPaths <- purrr::transpose(dfPaths)
}

showRefPriceCalcMatchesRefPrice <- function(lstPaths = TestListPaths, periodName = 'Jun_17', onOrOff = 'OFF') {

  dfRefPrice <- getDfRefPriceByPeriod(periodName = periodName, onOrOff = onOrOff, ftpRoot = LocalDriveRefPrice, lstPaths = lstPaths)
  if ( is.null(lstPaths) ) {
    stop("cannot continue without specifications of lstPaths")
    # dfPaths <- dfRefPrice[c('Source', 'Sink')]
    # lstPaths <- purrr::transpose(dfPaths)
  }

  # call the function to calculate the reference price
  dfCalc <- calcRefPriceSpp(lstPaths = lstPaths, periodName = periodName, onOrOff = onOrOff, ftpRoot = LocalDriveDaPrice)

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
  if ( is.null(lstPaths) ) {
    stop("cannot continue without specifications of lstPaths")
    # dfPaths <- dfRefPrice[c('Source', 'Sink')]
    # lstPaths <- purrr::transpose(dfPaths)
  }

  # calculate the number of hours once
  numHours <- getNumHours(periodName = periodName, onOrOff = onOrOff)

  # set the dateRange to historical data one year ago
  dfWorst <- calcIdealWorstCaseValueByPathSepWeights(lstPaths, periodName, onOrOff, numHours = numHours, vecQuantiles = c(0.5, 0.05, 0.02))

  dfCmp <- dfRefPrice  %>% select(Source, Sink, HOURLY_REFERENCE_PRICE) %>%
    left_join(dfWorst %>% select(Source, Sink, PeriodQ0.5PerMwh, PeriodQ0.05PerMwh, PeriodQ0.02PerMwh)) %>%
    tidyr::unite(Path, Source, Sink, sep = ' -- ')

  gg <- ggplot2::ggplot(dfCmp)  +
    geom_point(aes(x = PeriodQ0.5PerMwh, y = HOURLY_REFERENCE_PRICE), alpha = (0.5), colour = I("orange")) +
    geom_point(aes(x = PeriodQ0.05PerMwh, y = HOURLY_REFERENCE_PRICE), alpha = (0.5), colour = I("blue")) +
    geom_point(aes(x = PeriodQ0.02PerMwh, y = HOURLY_REFERENCE_PRICE), alpha = (0.5), colour = I("lightgreen")) +
    geom_abline(intercept = 0, slope = 1, alpha = I(0.5), color = I("red"))

  list(cmp = dfCmp, gg = gg)
}

cmpRefPriceToDayAheadCongestion <- function(lstPaths = TestListPaths, periodName = 'Jun_17', onOrOff = 'OFF') {
  periodInfo <- getPeriodInfo(periodName)

  lstCmp <- cmpRefPriceToIdealCalculationOnWorstValue(lstPaths, periodName, onOrOff)
  dfPrice <- lstCmp[['cmp']]

  dfCongest <- getDfDaCongestDistribution(lstPaths = lstPaths, periodName = periodName, onOrOff = onOrOff, yearOffset = 0, ftpRoot = LocalDriveDaPrice, vecQuantiles = 0.5, useProxy = TRUE)

  dfCmp <- dfPrice %>% left_join( dfCongest %>% tidyr::unite(Path, Source, Sink, sep = " -- ")  %>% select(Path, DaCongest = Mean) )

  # mark the hourly_reference_price failure
  indFailRefPrice <- dfCmp[['DaCongest']] < dfCmp[['HOURLY_REFERENCE_PRICE']]

  # mark the 5%-tile failure
  indFailProposed <- dfCmp[['DaCongest']] < dfCmp[['PeriodQ0.05PerMwh']]

  # mark negative ref price
  indNegRefPrice <- dfCmp[['HOURLY_REFERENCE_PRICE']] < 0


  dfCmp <- dfCmp %>% dplyr::mutate(
    # DaCongestLessRefPrice
    DaCongestLessRefPrice = DaCongest - HOURLY_REFERENCE_PRICE,
    # DaCongestLessProposed
    DaCongestLessProposed = DaCongest - PeriodQ0.05PerMwh,
    # sign
    PosOrNeg = factor( sign(DaCongest), levels = c(-1, 1) )
  )
  N <- nrow(dfCmp)


  # create the main plot
  gg <- ggplot(dfCmp) + geom_point(aes(x = DaCongestLessRefPrice, y = DaCongestLessProposed, color = DaCongest), alpha = I(0.5)) +
    ggplot2::scale_color_gradient2(low = ("red"), mid = "black", high = ("green")) +
    geom_abline(intercept = 0, slope = 1, alpha = I(0.2)) + geom_vline(xintercept = 0, alpha = I(0.2)) + geom_hline(yintercept = 0, alpha = I(0.2))

  # gathering the statistics
  # how many paths are missed
  lstCounts <- purrr::map(c('tr', 'tl', 'br', 'bl'), function(region) {
    ind <- rep(TRUE, nrow(dfCmp))

    topBottom <- stringr::str_sub(region, 1, 1)
    rightLeft <- stringr::str_sub(region, 2, 2)

    if ( topBottom == 't' ) {
      ind <- ind & !indFailProposed
    } else {
      ind <- ind & indFailProposed
    }

    if ( rightLeft == 'r' ) {
      ind <- ind & !indFailRefPrice
    } else {
      ind <- ind & indFailRefPrice
    }

    # calculate the center of right left
    x <- mean(dfCmp[['DaCongestLessRefPrice']] [ind], na.rm = TRUE)
    y <- mean(dfCmp[['DaCongestLessProposed']] [ind], na.rm = TRUE)
    num <- sum(ind, na.rm = TRUE)

    list(x = x, y = y, num = num, numPct = num/N)
  })


  list(cmp = dfCmp, gg = gg,
       lstCounts = lstCounts
       )
}

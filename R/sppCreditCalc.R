#' ..getRefPriceFileNameByPeriod
#' returns a filename convention that is used by SPP to name their reference price
#' TCR_REF_PRICES_12-01-2017_04-01-2018_Winter_Off_Peak.csv
#' TCR_REF_PRICES_08-01-2017_09-01-2017_August_On_Peak.csv
#'
#' @param periodName is the period name referred in the Allocation file or Auction file.
#' @param onOrOff
#'
#' @return
#'
#' @examples
..getRefPriceFileNameByPeriod <- function(periodName = "Jun_17", onOrOff = "OFF") {

  # translate the period to month and year
  periodInfo <- getPeriodInfo(periodName)
  fileName <- paste('TCR_REF_PRICES',
                    # the from date
                    strftime(periodInfo[['FromDate']], '%m-%d-%Y'),
                    # the to date
                    strftime(periodInfo[['ToDate']], '%m-%d-%Y'),
                    # get the long name
                    periodInfo[['PeriodFullName']],
                    # translate the ON or OFF to On_Peak or Off_Peak
                    if(onOrOff == "ON") "On_Peak" else "Off_Peak",
                    sep = "_"
  )

}

#' getDfRefPriceByPeriod
#'
#' @param periodName the periodName referred in the Allocation or Auction file. e.g. 'Jun_17'
#' @param onOrOff either ON or OFF
#' @param ftpRoot points to the ftp site, allows callers to point to local directory if the files have been downloaded
#'
#' @return a data frame containing the reference price table
#' @export
#'
#' @examples
#' dfRefPrice <- getDfRefPriceByPeriod('Jun_17', 'OFF', 'c:/temp/SPP_TCR_REF_PRICES)
#'
getDfRefPriceByPeriod <- function(periodName = "Jun_17", onOrOff = "OFF", ftpRoot = FTPROOT_REFPRICE, lstPaths = NULL) {
  fileName <- ..getRefPriceFileNameByPeriod(periodName, onOrOff)
  fullFilePath <- file.path(ftpRoot, paste0(fileName, '.csv'))

  # read the reference price
  dfRefPrice <- readr::read_delim(fullFilePath, delim = "|")
  dfRefPrice <- dplyr::select(dfRefPrice, Source = SOURCE_LOCATION, Sink = SINK_LOCATION, Class = TIME_OF_USE, HOURLY_REFERENCE_PRICE, PRODUCT_REFERENCE_PRICE, YEAR_1_PROXY_PRICE_IND, YEAR_2_PROXY_PRICE_IND)

  # returns all the paths if lstPaths are not specified
  if (is.null(lstPaths)) {
    return(dfRefPrice)
  }


  dfPaths <- getDfPathsFromPathList(lstPaths)
  dfPaths[['Class']] <- onOrOff

  # join with the reference price
  dfPaths <- dplyr::left_join(dfPaths, dfRefPrice)

  return(dfPaths)
}


#' getDistribution
#' returning the distribution of MCC Sink - MCC Source given the path descriptions specified in lstPaths
#' it will look at the yearOffset if available,
#'
#' @param lstPaths cannot be blank, must be specified with a list of list where Source and Sink must be specified
#' @param periodName e.g. Jun_17
#' @param onOrOff e.g. OFF
#' @param yearOffset e.g. 1 or 2
#'
#' @return
#' @export
#'
#' @examples
getDfDaCongestDistribution <- function(lstPaths, periodName = 'Jun_17', onOrOff = 'OFF', yearOffset = 1, ftpRoot = FTPROOT_DAPRICE, vecQuantiles = c(0.10, 0.25, 0.5), useProxy = TRUE) {
  periodInfo <- getPeriodInfo(periodName)
  fromDate <- periodInfo[['FromDate']]
  toDate <- periodInfo[['ToDate']]

  # support taking a look at more than one yearOffset
  lstByYearOffset <- purrr::map(yearOffset, function(yr) {
    # offset the year to 1 year in the past or 2 years in the past
    lubridate::year(fromDate) <- lubridate::year(fromDate) - yr
    lubridate::year(toDate) <- lubridate::year(toDate) - yr

    # if the fromDate has not happened yet, then further offset the year by 1
    while (fromDate > lubridate::now()) {
      lubridate::year(fromDate) <- lubridate::year(fromDate) - 1
      lubridate::year(toDate) <- lubridate::year(toDate) - 1
    }

    # fromDate and toDate are finalized here
    dateRange <- c(fromDate, toDate)

    # get the congestion by Source and Sink
    dfCongest <- getDfSppDaCongestOnPaths(lstPaths = lstPaths, dateRange = dateRange, ftpRoot = ftpRoot)

    # need to get the ON or OFF time of use
    dfCal <- getRtoCalendar('SPP',
                            fromDate = as.character(dateRange[1]),
                            toDate = as.character(dateRange[2]),
                            props = c(
                              #'DATE', 'HOURENDING', 'START_DT.GMT', 'END_DT.GMT',
                              'GMTIntervalEnd', 'TIMEOFUSE'))

    dfCal <- dplyr::select(dfCal, GMTIntervalEnd, Class = TIMEOFUSE)

    # carve out the rows that are not relevant
    dfCongest <- dplyr::left_join(dfCongest, dfCal)
    ind <- dfCongest[['Class']] == onOrOff

    if (sum(is.na(ind)) > 0) {
      stop('cannot continue, there are some where Class is undefined. ')
    }
    dfSet <- dfCongest[ind, c('GMTIntervalEnd', 'Source', 'Sink', 'CONGEST', 'CONGEST_PROXY')]
  })
  dfSet <- do.call(rbind, lstByYearOffset)


  # replace CONGEST with CONGEST_PROXY if it is NA
  if ( useProxy ) {
    indNA <- is.na(dfSet[['CONGEST']])
    dfSet[['CONGEST']][indNA] <- dfSet[['CONGEST_PROXY']][indNA]
  }

  # spread the congestion so each Path is per column
  dfSet <- dplyr::select(dfSet, GMTIntervalEnd, Source, Sink, CONGEST)
  dfSet <- tidyr::unite(dfSet, Path, Source, Sink, sep = '//')
  dfSet <- tidyr::spread(dfSet, Path, CONGEST)

  # calculate the quantiles for each path
  vecPaths <- names(dfSet)[2:length(dfSet)]
  lstDistr <- purrr::map(vecPaths, function(pathStr) {
    vecCongest <- dfSet[[pathStr]]
    vecDistr <- quantile(vecCongest, vecQuantiles, na.rm = TRUE)
    names(vecDistr) <- paste0('Q', as.character(vecQuantiles))
    vecDistr['Mean'] <- mean(vecCongest)
    data.frame(Path = pathStr, as.list(vecDistr))
  })
  dfDistr <- do.call(rbind, lstDistr)

  # break up Path back to Source and Sink
  dfDistr <- tidyr::separate(dfDistr, Path, c('Source', 'Sink'), sep = '//')

  dfDistr
}

getDfDaCongestDistributionYear1 <- function(lstPaths, periodName = 'Jun_17', onOrOff = 'OFF', ftpRoot = FTPROOT_DAPRICE, vecQuantiles = c(0.10, 0.25, 0.5)) {
  getDfDaCongestDistribution(lstPaths, periodName = periodName, onOrOff = onOrOff, yearOffset = 1, ftpRoot = ftpRoot, vecQuantiles = vecQuantiles)
}

getDfDaCongestDistributionYear2 <- function(lstPaths, periodName = 'Jun_17', onOrOff = 'OFF', ftpRoot = FTPROOT_DAPRICE, vecQuantiles = c(0.10, 0.25, 0.5)) {
  getDfDaCongestDistribution(lstPaths, periodName = periodName, onOrOff = onOrOff, yearOffset = 2, ftpRoot = ftpRoot, vecQuantiles = vecQuantiles)
}


calcRefPriceSpp <- function(lstPaths, periodName = 'Jun_17', onOrOff = 'OFF', ftpRoot = FTPROOT_DAPRICE) {
  dfYear1 <- getDfDaCongestDistributionYear1(lstPaths, periodName, onOrOff, ftpRoot = ftpRoot)
  dfYear2 <- getDfDaCongestDistributionYear2(lstPaths, periodName, onOrOff, ftpRoot = ftpRoot)

  # calculate the Mean to 10%-tile
  dfYear1[['StressTestPrice10th']] <- -dfYear1[['Q0.1']]
  dfYear1[['StressTestPrice25th']] <- -dfYear1[['Q0.25']]

  dfYear2[['StressTestPrice10th']] <- -dfYear2[['Q0.1']]
  dfYear2[['StressTestPrice25th']] <- -dfYear2[['Q0.25']]

  # make sure the reference price is lower than the Mean
  # "The TCR Stress Test Price has a minimum value of zero and thus can never increase the TCR Final Reference Price"
  dfCalc <- dplyr::select(dfYear1, Source, Sink, Mean1 = Mean, StressTestPrice10th1 = StressTestPrice10th, StressTestPrice25th1 = StressTestPrice25th)
  dfCalc <- dplyr::left_join(dfCalc,
                                     dplyr::select(dfYear2, Source, Sink, Mean2 = Mean, StressTestPrice10th2 = StressTestPrice10th, StressTestPrice25th2 = StressTestPrice25th)
                                     )
  dfCalc[['WeightedMean']] <- 0.75 * dfCalc[['Mean1']] + 0.25 * dfCalc[['Mean2']]

  # calculate the default reference price assuming the paths are negative in WeightedMean value
  dfCalc[['RefPrice']] <- dfCalc[['WeightedMean']] - (0.75 * dfCalc[['StressTestPrice10th1']]
                                                                      + 0.25 * dfCalc[['StressTestPrice10th2']])
  indPos <- dfCalc[['WeightedMean']] > 0
  dfCalc[['RefPrice']] [indPos] <- dfCalc[['WeightedMean']] [indPos] - (0.75 * dfCalc[['StressTestPrice25th1']] [indPos]
                                                                      + 0.25 * dfCalc[['StressTestPrice25th2']] [indPos])


  # override the reference price if it is less than the weighted mean
  indLower <- dfCalc[['RefPrice']] > dfCalc[['WeightedMean']]
  dfCalc[['RefPrice']] [indLower] <- dfCalc[['WeightedMean']] [indLower]


  # need to get the product reference price by multiplying the number of hours
  periodInfo <- getPeriodInfo(periodName)
  cal <- getRtoCalendar('SPP', fromDate = as.character(periodInfo[['FromDate']]),
                 toDate = as.character(periodInfo[['ToDate']]), props = 'TIMEOFUSE')
  numHours <- sum(cal[['TIMEOFUSE']] == onOrOff)
  dfCalc[['RefPriceProduct']] <- dfCalc[['RefPrice']] * numHours

  dfCalc
}


#' calcIdealWorstCaseValueByPath
#'
#' @param aPath
#' @param dateRange
#' @param ftpRoot
#'
#' @return
#' @export
#'
#' @examples
#' aPath <- list(Source = 'WR.MW.SMOK2.CU', Sink = 'SPRM_SPRM')
#' dateRange <- c('2016-06-01', '2016-07-01')
#' calcIdealWorstCaseValueByPath(aPath, dateRange = dateRange)
#'
calcIdealWorstCaseValueByPath <- function(lstPaths, periodName = 'Jun_17', onOrOff = 'OFF', ftpRoot = LocalDriveDaPrice, numHours = NULL, vecQuantiles = 0.05) {
  dfStatsYear12 <- getDfDaCongestDistribution(lstPaths, periodName = periodName, onOrOff = onOrOff, yearOffset = c(1,2), ftpRoot = ftpRoot, vecQuantiles = vecQuantiles)


  hourlyAverage <- dfStatsYear12[['Mean']]

  # if numHours is NULL, make it equal to the sample
  if (is.null(numHours)) {
    numHours <- getNumHours(periodName = periodName, onOrOff = onOrOff)
  }

  for ( theQuantile in vecQuantiles ) {
    labelQuantile <- paste0('Q', as.character(theQuantile))
    hourlyXPct <- dfStatsYear12[[labelQuantile]]

    # calculate the Monthly product accordingly
    labelPeriodPrice <- paste0('Period', labelQuantile)
    dfStatsYear12[[labelPeriodPrice]] <- numHours * hourlyAverage - sqrt(numHours) * (hourlyAverage - hourlyXPct)
    dfStatsYear12[[paste0(labelPeriodPrice, 'PerMwh')]] <- dfStatsYear12[[labelPeriodPrice]] / numHours
  }

  dfStatsYear12
}



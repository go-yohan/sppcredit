CacheDaPriceByDate <- new.env()

ColSpecs <- list()
ColSpecs[['DA_PRICE']] <- list(Interval              = readr::col_character(),
                               GMTIntervalEnd        = readr::col_character(),
                               `Settlement Location` = readr::col_character(),
                               Pnode                 = readr::col_character(),
                               LMP                   = readr::col_double(),
                               MLC                   = readr::col_double(),
                               MCC                   = readr::col_double(),
                               MEC                   = readr::col_double())


#' getDfDaPriceSpp
#'
#' @param dateRange a 2-tuple that specifies a from date and a to date
#' @param ftpRoot
#'
#' @return a dataframe that gives the price information on all SPP nodes
#' @export
#'
#' @examples
getDfDaPriceSpp <- function(dateRange, ftpRoot = FTPROOT_DAPRICE) {
  dateRange <- lubridate::as_date(dateRange)
  vecDates <- seq(dateRange[1], dateRange[2], by = 'days')
  # non-inclusive on the last one
  vecDates <- vecDates[1:(length(vecDates) - 1)]

  lstPrices <- purrr::map(vecDates, function(dateObj) {
    dateStr <- as.character(dateObj)
    dfDaPrice <- CacheDaPriceByDate[[dateStr]]

    # if it is not found in cache, go download and cache it
    if (is.null(dfDaPrice)) {
      dfDaPrice <- ..getDfDaPriceSppOnDate(dateObj, ftpRoot = ftpRoot)
      CacheDaPriceByDate[[dateStr]] <- dfDaPrice
    }

    dfDaPrice
  })

  do.call(rbind, lstPrices)
}

getDfSppDaCongestOnPaths <- function(lstPaths, dateRange, ftpRoot = FTPROOT_DAPRICE, toOverrideNaWithMarketWideAverageMcc = TRUE) {

  dfPaths <- getDfPathsFromPathList(lstPaths)

  dfDaPrice <- getDfDaPriceSpp(dateRange, ftpRoot = ftpRoot)

  # try to make sure that all GMTIntervalEnd is populated
  vecGmt <- unique(dfDaPrice[['GMTIntervalEnd']])
  lstPathsComplete <- purrr::map(vecGmt, function(gmtStr) {
    dfPaths[['GMTIntervalEnd']] <- gmtStr
    dfPaths
  })
  dfPaths <- do.call(rbind, lstPathsComplete)

  # calculate the SourceMCC and SinkMCC
  dfPaths <- dplyr::left_join(dfPaths, dplyr::select(dfDaPrice, GMTIntervalEnd, Source = `Settlement Location`, SourceMCC = MCC))
  dfPaths <- dplyr::left_join(dfPaths, dplyr::select(dfDaPrice, GMTIntervalEnd, Sink = `Settlement Location`, SinkMCC = MCC))

  # calcualte the congestion value
  dfPaths[['CONGEST']] <- dfPaths[['SinkMCC']] - dfPaths[['SourceMCC']]

  # check if the SourceMCC or SinkMCC are NA
  indNA <- is.na(dfPaths[['CONGEST']])

  dfPaths[['CONGEST_PROXY']] <- NA
  if ( sum(indNA) > 0 ) {
    # remove the NA
    # unless we are to override NA with market wide average MCC like what is suggested in SPP credit calculation today
    if (toOverrideNaWithMarketWideAverageMcc) {
      # calculate the market wide average by GMTIntervalEnd
      dfMarketWide <- dplyr::select(dfDaPrice, GMTIntervalEnd, MCC)
      dfMarketWide <- dplyr::group_by(dfMarketWide, GMTIntervalEnd)
      dfMarketWide <- dplyr::summarise(dfMarketWide, MarketMCC = mean(MCC))

      dfPaths <- dplyr::left_join(dfPaths, dfMarketWide)

      # will have to calculate the is.na on SourceMCC and SinkMCC
      indNASource <- is.na(dfPaths[['SourceMCC']])
      indNASink <- is.na(dfPaths[['SinkMCC']])
      indNABoth <- indNASource & indNASink

      dfPaths[['CONGEST_PROXY']] [indNASource] <- dfPaths[['SinkMCC']] [indNASource] - dfPaths[['MarketMCC']] [indNASource]
      dfPaths[['CONGEST_PROXY']] [indNASink] <- dfPaths[['MarketMCC']] [indNASink] - dfPaths[['SourceMCC']] [indNASink]
      dfPaths[['CONGEST_PROXY']] [indNABoth] <- 0
    }
  }

  dplyr::select( dfPaths, Source, Sink, GMTIntervalEnd, SourceMCC, SinkMCC, CONGEST, CONGEST_PROXY )
}

..getDaPriceFileNameByDate <- function(dateObj) {
  paste0('DA-LMP-SL-', strftime(dateObj, '%Y%m%d0100'), '.csv')
}

..getDfDaPriceSppOnDate <- function(dateObj, ftpRoot = FTPROOT_DAPRICE) {
  # there is a breakpoint of 7/21/2016 where any dates after that follows the new file name with /By_Day subdirectory
  # e.g. ftp://pubftp.spp.org/Markets/DA/LMP_By_SETTLEMENT_LOC/2017/05/By_Day/DA-LMP-SL-201705040100.csv
  # e.g. ftp://pubftp.spp.org/Markets/DA/LMP_By_SETTLEMENT_LOC/2016/07/DA-LMP-SL-201607210100.csv

  isFtpRootLocalDrive <- stringr::str_sub(ftpRoot, 1, 3) != 'ftp'

  rootDir <- file.path(ftpRoot,
                       strftime(dateObj, '%Y'),
                       strftime(dateObj, '%m')
  )

  daPriceFileName <- ..getDaPriceFileNameByDate(dateObj)

  fullFilePath <- file.path(rootDir, daPriceFileName)

  # if it is actual ftp site
  if ( !isFtpRootLocalDrive ) {
    if ( dateObj > lubridate::as_date('2016-07-21') ) {
      fullFilePath <- file.path(rootDir, 'By_Day', daPriceFileName)
    }

  } else {

    # if it is a local drive just test if the file exists or not
    if ( !file.exists(fullFilePath) ) {
      fullFilePath <- file.path(rootDir, 'By_Day', daPriceFileName)
    }

    if ( !file.exists(fullFilePath) ) {
      message('the file does not exists: ', fullFilePath)
      stop('cannot find the proper da price for the day: ', as.character(dateObj))
    }
  }

  message(fullFilePath)
  readr::read_csv(fullFilePath, col_types = ColSpecs[['DA_PRICE']])
}


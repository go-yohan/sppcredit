#'
#' getBidPriceQuantityPair
#'
#' @param bidEntry an entry from a bid sheet
#'
#' @return a list of PRICE and MW
#' @export
#'
#' @examples
getBidPriceQuantityPair <- function(bidEntry) {
  ind <- 1
  lst <- list(PRICE = c(), MW = c())
  while (TRUE) {
    nmMw <- paste('Bid Curve[', ind, '](MW)', sep = '')
    nmPrice <- paste('Bid Curve[', ind, ']($/MW)', sep = '')

    if ( is.null(bidEntry[[nmMw]]) || is.na(bidEntry[[nmMw]])) {
      break
    }
    lst[['MW']] <- c(lst[['MW']], bidEntry[[nmMw]])
    lst[['PRICE']] <- c(lst[['PRICE']], bidEntry[[nmPrice]])

    # go to the next segment
    ind <- ind + 1
  }

  lst
}


#' calcBidCreditNeeded
#' calcaultes the credit needed for a particular bid entry
#'
#' @param bidEntry
#' @param bidOffset
#' @param netting
#'
#' @return
#' @export
#'
#' @examples
#' bidEntry <- list(
#'     `Bid Curve[1](MW)` = 0,
#'     `Bid Curve[1]($/MW)` = 10,
#'     `Bid Curve[2](MW)` = 20,
#'     `Bid Curve[2]($/MW)` = 0
#' )
#' creditRequired <- calcBidCreditNeeded(bidEntry)
#' testthat::expect_equal(creditRequired, 20 * 0.5 * 10 * 0.5)
#'
#' bidEntry <- list(
#'     `Bid Curve[1](MW)` = 0,
#'     `Bid Curve[1]($/MW)` = 10,
#'     `Bid Curve[2](MW)` = 40,
#'     `Bid Curve[2]($/MW)` = -10,
#' )
#' creditRequired <- calcBidCreditNeeded(bidEntry)
#' testthat::expect_equal(creditRequired, 20 * 0.5 * 10 * 0.5)
calcBidCreditNeeded <- function(bidEntry, bidOffset = 0, netting = FALSE) {
  # bid entry looks at the construct that is created by buildBid
  # and return the credit requirement given the worstCasePrice
  # netting is by default FALSE, but if set to TRUE, the bidOffset
  # is netted to the bidEntry before calculating the worst outcome.
  #
  # in CAISO, the bidOffset is the negative of the credit margin
  # because for every bidEntry, CAISO may require you to post collateral
  # higher by the fatness of the distribution.
  #
  # we can think of bidOffset too as a worst case scenario which implies that
  # we look at the potential worst-case payout of the particular TCR and
  # only require the credit requirement on the difference.
  creditRequired <- 0

  # if bidOffset is NA, treat it as zero
  if (is.null(bidOffset) || is.na(bidOffset)) {
    bidOffset <- 0
  }

  lstPq <- getBidPriceQuantityPair(bidEntry)
  vecPrice <- lstPq[['PRICE']]
  vecMw <- lstPq[['MW']]

  if ( netting == TRUE ) {
    vecPrice <- vecPrice - bidOffset
  }

  # detect a self-convert, return 0 as it is equivalent to bidding at zero $/MW
  if ( length(vecMw) == 1 ) {
    vecCreditSegments <- c(0)

  } else {

    # calculate vecCreditSegments
    # by taking a look at the segment and calculate the maximum for bid and minimum for offer
    vecCreditSegments <- c()
    for (ind in 1:(length(vecPrice)-1)) {

      p1 <- vecPrice[ind]
      p2 <- vecPrice[ind+1]
      q1 <- vecMw[ind]
      q2 <- vecMw[ind+1]

      # weird: found a historical bid where q1 = q2
      # if they are equal just take the worst one
      m <- (p2 - p1) / (q2 - q1)

      if ( m == 0 || m == Inf || m == -Inf) {
        # if there is no slope, pick the vertices and maximize or minimize those
        vecCreditSegments <- c(vecCreditSegments, p1 * q1,  p2 * q2)

      } else {
        # calculate the maximizer or minimizer
        q_star <- (-p1 + q1 * m) / ( 2 * m )
        if (q_star < q1) {
          q_star <- q1
        } else if (q_star > q2) {
          q_star <- q2
        }

        # calculate the price at q_star
        p_star <- p1 + m * ( q_star - q1 )

        vecCreditSegments <- c(vecCreditSegments, p_star * q_star)
      }


    }
  }


  # go through each segment and calculate the worst-case scenario on each segment
  # and then take the maximum if it is a bid or take the minimum if it is an offer
  isBidBuy <- length(vecPrice) == 1 || all(diff(vecPrice) <= 0)

  if (isBidBuy) {
    creditRequired <- (max(vecCreditSegments))
  } else {
    creditRequired <- (max(-vecCreditSegments))
  }

  if ( netting == FALSE ) {
    creditRequired <- creditRequired - bidOffset * vecMw[length(vecMw)]
  }

  creditRequired
}

..writeCsvLineByEntry <- function(entry, colPathDescr) {
  # line now contains the description of the bid
  line <- purrr::map_chr(colPathDescr, function(colName) {
    val <- entry[[colName]]
    if (is.null(val) || is.na(val))
      val <- ''
    else
      val <- as.character(val)

    val
  })


  return (paste(line, collapse = ','))
}


#' writeCsvTemplate
#' write the csv template based on the entries and the header specification
#'
#' @param lstEntries the list of entries
#' @param header header specifications (the column of the csv template)
#' @param filePath the output file path
#'
#' @return NULL
#' @export
#'
#' @examples
writeCsvTemplate <- function(lstEntries, header, filePath) {

  csvLines <- purrr::map(lstEntries, function(entry) {
    if ( is.null(entry) ) {
      return(NULL)
    }
    ..writeCsvLineByEntry(entry, colPathDescr = header)
  })
  csvLines <- do.call(c, csvLines)

  headerLine <- (paste(header, collapse = ','))

  fileConn <- file(filePath)
  linesToWrite <- c(headerLine,csvLines)
  writeLines(linesToWrite[!is.na(linesToWrite)], fileConn)
  close(fileConn)
}


#' readSppBidFile
#'
#' @param filePath
#' @param maxSegments default to 5
#'
#' @return a data frame that show a complete segments of length maxSegments (filling empty segments with NA)
#' @export
#'
#' @examples
readSppBidFile <- function(filePath, maxSegments = 5) {

  colTypes <- list(
    `Bid ID` = readr::col_integer(),
    `TCR ID` = readr::col_integer(),
    Description = readr::col_character(),
    `Asset Owner` = readr::col_character(),
    Source = readr::col_character(),
    Sink = readr::col_character(),
    `OPT/OBL` = readr::col_character(),
    Class = readr::col_character(),
    Type = readr::col_character(),
    Period = readr::col_character()
  )

  # add the Bid Curve up to the number of segments
  for ( ind in 1:maxSegments ) {
    nmBidMw <- paste0("Bid Curve[", ind, "](MW)")
    nmBidPrice <- paste0("Bid Curve[", ind, "]($/MW)")
    colTypes[[nmBidMw]] <- readr::col_double()
    colTypes[[nmBidPrice]] <- readr::col_double()
  }

  # create the unnamed columns to be used for reading
  colTypesUnnamed <- colTypes
  names(colTypesUnnamed) <- NULL

  dfData <- readr::read_csv(filePath, skip = 1, col_names = FALSE, col_types = do.call(readr::cols, colTypesUnnamed))

  N_data <- length(dfData)
  N_cols <- length(colTypes)

  # name the columns properly
  names(dfData) <- names(colTypes)[1:length(dfData)]

  # make sure they all have the same column length
  for (nm in names(colTypes)[(1+N_data):N_cols]) {
    dfData[[nm]] <- NA
  }

  dfData
}


#' readSppHistoricalBidFile
#' read the SPP historical bid and offer file
#'
#' @param filePath
#' @param maxSegments
#'
#' @return
#' @export
#'
#' @examples
readSppHistoricalBidFile <- function(filePath, maxSegments = 5) {

  colTypes <- list(
    Source = readr::col_character(),
    Sink = readr::col_character(),
    HedgeType = readr::col_character(),
    Class = readr::col_character(),
    Type = readr::col_character(),
    Round = readr::col_character(),
    MW = readr::col_double(),
    PricePerMW = readr::col_double(),
    CaseID = readr::col_integer(),
    MarketName = readr::col_character()
  )


  # create the unnamed columns to be used for reading
  colTypesUnnamed <- colTypes
  names(colTypesUnnamed) <- NULL

  dfData <- readr::read_csv(filePath, skip = 1, col_names = FALSE, col_types = do.call(readr::cols, colTypesUnnamed))

  N_data <- length(dfData)
  N_cols <- length(colTypes)

  # name the columns properly
  names(dfData) <- names(colTypes)[1:length(dfData)]


  # add a bid id so each chunk is unique
  dfData[['Bid ID']] <- NA
  ind <- !is.na(dfData[['Source']])
  bidIds <- 1:(sum(ind))
  dfData[['Bid ID']] [ind] <- bidIds

  dfData[['lineno']] <- 1:nrow(dfData)
  dfData[['lineno.start']] [ind] <- dfData[['lineno']] [ind]

  # fill in the NA
  dfData <- tidyr::fill(dfData, `Bid ID`, lineno.start)

  # calculate the segment index
  dfData[['SegmentIndex']] <- dfData[['lineno']] - dfData[['lineno.start']] + 1

  # spread the MW
  dfMw <- tidyr::spread(dfData %>% select(`Bid ID`, SegmentIndex, MW), SegmentIndex, MW)
  dfPrice <- tidyr::spread(dfData %>% select(`Bid ID`, SegmentIndex, PricePerMW), SegmentIndex, PricePerMW)

  maxSegments <- length(dfMw) - 1
  names(dfMw)[1+(1:maxSegments)] <- paste0("Bid Curve[", 1:maxSegments, "](MW)")
  names(dfPrice)[1+(1:maxSegments)] <- paste0("Bid Curve[", 1:maxSegments, "]($/MW)")

  # join them
  dfDataSpread <- dplyr::select( dfData, `Bid ID`, Source, Sink, HedgeType, Class, Type, Round, CaseID, MarketName )
  dfDataSpread <- dplyr::filter( dfDataSpread, !is.na(Source))
  dfDataSpread <- dplyr::left_join(dfDataSpread, dfMw)
  dfDataSpread <- dplyr::left_join(dfDataSpread, dfPrice)


  dfDataSpread
}

#' getHistoricalBidFileName
#'
#' @param periodName e.g. Spring_17 or Jun_16
#'
#' @return the expected historical bid offer csv file name
#' @export
#'
#' @examples
#' getHistoricalBidFileName('Spring_17')
#' # [1] "2016_Annual_TCR_Auction_99_PROD__Spring_17_HISTORICAL_BID_OFFER.CSV"
getHistoricalBidFileName <- function(periodName) {
  periodInfo <- getPeriodInfo(periodName)
  if ( is.null(periodInfo) ) {
    return (NULL)
  }
  paste(periodInfo[['PlanningYear']], 'Annual_TCR_Auction_99_PROD', periodName, 'HISTORICAL_BID_OFFER.CSV', sep = '_')
}

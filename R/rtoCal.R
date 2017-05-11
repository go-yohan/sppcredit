### List of Time Zones:
### https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
### for CAISO use US/Pacific
### for MISO use EST
### for PJM use EST5EDT

STANDARD_FORMATS <- list(
  DATE = '%Y-%m-%d',
  START_DT = '%Y-%m-%d %H:%M:%S'
)

STANDARD_CAL_PROPERTIES <- c('START_DT', 'START_DT.GMT', 'DATE', 'HOURENDING', 'TIMEOFUSE')

#' @title create a master calendar for a specific RTO
#'
#' @description
#' \code{getRtoCalendar} creates a master calendar and allow users to specify the calendar properties
#'
#' @details
#' Recognized properties are:
#' START_DT, START_DT.GMT, DATE, DATE.YEARMONTH, END_DT, HOURENDING, TIMEOFUSE, SEASON, PO_INTERVAL, PLANNING_YEAR
#'
#'
#' @param marketName an RTO name: MISO, SPP, CAISO,
#' @param fromDate a date
#' @param toDate a date, if NULL then one-day calendar is assumed.
#' @param props a character vector that lists the requested properties
#'
#' @examples
#' cal <- getRtoCalendar('SPP', '2017-01-01','2017-04-01', props=c('DATE.YEARMONTH', 'START_DT', 'START_DT.GMT', 'TIMEOFUSE'))
#'
#' @export
getRtoCalendar <- function(marketName, fromDate, toDate = NULL, props = STANDARD_CAL_PROPERTIES, byMinutes = NULL) {
  cal <- RtoCalendar$new(marketName)
  print(cal$time_zone)
  if (is.character(fromDate))
    fromDate <- lubridate::ymd(fromDate, tz=cal$time_zone)

  if (is.null(toDate)) {
    toDate <- fromDate + lubridate::ddays(1)
  } else {
    if (is.character(toDate))
      toDate <- lubridate::ymd(toDate, tz=cal$time_zone)
  }

  data.frame( cal$createMasterCalendar(fromDate, toDate, props, byMinutes = byMinutes), stringsAsFactors = FALSE )
}

#' @export
RtoCalendar <-
  R6::R6Class("RtoCalendar",
              public = list(
                initialize = function(marketName) {
                  private$initByMarket(marketName)
                },

                # createMasterCalendar create the column START_DT.GMT and START_DT
                createMasterCalendar = function(fromDate, toDate, props=NULL, byMinutes = NULL) {
                  if (is.null(props))
                    props = STANDARD_CAL_PROPERTIES

                  dtNow <- lubridate::now()
                  localTz <- private$..tz

                  fromDateTime <- toDateTime <- NULL

                  if (missing(fromDate)) {
                    fromDate <- lubridate::floor_date( lubridate::as_datetime(dtNow), 'year' )
                    fromDate <- lubridate::force_tz(fromDate, localTz)
                  }
                  fromDateTime <- lubridate::as_datetime(fromDate, tz=localTz)

                  if (missing(toDateTime)) {
                    toDate <- lubridate::ceiling_date( lubridate::as_datetime(dtNow), 'year' )
                    toDate <- lubridate::force_tz(toDate, localTz)
                  }
                  toDateTime <- lubridate::as_datetime(toDate, tz=localTz)

                  fromDateTimeGmt <- lubridate::as_datetime(fromDateTime, tz='UTC')
                  toDateTimeGmt <- lubridate::as_datetime(toDateTime, tz='UTC')
                  toDateTimeGmt <- toDateTimeGmt - lubridate::dhours(1)

                  private$..lstCal[['START_DT.GMT']] <- seq(fromDateTimeGmt, toDateTimeGmt, by='hour')
                  timeSpanPerEntry <- lubridate::dhours(1)
                  # check if minutes is requested
                  if (!is.null(byMinutes)) {
                    timeSpanPerEntry <- lubridate::dminutes(byMinutes)
                    N <- 60 / byMinutes # how many periods are there
                    theMinutes <- seq(0,55,byMinutes)
                    private$..lstCal[['START_DT.GMT']] <- rep(private$..lstCal[['START_DT.GMT']], each = N) + lubridate::dminutes(theMinutes)
                  }
                  private$..lstCal[['START_DT']] <- lubridate::as_datetime(private$..lstCal[['START_DT.GMT']], tz=localTz)

                  vDate <- lubridate::floor_date(private$..lstCal[['START_DT']], 'day')
                  lubridate::tz(vDate) <- localTz
                  private$..lstCal[['DATE']] <- vDate
                  private$..lstCal[['DATE.YEARMONTH']] <- lubridate::floor_date(vDate, 'month')

                  private$..lstCal[['END_DT']] <- private$..lstCal[['START_DT']] + timeSpanPerEntry
                  private$..lstCal[['END_DT.GMT']] <- private$..lstCal[['START_DT.GMT']] + timeSpanPerEntry

                  # hourending should be derived from hour(START_DT) + 1
                  # i.e. when daylight saving starts, there should not be HOURENDING: 3
                  vHourEnding  <- 1 + lubridate::hour(private$..lstCal[['START_DT']])
                  private$..lstCal[['HOURENDING']] <- vHourEnding

                  # if TIMEOFUSE is needed
                  if ('TIMEOFUSE' %in% props)  private$calcTimeOfUse()

                  # if SEASON is needed
                  if ('SEASON' %in% props)  private$calcSeason()

                  # if PO_INTERVAL is needed
                  if ('PO_INTERVAL' %in% props)  private$calcPoInterval()

                  # if PLANNING_YEAR is needed
                  if ('PLANNING_YEAR' %in% props)  private$calcPlanningYear()

                  # if INTERVAL25 is needed
                  if ('INTERVAL25' %in% props) private$calcInterval25()

                  # if HOURENDINGSTAR is needed
                  if ('HOURENDINGSTAR' %in% props) private$calcHourEndingStar()

                  # if GMTIntervalEnd is needed
                  if ('GMTIntervalEnd' %in% props) private$calcGmtIntervalEnd()


                  # check if all requested properties are included
                  ind <- props %in% names(private$..lstCal)
                  if (sum(!ind) > 0) {
                    warning(paste( "Not sure how to calculate properties: ",
                                   paste(props[!ind], collapse=', ')))
                  }

                  return (private$..lstCal[props[ind]])
                }


              ),
              private = list(
                ..marketName = NULL,
                ..tz = NULL,
                ..isSaturdayAllOffpeak = TRUE,
                ..peakHours = c(6,22),
                ..lstCal = list(),   # final calendar

                createDateTimeVector = function(fromDateTime, toDateTime, by="hour") {
                  return (seq(fromDateTime, toDateTime, by=by))
                },

                initByMarket = function(marketName) {

                  private$..marketName <- marketName
                  private$..tz <- 'US/Pacific'
                  private$..isSaturdayAllOffpeak  <- FALSE
                  private$..peakHours <- c(6,22)

                  if (marketName == 'MISO') {
                    private$..tz <- 'EST'
                    private$..peakHours <- c(6,22)
                    private$..isSaturdayAllOffpeak <- TRUE

                  } else if (marketName == 'PJM') {
                    private$..tz <- 'EST5EDT'
                    private$..peakHours <- c(7,23)
                    private$..isSaturdayAllOffpeak <- TRUE

                  } else if (marketName == 'SPP') {
                    private$..tz <- 'CST6CDT'
                    private$..peakHours <- c(6,22)
                    private$..isSaturdayAllOffpeak <- TRUE

                  } else if (marketName == 'CAISO') {
                    private$..tz <- 'US/Pacific'
                    private$..peakHours <- c(6,22)
                    private$..isSaturdayAllOffpeak <- FALSE
                  }

                },

                calcTimeOfUse = function() {
                  vDateTime <- private$..lstCal[['START_DT']]

                  # check for NERC holidays
                  yy <- unique( lubridate::year(vDateTime) )
                  holidayDates <- timeDate::holidayNERC(yy, FinCenter="GMT")

                  vDates <- lubridate::floor_date(vDateTime, 'day')
                  vWeekday <- lubridate::wday(vDates)
                  vHour <- lubridate::hour(vDateTime)

                  N <- length(vDateTime)
                  vTou <- rep('ON', N)

                  # calculate the index whose TIMEOFUSE is to be set to OFF
                  indOff <- rep(FALSE, N)
                  indOff <- indOff | lubridate::as_date(vDates) %in% lubridate::as_date(holidayDates)  # NERC Holiday
                  indOff <- indOff | vWeekday == 1  # Sunday
                  if (private$..isSaturdayAllOffpeak) {
                    indOff <- indOff | vWeekday == 7 # Saturday
                  }
                  indOff <- indOff | vWeekday == 1  # Sunday
                  indOff <- indOff | vHour < private$..peakHours[1]
                  indOff <- indOff | vHour >= private$..peakHours[2]

                  vTou[indOff] <- 'OFF'

                  private$..lstCal[['TIMEOFUSE']] <- vTou

                },

                calcSeason = function() {
                  vMonth <- lubridate::month(private$..lstCal[['DATE']])
                  vSeason <- NULL
                  rto <- private$..marketName

                  if (rto == 'MISO' || rto == 'PJM') {
                    vSeason <- rep('WINTER', length(vMonth))
                    vSeason[vMonth >=6 & vMonth <=8] <- 'SUMMER'
                    vSeason[vMonth >=9 & vMonth <=11] <- 'FALL'
                    vSeason[vMonth >=3 & vMonth <=5] <- 'SPRING'

                    vSeason <- factor(vSeason, levels=c('SUMMER', 'FALL', 'WINTER', 'SPRING'))

                  } else if (rto == 'SPP') {
                    vSeason <- rep('WINTER', length(vMonth))
                    vSeason[vMonth ==6] <- 'JUNE'
                    vSeason[vMonth ==7] <- 'JULY'
                    vSeason[vMonth ==8] <- 'AUGUST'
                    vSeason[vMonth ==9] <- 'SEPTEMBER'
                    vSeason[vMonth >=10 & vMonth <=11] <- 'FALL'
                    vSeason[vMonth >=4 & vMonth <=5] <- 'SPRING'

                    vSeason <- factor(vSeason, levels=c('JUNE', 'JULY', 'AUGUST', 'SEPTEMBER', 'FALL', 'WINTER', 'SPRING'))

                  } else if (rto == 'CAISO') {

                    vSeason <- paste0('S0', floor( (vMonth - 1)/3.0) + 1)
                    vSeason <- factor(vSeason, levels=c('S01', 'S02', 'S03', 'S04'))
                  }

                  private$..lstCal[['SEASON']] <- vSeason

                },

                calcPoInterval = function() {
                  ## given the rto calendar, calculate the interval 0000 0060 0120 ... 1440 1500
                  ## relieds on the START_DT.GMT and HOURENDING and DATE
                  ## steps:
                  ##   look for START_DT.GMT with HOURENDING 1 and register to a new column HE1GMT
                  ##   take the diff = START_DT.GMT - HE1GMT
                  ##   convert to numeric as.numeric(diff, units="hours")
                  ##   and multiply the number of hours by 60, and convert to four digit numbers str_pad(60,4,pad='0') to produce 0060

                  vDate <- private$..lstCal[['DATE']]
                  vDateGmt <- lubridate::as_datetime(vDate, tz='UTC')

                  vDiff <- private$..lstCal[['START_DT.GMT']] - vDateGmt
                  vDiff <- as.numeric(vDiff, units = 'hours')

                  private$..lstCal[['PO_INTERVAL']] <- (1+vDiff) * 60
                },

                calcPlanningYear = function() {
                  if (private$..marketName == 'CAISO') {
                    # CAISO plannig year is the calendar year
                    private$..lstCal[['PLANNING_YEAR']] <- lubridate::year(private$..lstCal[['DATE']])

                  } else {
                    # MISO, PJM, and SPP planning year starts with June
                    private$..lstCal[['PLANNING_YEAR']] <- lubridate::year(private$..lstCal[['DATE']])
                    ind <- lubridate::month(private$..lstCal[['DATE']]) < 6
                    private$..lstCal[['PLANNING_YEAR']][ind] <- -1 + private$..lstCal[['PLANNING_YEAR']][ind]

                  }
                },

                calcInterval25 = function() {
                  # will return an interval from 1 to 25 where 25 is only applicable on the day
                  # when daylight saving ends, i.e. when there are 25 days on a day
                  # get the corresponding START_DT.GMT for each DATE
                  vDate <- private$..lstCal[['DATE']]
                  vDateGmt <- lubridate::as_datetime(vDate, tz='UTC')

                  vDiff <- private$..lstCal[['START_DT.GMT']] - vDateGmt
                  vDiff <- as.numeric(vDiff, units = 'hours')

                  private$..lstCal[['INTERVAL25']] <- (1+vDiff)
                },

                calcHourEndingStar = function() {
                  # will return the same as hourending except haivng a star on the second HourEnding 2
                  private$..lstCal[['HOURENDINGSTAR']] <- as.character(private$..lstCal[['HOURENDING']])

                  # reuse the calcInterval25 and identify the HourEnding 2* as HourEnding == 2 and Interval25 == 3
                  private$calcInterval25()
                  ind <- private$..lstCal[['HOURENDING']] == 2 & private$..lstCal[['INTERVAL25']] == 3
                  private$..lstCal[['HOURENDINGSTAR']][ind] <- '2*'

                },

                calcGmtIntervalEnd = function() {
                  private$..lstCal[['GMTIntervalEnd']] <- strftime(private$..lstCal[['END_DT.GMT']], '%m/%d/%Y %H:00:00', tz = 'UTC')
                }


              ),
              active = list(
                time_zone = function() {
                  return( private$..tz )
                }

              )
  )







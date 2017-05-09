PeriodToDateRange <- tibble::tribble(
  ~Period, ~FromDate, ~ToDate, ~PeriodFullName,
  "Jan", "2000-01-01", "2000-02-01", "January",
  "Feb", "2000-02-01", "2000-03-01", "February",
  "Mar", "2000-03-01", "2000-04-01", "March",
  "Apr", "2000-04-01", "2000-05-01", "April",
  "May", "2000-05-01", "2000-06-01", "May",
  "Jun", "2000-06-01", "2000-07-01", "June",
  "Jul", "2000-07-01", "2000-08-01", "July",
  "Aug", "2000-08-01", "2000-09-01", "August",
  "Sep", "2000-09-01", "2000-10-01", "September",
  "Oct", "2000-10-01", "2000-11-01", "October",
  "Nov", "2000-11-01", "2000-12-01", "November",
  "Dec", "2000-12-01", "2001-01-01", "December",
  "Fall", "2000-10-01", "2000-12-01", "Fall",
  "Winter", "2000-12-01", "2001-04-01", "Winter",
  "Spring", "2000-04-01", "2000-06-01", "Spring"
)
PeriodToDateRange <- dplyr::mutate(PeriodToDateRange,
                                   FromDate = lubridate::as_date(FromDate),
                                   ToDate = lubridate::as_date(ToDate))


#' getPeriodInfo
#' returns a data frame that describes more information give a periodName
#' for example a periodName of 'Jun_17' will return the following list:
#'   Period : 'Jun'
#'   FromDate: (date object) '2017-06-01'
#'   ToDate: (date object) '2017-07-01'
#'   PeriodFullName: 'June'
#'
#' @param periodName
#'
#' @return a list decsribing more details about the periodName
#' @export
#'
#' @examples
#' getPeriodInfo('Jun_17')
#' getPeriodInfo('Fall_17')
#' getPeriodInfo('Spring_18')
getPeriodInfo <- function(periodName = 'Jun_17') {
  # break up the periodName to period and year
  mm <- unlist(stringr::str_split(periodName, '_'))
  if (length(mm) != 2) {
    stop('cannot continue because periodName does not have two-items (e.g. Jun_17): ', period)
  }

  period <- mm[1]
  yearOffset <- as.numeric(mm[2])

  # get the FromDate and ToDate
  dfDateRange <- PeriodToDateRange[ PeriodToDateRange[['Period']] == period, ]
  if (nrow(dfDateRange) != 1) {
    stop("cannot understand ", period, " from the period name: ", periodName)
  }

  lubridate::year(dfDateRange[['FromDate']]) <-  yearOffset + lubridate::year(dfDateRange[['FromDate']])
  lubridate::year(dfDateRange[['ToDate']]) <-  yearOffset + lubridate::year(dfDateRange[['ToDate']])
  dfDateRange[['PeriodName']] <- paste(period, yearOffset, sep = "_")

  # calculate the planning year
  dfDateRange[['PlanningYear']] <- 2000 + yearOffset
  ind <- periodName == 'Spring'
  dfDateRange[['PlanningYear']][ind] <- 2000 + yearOffset - 1

  dfDateRange
}


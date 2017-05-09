LocalDriveDaPrice <- "//fs2/world/Analytics/Apps/Qlikview/FTR/SPP_DATA/Markets/DA/LMP_By_SETTLEMENT_LOC"
LocalDriveRefPrice <- "//fs2/world/SMD/Staff/SPP_Auction_Revenue_Rights/PY_2017_2018_ARR_Registration/TCR_REFERENCE_PRICES"

TestListPaths <- list(list(Source = 'NPPD_NPPD', Sink = 'SPRM_SPRM'),
                 list(Source = 'SPRM_SPRM', Sink = 'NPPD_NPPD'),
                 list(Source = 'AECI', Sink = 'SPA'),
                 list(Source = 'SPA', Sink = 'AECI'),
                 list(Source = 'GRDA.GREC3', Sink = 'WR.NSW'),
                 list(Source = 'WR.NSW', Sink = 'GRDA.GREC3'),
                 list(Source = 'GRDA.GREC3', Sink = 'NPPD_NPPD'),
                 list(Source = 'NPPD_NPPD', Sink = 'GRDA.GREC3')
)


testthat::test_that('getting day-ahead price using FTP on a specific day works', {
  dateRange <- lubridate::as_date(c('2016-06-01', '2016-06-03'))
  dfData <- getDfDaPriceSpp( dateRange = dateRange )
  ind <- dfData[['Settlement Location']] == 'NPPD_NPPD'
  testthat::expect_equal(nrow(dfData[ind, ]), 2*24)


  # this should not download again, and just use the cache
  dateRange <- lubridate::as_date(c('2016-06-02', '2016-06-03'))
  testthat::expect_silent(dfData <- getDfDaPriceSpp( dateRange = dateRange ))
  ind <- dfData[['Settlement Location']] == 'NPPD_NPPD'
  testthat::expect_equal(nrow(dfData[ind, ]), 1*24)

})


testthat::test_that('getting day-ahead price using local drive on a specific day works', {
  dateRange <- lubridate::as_date(c('2016-07-01', '2016-07-03'))
  dfData <- getDfDaPriceSpp( dateRange = dateRange, ftpRoot = LocalDriveDaPrice )
  ind <- dfData[['Settlement Location']] == 'NPPD_NPPD'
  testthat::expect_equal(nrow(dfData[ind, ]), 2*24)

  # this should not download again, and just use the cache
  dateRange <- lubridate::as_date(c('2016-07-02', '2016-07-03'))
  testthat::expect_silent(dfData <- getDfDaPriceSpp( dateRange = dateRange, ftpRoot = LocalDriveDaPrice ))
  ind <- dfData[['Settlement Location']] == 'NPPD_NPPD'
  testthat::expect_equal(nrow(dfData[ind, ]), 1*24)
})

testthat::test_that('getting the lmp distribution works', {
  dateRange <- lubridate::as_date(c('2016-07-01', '2016-07-03'))
  dfCongest <- getDfSppDaCongestOnPaths(lstPaths = TestListPaths, dateRange, ftpRoot = LocalDriveDaPrice)
  dfCongest <- getDfDaCongestDistribution(lstPaths = TestListPaths, dateRange, ftpRoot = LocalDriveDaPrice, periodName = 'Jun_17', onOrOff = 'OFF', yearOffset = 1)

})

testthat::test_that('calculation of reference price works', {
  dfCalc <- calcRefPriceSpp(lstPaths = TestListPaths, periodName = 'Jun_17', onOrOff = 'OFF', ftpRoot = LocalDriveDaPrice)

})


testthat::test_that('reference price download works', {
  dfRefPrice <- getDfRefPriceByPeriod('Jun_17', onOrOff = 'OFF', ftpRoot = LocalDriveRefPrice, lstPaths = TestListPaths)
})

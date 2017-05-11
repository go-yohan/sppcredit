library(ggplot2)

# pick the what seems to be a long path
lstPathsInterest <- list(
  list(Source = "OKGECENTWIND", Sink = "OKGESNRWIND"),
  list(Source = "OKGESNRWIND", Sink = "OKGECENTWIND")
  # list(Source = "OKGEKEENANWIND", Sink = "CSWSLEEPINGBEAR"),
  # list(Source = "SECI.FPLP.GRAYWIND", Sink = "WAUE.BEPM.BRADY1"),
  # list(Source = "OKGESNRWIND", Sink = "WR.MW.GMEC.MW"),
  # list(Source = "INDN_MWE_SMKY2", Sink = "CSWMAJESTICWIND")
)

dateRange <- lubridate::as_date(c('2016-06-01', '2016-07-01'))
onOrOff <- "OFF"
dfCongest <- getDfSppDaCongestOnPaths(lstPaths = lstPathsInterest, dateRange = dateRange, ftpRoot = LocalDriveDaPrice, toOverrideNaWithMarketWideAverageMcc = FALSE)
dfCongest <- dfCongest %>% left_join(getRtoCalendar("SPP", fromDate = as.character(dateRange[1]), toDate = as.character(dateRange[2]),
                                                    props = c("GMTIntervalEnd", "TIMEOFUSE")) %>%
                                       select(GMTIntervalEnd, Class = TIMEOFUSE))

ind <- dfCongest[['Class']] == onOrOff
dfCongest <- dfCongest[ind, ]


# gather a sum of 400 hours
theSource <- "OKGECENTWIND"
theSink <- "OKGESNRWIND"
theSource <- "OKGESNRWIND"
theSink <- "OKGECENTWIND"
N <- 1000
NumHours <- 400
samplesCongestHourly <- unlist(dfCongest %>% filter(Source == theSource, Sink == theSink) %>% select(CONGEST))

vecQuantiles <- c(0, 0.05, 0.10, 0.25, 0.5)

# plot the hourly samples (most likely skewed)
hist(samplesCongestHourly)
c(quantile(samplesCongestHourly, vecQuantiles), Mean = mean(samplesCongestHourly))

samplesCongestMonthly <- purrr::map_dbl(1:N, function(ind) {
  vecQuantiles <- runif(NumHours)
  sum( quantile(samplesCongestHourly, probs = vecQuantiles, names = FALSE) )
})

hist(samplesCongestMonthly)
quantile(samplesCongestMonthly)
mean(samplesCongestMonthly)

c(quantile(samplesCongestMonthly, vecQuantiles), Mean = mean(samplesCongestMonthly))


# comparing the mean
mean(samplesCongestMonthly) / (NumHours * mean(samplesCongestHourly))

# comparing the standard deviation of the Monthly samples to the sqrt(hours) * standard deviation of the Hourly samples
sd(samplesCongestMonthly) / (sqrt(NumHours) * sd(samplesCongestHourly))

# comparing the actual calculation of Monthly congestion using the normal distribution formula to the 5%-tile using quantile function on the samples
(mean(samplesCongestMonthly) - 1.96 * sd(samplesCongestMonthly)) / (quantile(samplesCongestMonthly, 0.05))

(mean(samplesCongestMonthly) - 1.96 * sd(samplesCongestMonthly)) / (NumHours * mean(samplesCongestHourly) - sqrt(NumHours) * 1.96 * sd(samplesCongestHourly))


# comparing the theoretical 5%-tile of the Monthly to the calculation that reflects the hourly margin
marginHourly <- mean(samplesCongestHourly) - 1.96 * sd(samplesCongestHourly)
(mean(samplesCongestMonthly) - 1.96 * sd(samplesCongestMonthly)) / ((NumHours - sqrt(NumHours)) * mean(samplesCongestHourly) + sqrt(NumHours) * marginHourly)

# what about an approximation
(mean(samplesCongestMonthly) - 1.96 * sd(samplesCongestMonthly)) / ((NumHours) * mean(samplesCongestHourly) + sqrt(NumHours) * marginHourly)

# comparing simulated samples to the formula to estimate the 5%-tile of the monthly product
quantile(samplesCongestMonthly, 0.05) / (NumHours * mean(samplesCongestHourly) - sqrt(NumHours) * (mean(samplesCongestHourly - quantile(samplesCongestHourly, 0.05))))

mean(samplesCongestMonthly) - quantile(samplesCongestMonthly, 0.05)

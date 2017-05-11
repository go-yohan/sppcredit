dateRange <- lubridate::as_date(c('2017-01-01', '2017-05-01'))
dfPrice <- getDfDaPriceSpp(dateRange = dateRange, ftpRoot = LocalDriveDaPrice)

dfLowestMccByNode <- dfPrice %>% select(`Settlement Location`, MCC) %>% group_by(`Settlement Location`) %>% summarise(MCC = quantile(MCC, 0.05, na.rm = TRUE))
nrow(dfLowestMccByNode)
plot(dfLowestMccByNode[['MCC']] [order(dfLowestMccByNode[['MCC']])])
dfLowestMccByNode[order(dfLowestMccByNode[['MCC']]),] %>% filter(stringr::str_detect(`Settlement Location`, '_'))

# get the load zones
ind <- stringr::str_match(dfLowestMccByNode[['Settlement Location']], "^([^_]+)_([^_]+)$")
indLoad <- (!is.na(ind[,2]) & !is.na(ind[,3]) & ind[,2] == ind[,3])
vecLoadNodes <- dfLowestMccByNode[['Settlement Location']] [indLoad]

dfMeanMccByNode <- dfPrice %>% filter(`Settlement Location` %in% vecLoadNodes) %>%
  select(`Settlement Location`, MCC) %>% group_by(`Settlement Location`) %>% summarise(MCC = mean(MCC, na.rm = TRUE))
dfMeanMccByNode[order(dfMeanMccByNode[['MCC']]), ]

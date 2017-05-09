FTPROOT_REFPRICE <- "ftp://pubftp.spp.org/TCR/ReferencePrices"

FTPROOT_DAPRICE <- "ftp://pubftp.spp.org/Markets/DA/LMP_By_SETTLEMENT_LOC"


getDfPathsFromPathList <- function(lstPaths) {
  dfPaths <- tibble::tibble(Source = purrr::map_chr(lstPaths, function(entry) entry[['Source']]),
                        Sink = purrr::map_chr(lstPaths, function(entry) entry[['Sink']]))

}

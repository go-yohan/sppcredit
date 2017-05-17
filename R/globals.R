FTPROOT_REFPRICE <- "ftp://pubftp.spp.org/TCR/ReferencePrices"

FTPROOT_DAPRICE <- "ftp://pubftp.spp.org/Markets/DA/LMP_By_SETTLEMENT_LOC"

TEAColors <- c(
  "#0BC1AC",
  "#919191",
  "#87E8E7",
  "#D0272A",
  "#5FCE5B",
  "#535210",
  "#F55165",
  "#277DCE",
  "#531033",
  "#101153",
  "#FFAD5C",
  "#533110",
  "#0BC1AD",
  "#FF3D3D",
  "#D86F4B",
  "#0B7BC1",
  "#7BC10B",
  "#535210"
)

getDfPathsFromPathList <- function(lstPaths) {
  dfPaths <- tibble::tibble(Source = purrr::map_chr(lstPaths, function(entry) entry[['Source']]),
                        Sink = purrr::map_chr(lstPaths, function(entry) entry[['Sink']]))

}

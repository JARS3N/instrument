inst_cal <- function(fl = choose.files()){
require(dplyr)
require(XML)
A <- XML::xmlTreeParse(fl, useInternalNodes = T)
types <- c(
  int = file.path("ArrayOfInt", "int"),
  double = file.path("ArrayOfDouble", "double"),
  quality = file.path("ArrayOfCalibrationQuality", "CalibrationQuality")
)
analytes <- c("//PHData", "//O2Data")
vars <- c("status", "LED", "gain", "emission", "refdelta")
pos <-
  c(
    "LedStatusValues",
    "LedValues",
    "GainValues",
    "CalibrationEmissionValues",
    "IntialReferenceDeltaValues"
  )

paths <- Map(function(x, y) {
  file.path(analytes, x, y)
},
x = pos,
y = types[c('quality', 'int', 'int', 'double', 'int')])

df <-
  function(u) {
    as.data.frame(setNames(lapply(
      lapply(u, xpathApply, doc = A, fun = xmlValue), unlist
    ), c("pH", "O2")))
  }


OUT <- dplyr::bind_cols(Map(function(x, y) {
  setNames(x, paste0(c("pH", "O2"), "_", y))
},
x = lapply(paths, df),
y = vars))
OUT$Well <- plates::wells(length(OUT[, 1]))$Wells
OUT$sn <- as.numeric(xpathSApply(A, "//CartridgeSerial", xmlValue))
OUT$Lot <-xpathSApply(A, "//CartridgeLot", xmlValue)
if(OUT$Lot[1]==""){OUT$Lot<-NA}
OUT$file<-basename(fl)
OUT}

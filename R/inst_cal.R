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
OUT$Well <- plates::well(length(OUT[, 1]))
check_sn<-xpathSApply(A, "//CartridgeSerial",
                        xmlValue)
check_lot<-xpathSApply(A, "//CartridgeLot", xmlValue)
OUT$sn <- if(length(check_sn)>0){as.numeric(check_sn)}else{NA}
OUT$Lot<-if (length(check_lot)>0) {check_lot}else{NA}
OUT$O2_target<-xpathSApply(A, "//O2Data/TargetEmissionValue", xmlValue)
OUT$pH_target <- xpathSApply(A, "//PHData/TargetEmissionValue", xmlValue)
OUT$when <- xpathSApply(A, "//WhenCalibrated", xmlValue)[1]
OUT$file<-basename(fl)
OUT}

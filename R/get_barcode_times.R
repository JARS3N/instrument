get_barcode_times<-function(u){
  fls<- list.files(path=u,pattern='.log')
  dplyr::bind_rows(
    lapply(
      split( fls, fls),
      read_barcode_time)
  )
}

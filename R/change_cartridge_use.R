change_cartridge_use<-function(filename,n,outName='Outputhere.xml'){
  require(dplyr)
  require(xml2)
  require(base64enc)
  #read input file
  sm<-file(filename)  %>%
    base64decode() %>%
    readBin(.,"character") %>%
    xml2::read_xml( )
  #set new number of uses
  b<-n %>%
    paste0("<o><NumberOfTimesUsed>",.,"</NumberOfTimesUsed></o>") %>%
    read_xml() %>%
    xml_node('NumberOfTimesUsed')
  a<-xml_nodes(sm,'NumberOfTimesUsed')
  xml_replace(a,b)
  #xml2::write_xml(sm,'intermediate.xml')
  out<-paste0(sm,collapse="\n") %>%
    charToRaw(.) %>%
    base64encode(.)
  #writeLines(out,file.path(dir,outName))
  dirname(Sys.getenv('HOME')) %>%
    file.path(.,'Desktop',outName) %>%
    writeLines(out,.)
}

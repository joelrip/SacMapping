##Download and display local Shapefiles
GISDown <- function(location, filename) {
  require(sp)
  require(rgdal)
  ##require(rgeos)
  ##library(RColorBrewer)
  temp <- tempfile()
  if(location == "County") {
    url <- paste0("http://www.sacgis.org/GISDataPub/Data/",filename,".zip")
  }
  if(location == "SACOG") {
    url <- paste0("http://www.sacog.org/mapping/clearinghouse/data/",filename,".zip")
  }
  if(location == "City") {
    url <- paste0("http://www.cityofsacramento.org/gis/zipdata/",filename,".zip")
  }
  download.file(url,temp)
  unzip(temp, exdir = paste0("/Users/joelrip/Documents/R_Files/Mapping/Sacramento/",location,"Data"))
  unlink(temp)
  tempmap <- readOGR(paste0("/Users/joelrip/Documents/R_Files/Mapping/Sacramento/",location,"Data"), filename, stringsAsFactors=F)
  plot(tempmap)
}

library(aqp)
library(soilDB)
library(sp)
library(rgdal)
library(raster)
library(rgeos)

usa <- map_data("usa")

coordinates(usa) <- ~ long + lat
proj4string(usa) <- '+proj=longlat +datum=WGS84'

pnts <- SDA_spatialQuery(usa, what = 'mupolygon', db= "SSURGO")

# get KSSL pedons with taxonname = Auburn
# coordinates will be WGS84 GCS
auburn <- fetchKSSL('auburn')

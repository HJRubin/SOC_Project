## ----setup, include=FALSE------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warnings = FALSE, purl = FALSE)
options(warn=-1)


## ----load.package--------------------------------------------------------------------------------
# library(devtools)
# devtools::install_github("JoshOBrien/gdalUtilities")
library(gdalUtilities)      # wrappers for GDAL utility programs that could be
                        #  called from the command line, but here via `sf`
# devtools::install_github("gearslaboratory/gdalUtils")
library(gdalUtils)      # wrappers for GDAL utility programs that could be
                        #  called from the command line,
library(sf)             # spatial data types -- Simple Features
library(stars)          # Spatiotemporal Arrays, Raster and Vector Data Cubes
library(dplyr)          # tidyverse data manipulation functions
library(ggplot2)        # gpplot graphics
library(maps)           # optional -- for boundary polygons
library(mapdata)


## ------------------------------------------------------------------------------------------------
drivers <- sf::st_drivers()
# print(drivers)
ix <- grep("GPKG", drivers$name,  fixed=TRUE)
drivers[ix,]
ix <- grep("ESRI", drivers$name,  fixed=TRUE)
drivers[ix,]
ix <- grep("CSV", drivers$name,  fixed=TRUE)
drivers[ix,]


## ----specify.wfs.address-------------------------------------------------------------------------
wfs <- "WFS:https://maps.isric.org/mapserv?map=/map/wosis_latest.map"


## ----ogrinfo.wfs---------------------------------------------------------------------------------
(layers.info <- st_layers(wfs))


## ----ogrinfo.site--------------------------------------------------------------------------------
profiles.info <-
  gdalUtils::ogrinfo(wfs, layer = "ms:wosis_latest_profiles",
                     ro = TRUE, so = TRUE, q = TRUE)
cat(profiles.info, sep="\n")

## ----setup.local.dir-----------------------------------------------------------------------------
wosis.dir.name <- "./wosis_latest"
if (!file.exists(wosis.dir.name)) dir.create(wosis.dir.name)


## ----download.profile.gpkg-----------------------------------------------------------------------
layer.name <- "wosis_latest_profiles"
(dst.target.name <- paste0(wosis.dir.name,"/", layer.name, ".gpkg"))
if (!file.exists(dst.target.name)) { 
system.time(
  gdalUtilities::ogr2ogr(src=wfs,
          dst=dst.target.name,
          layer=layer.name,
          f = "GPKG",
          overwrite=TRUE,
          skipfailures=TRUE)
)
}
file.info(dst.target.name)$size/1024/1024


## ----read.profile.gpkg---------------------------------------------------------------------------
profiles.gpkg <- st_read(dst.target.name)
class(profiles.gpkg)
dim(profiles.gpkg)
names(profiles.gpkg)


## ------------------------------------------------------------------------------------------------
head(sort(table(profiles.gpkg$country_name), decreasing=TRUE))


## ----map.profiles.gpkg, fig.width=12, fig.height=12----------------------------------------------
ggplot(data=profiles.gpkg[(profiles.gpkg$country_name=="United States of America"), ]) +
  aes(col=dataset_id) +
  geom_sf(shape=21, size=0.8, fill="black")

data_us=profiles.gpkg[(profiles.gpkg$country_name=="United States of America"), ]
data_us = data_us[data_us$orgc == "true",]


########################################################

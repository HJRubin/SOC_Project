library(terra)
#check POLARIS,GSOCmap,. SOC_Mexico_CONUS(1991–2010),Ramcharan----
setwd("/Users/zhuonanwang/SOC_MajorRevision_data")
list.files()

polaris <- rast("polaris_socstock_30cm.tif") #kg/m2
plot(polaris)

#250m resolution
SOC_Mexico_CONUS <- rast("SOC_prediction_1991_2010.tif") #kg/m2
plot(SOC_Mexico_CONUS)
crs=crs(SOC_Mexico_CONUS, proj=TRUE)

conus_state <-  st_read("/Users/zhuonanwang/SOC_MajorRevision_data/INPUT/us_shp/conus_state.shp")
crs(conus_state, proj=TRUE)
plot(conus_state)
conus_state_terra <- vect(conus_state)
crs(conus_state_terra, proj=TRUE)
conus_state_proj <- project(conus_state_terra,crs)
plot(conus_state_proj)
SOC_CONUS<- mask(SOC_Mexico_CONUS,conus_state_proj)
plot(SOC_CONUS)

writeRaster(SOC_CONUS, "SOC_prediction_1991_2010_US.tif", overwrite=TRUE)


#GSOCmap1.5.0------
GSOCmap_us=rast("GSOCmap_us_0_30cm_1km.tif")#kg/m2
plot(GSOCmap_us)

calculate=FALSE
if(calculate=TRUE){
GSOCmap <- rast("GSOCmap1.5.0.tif")#SOC stock [t/ha] 0-30 cm depth
#1 t/ha=0.1kg/m2
plot(GSOCmap)
ourformat <- rast("/Users/zhuonanwang/SOC_MajorRevision_data/modis_npp_30arc.tif")
crs_target = "+proj=longlat +datum=WGS84 +no_defs"
us_extent <- ext(-180, -55, 15, 80)
# us_boundary <- vect("/Users/zwang61/Data_soc/INPUT/us_shp/us.shp")
# # us_boundary
# crs(us_boundary, describe=TRUE, proj=TRUE)
# crs(us_boundary) <- crs_target #us boundary

GSOCmap_30arc_us <- mask(crop(GSOCmap,us_extent),ourformat)
#plot(GSOCmap_30arc_us)
origin(GSOCmap_30arc_us)==origin(ourformat)
GSOCmap_us <- resample(GSOCmap_30arc_us, ourformat, method="bilinear")
origin(GSOCmap_us)==origin(ourformat)
plot(GSOCmap_us)
GSOCmap_us=GSOCmap_us*0.1
writeRaster(GSOCmap_us, "GSOCmap_us_0_30cm_1km.tif", overwrite=TRUE)
}


#Ramcharan 100m resolution----
Ramcharan_include=FALSE
if(Ramcharan_include==TRUE){
#0, 5, 15, 30, 60, 100, 200 cm
#soil organic C in % weight, 
#bulk density  in g cm-3
#convert g/cm3 to kg/m2 (multiply layer depth,then multiply 10)
  
# bd_Ramcharan_0_5cm <-  rast("bd_M_sl1_100m.tif")
# minmax(bd_Ramcharan_0_5cm)
# max(bd_Ramcharan_0_5cm)
# global(bd_Ramcharan_0_5cm, "max", na.rm=TRUE)

bd_Ramcharan_0_5cm <-  rast("bd_M_sl1_100m.tif")*50/1000 #kg/m2 
bd_Ramcharan_5_15cm <-  rast("bd_M_sl2_100m.tif")*100/1000 #kg/m2 
bd_Ramcharan_15_30cm <-  rast("bd_M_sl3_100m.tif")*150 /1000#kg/m2 
bd_Ramcharan_30_60cm <-  rast("bd_M_sl4_100m.tif")*300/1000 #kg/m2 
bd_Ramcharan_60_100cm <-  rast("bd_M_sl5_100m.tif")*400/1000 #kg/m2 
plot(bd_Ramcharan_60_100cm)
#SOC = SOC (%)× BD × SD 
# soc_Ramcharan_0_5cm <-  rast("soc_M_sl1_100m.tif")/1000 #unit is null,50%-->0.5
# plot(soc_Ramcharan_0_5cm)
# minmax(soc_Ramcharan_0_5cm)
soc_Ramcharan_0_5cm <-  rast("soc_M_sl1_100m.tif")/1000
soc_Ramcharan_5_15cm <-  rast("soc_M_sl2_100m.tif")/1000
soc_Ramcharan_15_30cm <-  rast("soc_M_sl3_100m.tif")/1000
soc_Ramcharan_30_60cm <-  rast("soc_M_sl4_100m.tif")/1000
soc_Ramcharan_60_100cm <-  rast("soc_M_sl5_100m.tif")/1000

plot(soc_Ramcharan_60_100cm)

#kg/m2
soc_Ramcharan_0_30cm <- bd_Ramcharan_0_5cm*soc_Ramcharan_0_5cm+
                        bd_Ramcharan_5_15cm*soc_Ramcharan_5_15cm+
                        bd_Ramcharan_15_30cm*soc_Ramcharan_15_30cm

soc_Ramcharan_0_100cm <- soc_Ramcharan_0_30cm+
  bd_Ramcharan_30_60cm *soc_Ramcharan_30_60cm+
  bd_Ramcharan_60_100cm*soc_Ramcharan_60_100cm

writeRaster(soc_Ramcharan_0_30cm, "soc_Ramcharan_0_30cm.tif", overwrite=TRUE)
writeRaster(soc_Ramcharan_0_100cm, "soc_Ramcharan_0_100cm.tif", overwrite=TRUE)

soc_Ramcharan_0_30cm_1km <- aggregate(soc_Ramcharan_0_30cm, fact = 10, fun = mean, na.rm=TRUE)
soc_Ramcharan_0_100cm_1km <- aggregate(soc_Ramcharan_0_100cm, fact = 10, fun = mean, na.rm=TRUE)

writeRaster(soc_Ramcharan_0_30cm_1km, "soc_Ramcharan_0_30cm_1km.tif", overwrite=TRUE)
writeRaster(soc_Ramcharan_0_100cm_1km, "soc_Ramcharan_0_100cm_1km.tif", overwrite=TRUE)
}


soc_Ramcharan_0_30cm_1km <- rast("soc_Ramcharan_0_30cm_1km.tif")
soc_Ramcharan_0_100cm_1km <- rast("soc_Ramcharan_0_100cm_1km.tif")
plot(soc_Ramcharan_0_30cm_1km)
plot(soc_Ramcharan_0_100cm_1km)

#Ramcharan divide by 100-------
bd_Ramcharan_0_5cm <-  rast("bd_M_sl1_100m.tif")*50/100#kg/m2 
bd_Ramcharan_5_15cm <-  rast("bd_M_sl2_100m.tif")*100/100 #kg/m2 
bd_Ramcharan_15_30cm <-  rast("bd_M_sl3_100m.tif")*150 /100#kg/m2 
bd_Ramcharan_30_60cm <-  rast("bd_M_sl4_100m.tif")*300/100 #kg/m2 
bd_Ramcharan_60_100cm <-  rast("bd_M_sl5_100m.tif")*400/100 #kg/m2 
plot(bd_Ramcharan_60_100cm)
#SOC = SOC (%)× BD × SD 
# soc_Ramcharan_0_5cm <-  rast("soc_M_sl1_100m.tif")/1000 #unit is null,50%-->0.5
# plot(soc_Ramcharan_0_5cm)
# minmax(soc_Ramcharan_0_5cm)
soc_Ramcharan_0_5cm <-  rast("soc_M_sl1_100m.tif")/100
soc_Ramcharan_5_15cm <-  rast("soc_M_sl2_100m.tif")/100
soc_Ramcharan_15_30cm <-  rast("soc_M_sl3_100m.tif")/100
soc_Ramcharan_30_60cm <-  rast("soc_M_sl4_100m.tif")/100
soc_Ramcharan_60_100cm <-  rast("soc_M_sl5_100m.tif")/100

plot(soc_Ramcharan_60_100cm)

#kg/m2
soc_Ramcharan_0_30cm <- bd_Ramcharan_0_5cm*soc_Ramcharan_0_5cm+
  bd_Ramcharan_5_15cm*soc_Ramcharan_5_15cm+
  bd_Ramcharan_15_30cm*soc_Ramcharan_15_30cm

soc_Ramcharan_0_100cm <- soc_Ramcharan_0_30cm+
  bd_Ramcharan_30_60cm *soc_Ramcharan_30_60cm+
  bd_Ramcharan_60_100cm*soc_Ramcharan_60_100cm

writeRaster(soc_Ramcharan_0_30cm, "soc_Ramcharan_0_30cm.tif", overwrite=TRUE)
writeRaster(soc_Ramcharan_0_100cm, "soc_Ramcharan_0_100cm.tif", overwrite=TRUE)

soc_Ramcharan_0_30cm_1km <- aggregate(soc_Ramcharan_0_30cm, fact = 10, fun = mean, na.rm=TRUE)
soc_Ramcharan_0_100cm_1km <- aggregate(soc_Ramcharan_0_100cm, fact = 10, fun = mean, na.rm=TRUE)

writeRaster(soc_Ramcharan_0_30cm_1km, "soc_Ramcharan_0_30cm_1km.tif", overwrite=TRUE)
writeRaster(soc_Ramcharan_0_100cm_1km, "soc_Ramcharan_0_100cm_1km.tif", overwrite=TRUE)
}


soc_Ramcharan_0_30cm_1km <- rast("soc_Ramcharan_0_30cm_1km.tif")
soc_Ramcharan_0_100cm_1km <- rast("soc_Ramcharan_0_100cm_1km.tif")
plot(soc_Ramcharan_0_30cm_1km)
plot(soc_Ramcharan_0_100cm_1km)
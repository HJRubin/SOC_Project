library(terra)
#check POLARIS,GSOCmap,. SOC_Mexico_CONUS(1991–2010),Ramcharan----
setwd("/Users/zhuonanwang/SOC_MajorRevision_data")
list.files()

#POLARIS resolution
# 30-m spatial resolution
polaris30cm <- rast("polaris_socstock_30cm.tif") #kg/m2
plot(polaris30cm)
area <- cellSize(polaris30cm) #unit is m2
polaris30cm_US_stock <- global(area*polaris30cm*1e-12,"sum",na.rm=TRUE) #Pg 59.15127

polaris100cm <- rast("polaris_socstock_100cm.tif") #kg/m2
plot(polaris100cm)
area <- cellSize(polaris100cm) #unit is m2
polaris100cm_US_stock <- global(area*polaris100cm*1e-12,"sum",na.rm=TRUE) #Pg 104.9718



#250m resolution
SOC_CONUS <- rast("SOC_prediction_1991_2010_US.tif") #kg/m2
plot(SOC_CONUS)

SOC_kg <- SOC_CONUS*250*250
SOC_Pg <- global(SOC_kg*1e-12,"sum",na.rm=TRUE) #Pg 28.88397



#GSOCmap1.5.0------
GSOCmap_us=rast("GSOCmap_us_0_30cm_1km.tif")#kg/m2
plot(GSOCmap_us)

area <- cellSize(GSOCmap_us) #unit is m2
GSOCmap_US_stock <- global(area*GSOCmap_us*1e-12,"sum",na.rm=TRUE) #Pg 52.83141

#Ramcharan 100m resolution----
#data has problem. I have sent email to authors and coauthors.
soc_Ramcharan_0_30cm <- rast("soc_Ramcharan_0_30cm.tif") #kg/m2
plot(soc_Ramcharan_0_30cm)
soc_Ramcharan_0_30cmSOC_kg <- soc_Ramcharan_0_30cm*100*100
soc_Ramcharan_0_30cmSOC_Pg <- global(soc_Ramcharan_0_30cmSOC_kg*1e-12,"sum",na.rm=TRUE) #Pg 106.2147
soc_Ramcharan_0_30cmSOC_Pg

soc_Ramcharan_0_100cm <- rast("soc_Ramcharan_0_100cm.tif") #kg/m2
plot(soc_Ramcharan_0_100cm)
soc_Ramcharan_0_100cmSOC_kg <- soc_Ramcharan_0_100cm*100*100
soc_Ramcharan_0_100cmSOC_Pg <- global(soc_Ramcharan_0_100cmSOC_kg*1e-12,"sum",na.rm=TRUE) #Pg 186.6603
soc_Ramcharan_0_100cmSOC_Pg


soc_Ramcharan_0_30cm_1km <- rast( "soc_Ramcharan_0_30cm_1km.tif")
soc_Ramcharan_0_100cm_1km <- rast( "soc_Ramcharan_0_100cm_1km.tif")


library(terra)
library(sf)
library(sp)
library(tidyterra)
library(rgdal)
library(randomForest)
# library(caret)
# library(stats)

setwd("~/ArcGIS/China_Dep/US_Boundary")

inityear = 1984
year0 = 2022
us_shp = vect('cb_2018_us_nation_5m.shp')
# ca = vect('~/R/SOC/ca-state-boundary/CA_State_TIGER2016.shp')

setwd("~/Dissertation Work/My Work/FAA/Data/Daymet_tmax")
tmax_y0 = rast("daymet_v4_tmax_annavg_na_2022.tif")
tmax_y1 = rast("daymet_v4_tmax_annavg_na_2021.tif")
tmax_y2 = rast("daymet_v4_tmax_annavg_na_2020.tif")
tmax_y3 = rast("daymet_v4_tmax_annavg_na_2019.tif")
tmax_y4 = rast("daymet_v4_tmax_annavg_na_2018.tif")
tmax_y5 = rast("daymet_v4_tmax_annavg_na_2017.tif")
setwd("~/Dissertation Work/My Work/FAA/Data/Daymet_tmin")
tmin_y0 = rast("daymet_v4_tmin_annavg_na_2022.tif")
tmin_y1 = rast("daymet_v4_tmin_annavg_na_2021.tif")
tmin_y2 = rast("daymet_v4_tmin_annavg_na_2020.tif")
tmin_y3 = rast("daymet_v4_tmin_annavg_na_2019.tif")
tmin_y4 = rast("daymet_v4_tmin_annavg_na_2018.tif")
tmin_y5 = rast("daymet_v4_tmin_annavg_na_2017.tif")
setwd("~/Dissertation Work/My Work/FAA/Data/Daymet_Precip")
prcp_y0 = rast("daymet_v4_prcp_annttl_na_2022.tif")
prcp_y1 = rast("daymet_v4_prcp_annttl_na_2021.tif")
prcp_y2 = rast("daymet_v4_prcp_annttl_na_2020.tif")
prcp_y3 = rast("daymet_v4_prcp_annttl_na_2019.tif")
prcp_y4 = rast("daymet_v4_prcp_annttl_na_2018.tif")
prcp_y5 = rast("daymet_v4_prcp_annttl_na_2017.tif")

setwd("~/Dissertation Work/My Work/FAA/Data/Landuse")
lulc_y0 = rast("CONUS_2022.tif")
lulc_y1 = rast("CONUS_2021.tif")
lulc_y2 = rast("CONUS_2020.tif")
lulc_y3 = rast("CONUS_2019.tif")
lulc_y4 = rast("CONUS_2018.tif")
lulc_y5 = rast("CONUS_2017.tif")

setwd("~/Dissertation Work/My Work/FAA/Data/Landsat_full_yrs_NDVI")
ndvi_mean = rast("wgs_landsat8_2022_full.tif")
ndvi_mean_y5 = rast("wgs_landsat8_2017_full.tif")
ndvi_mean_y4 = rast("wgs_landsat8_2018_full.tif")
ndvi_mean_y3 = rast("wgs_landsat8_2019_full.tif")
ndvi_mean_y2 = rast("wgs_landsat8_2020_full.tif")
ndvi_mean_y1 = rast("wgs_landsat8_2021_full.tif")

setwd("~/Dissertation Work/My Work/FAA/Data/Landsat_tmax_tmin_NDVI")
ndvi_min = rast("Landsat7_ndvi_5_2022.tif")
ndvi_max = rast("Landsat7_ndvi_95_2022.tif")
ndvi_min_y1 = rast("Landsat7_ndvi_5_2021.tif")
ndvi_max_y1 = rast("Landsat7_ndvi_95_2021.tif")
ndvi_min_y2 = rast("Landsat7_ndvi_5_2020.tif")
ndvi_max_y2 = rast("Landsat7_ndvi_95_2020.tif")
ndvi_min_y3 = rast("Landsat7_ndvi_5_2019.tif")
ndvi_max_y3 = rast("Landsat7_ndvi_95_2019.tif")
ndvi_min_y4 = rast("Landsat7_ndvi_5_2018.tif")
ndvi_max_y4 = rast("Landsat7_ndvi_95_2018.tif")
ndvi_min_y5 = rast("Landsat7_ndvi_5_2017.tif")
ndvi_max_y5 = rast("Landsat7_ndvi_95_2017.tif")

setwd("~/ArcGIS/Projects/FAA_SOC/")
elevation = rast("Agg_elev.tif")
slope = rast("slope.tif")
aspect = rast("Agg_Aspect.tif")

geol = rast("SGMC_Geology_general.tif")

setwd("~/Dissertation Work/My Work/FAA/Data/NACP_MsTMIP_Unified_NA_SoilMap_1242/data")
clay = rast("Unified_NA_Soil_Map_Subsoil_Clay_Fraction.tif")

# soilmoist1984 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-1984-year-fv08.1.nc_annual.nc")
# soilmoist1985 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-1985-year-fv08.1.nc_annual.nc")
# soilmoist1986 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-1986-year-fv08.1.nc_annual.nc")
# soilmoist1987 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-1987-year-fv08.1.nc_annual.nc")
# soilmoist1988 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-1988-year-fv08.1.nc_annual.nc")
# soilmoist1989 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-1989-year-fv08.1.nc_annual.nc")
# soilmoist1990 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-1990-year-fv08.1.nc_annual.nc")
# soilmoist1991 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-1991-year-fv08.1.nc_annual.nc")
# soilmoist1992 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-1992-year-fv08.1.nc_annual.nc")
# soilmoist1993 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-1993-year-fv08.1.nc_annual.nc")
# soilmoist1994 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-1994-year-fv08.1.nc_annual.nc")
# soilmoist1995 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-1995-year-fv08.1.nc_annual.nc")
# soilmoist1996 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-1996-year-fv08.1.nc_annual.nc")
# soilmoist1997 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-1997-year-fv08.1.nc_annual.nc")
# soilmoist1998 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-1998-year-fv08.1.nc_annual.nc")
# soilmoist1999 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-1999-year-fv08.1.nc_annual.nc")
# soilmoist2000 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2000-year-fv08.1.nc_annual.nc")
# soilmoist2001 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2001-year-fv08.1.nc_annual.nc")
# soilmoist2002 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2002-year-fv08.1.nc_annual.nc")
# soilmoist2003 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2003-year-fv08.1.nc_annual.nc")
# soilmoist2004 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2004-year-fv08.1.nc_annual.nc")
# soilmoist2005 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2005-year-fv08.1.nc_annual.nc")
# soilmoist2006 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2006-year-fv08.1.nc_annual.nc")
# soilmoist2007 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2007-year-fv08.1.nc_annual.nc")
# soilmoist2008 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2008-year-fv08.1.nc_annual.nc")
# soilmoist2009 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2009-year-fv08.1.nc_annual.nc")
# soilmoist2010 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2010-year-fv08.1.nc_annual.nc")
# soilmoist2011 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2011-year-fv08.1.nc_annual.nc")
# soilmoist2012 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2012-year-fv08.1.nc_annual.nc")
# soilmoist2013 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2013-year-fv08.1.nc_annual.nc")
# soilmoist2014 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2014-year-fv08.1.nc_annual.nc")
# soilmoist2015 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2015-year-fv08.1.nc_annual.nc")
# soilmoist2016 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2016-year-fv08.1.nc_annual.nc")
# soilmoist2017 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2017-year-fv08.1.nc_annual.nc")
# soilmoist2018 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2018-year-fv08.1.nc_annual.nc")
# soilmoist2019 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2019-year-fv08.1.nc_annual.nc")
# soilmoist2020 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2020-year-fv08.1.nc_annual.nc")
# soilmoist2021 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2021-year-fv08.1.nc_annual.nc")
soilmoist2022 = rast("C:/Users/User/Documents/SOC/soil_moisture/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2022-year-fv08.1.nc_annual.nc")

#######################CLIP########################
crs(us_shp)<-crs("EPSG:4326")
# ca = terra::project(us_shp, "EPSG:4326")

list_ndvi = c(ndvi_mean$NDVI, ndvi_mean_y1$NDVI, ndvi_mean_y2$NDVI, ndvi_mean_y3$NDVI, ndvi_mean_y4$NDVI, 
              ndvi_mean_y5$NDVI)
list_ndvi1 = c(ndvi_min$NDVI_p5, ndvi_min_y1$NDVI_p5, ndvi_min_y2$NDVI_p5, ndvi_min_y3$NDVI_p5, 
               ndvi_min_y4$NDVI_p5, ndvi_min_y5$NDVI_p5,
              ndvi_max$NDVI_p95, ndvi_max_y1$NDVI_p95, ndvi_max_y2$NDVI_p95, ndvi_max_y3$NDVI_p95, 
              ndvi_max_y4$NDVI_p95, ndvi_max_y5$NDVI_p95)
ndvi_us = mask(list_ndvi, us_shp)
list_ndvi_us_1 = mask(resample(list_ndvi1, ndvi_us), us_shp)

list_day = c(tmax_y0, tmax_y1, tmax_y2, tmax_y3, tmax_y4, tmax_y5,
                   tmin_y0, tmin_y1, tmin_y2, tmin_y3, tmin_y4, tmin_y5,
                   prcp_y0, prcp_y1, prcp_y2, prcp_y3, prcp_y4, prcp_y5)
fa = mask(ndvi_mean, us_shp)
tmax_y0_pr = terra::project(list_day, "EPSG:4326")
list_day_us = mask(resample(tmax_y0_pr, ndvi_us), us_shp)

list_lulc = c(lulc_y0, lulc_y1, lulc_y2, lulc_y3, lulc_y4, lulc_y5)
lulc_pr = terra::project(list_lulc, "EPSG:4326")
list_lulc_us = mask(resample(lulc_pr, ndvi_us), us_shp)

list_pr_us = mask(resample(terra::project(geol, "EPSG:4326"), ndvi_us), us_shp)
list_pr_us1 <- catalyze(list_pr_us)
list_pr_us2 = as.numeric(list_pr_us1)

 # writeCDF(list_pr_us2, "geology_catalyze.nc")

list_topo = c(elevation, slope, aspect) 
topo_pr = terra::project(list_topo, "EPSG:4326")
list_topo_us = mask(resample(topo_pr, ndvi_us), us_shp)

clay_pr = terra::project(clay, "EPSG:4326")
clay_us = mask(resample(clay, ndvi_us), us_shp)

soilmoist_us = mask(resample(soilmoist2022, ndvi_us), us_shp)

prec_log_y0 = log10(list_day_us$daymet_v4_prcp_annttl_na_2022)
prec_log_y1 = log10(list_day_us$daymet_v4_prcp_annttl_na_2021)
prec_log_y2 = log10(list_day_us$daymet_v4_prcp_annttl_na_2020)
prec_log_y3 = log10(list_day_us$daymet_v4_prcp_annttl_na_2019)
prec_log_y4 = log10(list_day_us$daymet_v4_prcp_annttl_na_2018)
prec_log_y5 = log10(list_day_us$daymet_v4_prcp_annttl_na_2017)

elev_log_us = log10(list_topo_us[[1]])
slope_log_us = log10(list_topo_us[[2]])

r <- ndvi_us$NDVI
lon <- init(r, "x") |> mask(r)
lat <- init(r, "y") |> mask(r)

Year <- init(r, "y") |> mask(r)
Year$val <- 2023
Year_us = mask(Year, us_shp)

stack_vars = c(list_ndvi_us, list_ndvi_us_1)
stack_vars1 = c(lon, lat, list_lulc_us$CONUS_2021, list_day_us$daymet_v4_tmin_annavg_na_2022, 
                list_day_us$daymet_v4_tmin_annavg_na_2021, list_day_us$daymet_v4_tmin_annavg_na_2020,
                list_day_us$daymet_v4_tmin_annavg_na_2019, list_day_us$daymet_v4_tmin_annavg_na_2018,
                list_day_us$daymet_v4_tmin_annavg_na_2017, list_day_us$daymet_v4_tmax_annavg_na_2022,
                list_day_us$daymet_v4_tmax_annavg_na_2021, list_day_us$daymet_v4_tmax_annavg_na_2020,
                list_day_us$daymet_v4_tmax_annavg_na_2019, list_day_us$daymet_v4_tmax_annavg_na_2018,
                list_day_us$daymet_v4_tmax_annavg_na_2017)
stack_vars2 = c(prec_log_y0, prec_log_y1, prec_log_y2, prec_log_y3, prec_log_y4, prec_log_y5, 
                clay_us, soilmoist_us$sm)
                # slope_log_us, elev_log_us, list_topo_us[[3]], ) #geology

names(stack_vars) = c("SOC1987.NDVI_mean", "SOC1987.NDVI_mean_y1", "SOC1987.NDVI_mean_y2", "SOC1987.NDVI_mean_y3",
                      "SOC1987.NDVI_mean_y4", "SOC1987.NDVI_mean_y5", "SOC1987.NDVI_min", "SOC1987.NDVI_min_y1",
                      "SOC1987.NDVI_min_y2", "SOC1987.NDVI_min_y3", "SOC1987.NDVI_min_y4", "SOC1987.NDVI_min_y5",
                      "SOC1987.NDVI_max","SOC1987.NDVI_max_y1", "SOC1987.NDVI_max_y2", "SOC1987.NDVI_max_y3",
                      "SOC1987.NDVI_max_y4", "SOC1987.NDVI_max_y5")
names(stack_vars1) = c("Longitude", "Latitude", "LULC_y1",
                      "SOC1987.Tmin", "SOC1987.Tmin_yr1", "SOC1987.Tmin_yr2", "SOC1987.Tmin_yr3", 
                      "SOC1987.Tmin_yr4","SOC1987.Tmin_yr5", "SOC1987.Tmax", "SOC1987.Tmax_yr1", 
                      "SOC1987.Tmax_yr2", 
                      "SOC1987.Tmax_yr3", 
                      "SOC1987.Tmax_yr4", "SOC1987.Tmax_yr5")

names(stack_vars2) = c("Precip_log", "Precip_y1_log", "Precip_y2_log", "Precip_y3_log", "Precip_y4_log", 
                       "Precip_y5_log", "Clay", "SoilMoisture1") 
                      # , "Slope_log", "Elev_log", "SOC1987.Aspect", "Year")
stack_vars_new = c(stack_vars, stack_vars1, stack_vars2)

stack_vars_new[is.na(stack_vars_new)] <- 0
nas = is.na(stack_vars_new)
plot(nas)

RFtraining = get(load("~/R/SOC/RF_03252024.RData"))

RF_preds = predict(model=RFtraining, object=stack_vars_new)
# RF_probs = predict(model=RFtraining, object=stack_vars_new, type = "prob")

RF_preds_SOC = exp(RF_preds)

# RF_preds = RF_preds / 10
# RF_preds1 = RF_preds1 / 10

# RF_diff = RF_preds - RF_preds1

# GBM_preds = predict(model=GBTraining1, object=stack_for_pred, na.action = na.omit)
# LM_preds = predict(model=LMtraining, object=stack_for_pred)
# KNN_preds = predict(model=fit.knn, object=stack_for_pred, na.rm = TRUE)
# KNN_preds$lyr2 <- (KNN_preds$lyr1)*(max(training$SOC_gcm2)-min(training$SOC_gcm2))+min(training$SOC_gcm2)

ggplot() +
  geom_spatraster(data = RF_preds_SOC)+
  # scale_fill_gradient(low = "brown", high = "lightblue")
  scale_fill_gradientn(colours = c("darkred", "orange","gold", "green","darkgreen", "darkblue"),
                       breaks = c(0,0.5,1,1.5,2), limits=c(0,2)) + 
  geom_sf(data = us_shp, lwd=1.2, fill = NA)+
  ylim(25,50) +
  xlim(-125,-60)

ggplot() +
  geom_spatraster(data = list_lulc_us$CONUS_2021)+
  # scale_fill_gradient(low = "brown", high = "lightblue") +
  scale_fill_gradientn(colours = c("darkred", "brown1", "orange","gold", "green","darkgreen", "blue",
                                   "darkblue", "purple"), limits=c(0,16))+
  ylim(32.00,43.00) + 
  xlim(-125.00,-112.00)

writeCDF(RF_preds_SOC, "RF_preds_SOC_2022_02192024.nc")

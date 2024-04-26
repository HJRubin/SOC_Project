library(terra)
library(sp)
library(tidyterra)
library(ncdf4)
library(randomForest)
library(ggplot2)
library(randomForest)
setwd("~/R/SOC")
us_shp = vect('~/ArcGIS/China_Dep/US_Boundary/cb_2018_us_nation_5m.shp')

yearinit = 1989
yearfin = 1990


elevation = rast("~/ArcGIS/Projects/FAA_SOC/agg_elev.tif")
slope = rast("~/ArcGIS/Projects/FAA_SOC/slope.tif")
aspect = rast("~/ArcGIS/Projects/FAA_SOC/agg_aspect.tif")

geol = rast("C:/Users/User/Documents/SOC/Input/geology_catalyze.nc")

clay = rast("~/Dissertation Work/My Work/FAA/Data/NACP_MsTMIP_Unified_NA_SoilMap_1242/data/Unified_NA_Soil_Map_Topsoil_Clay_Fraction.tif")

#######################CLIP########################
# crs(us_shp) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(us_shp)<-crs("EPSG:4326")

ndvi_mean = rast(paste0("~/Dissertation Work/My Work/FAA/Data/Landsat_full_yrs_NDVI/wgs_landsat8_2022_full.tif"))
ndvi_us = mask(ndvi_mean, us_shp)

list_topo = c(elevation, slope, aspect)
topo_pr = terra::project(list_topo, "EPSG:4326")
list_topo_us = mask(resample(topo_pr, ndvi_us), us_shp)

clay_pr = terra::project(clay, "EPSG:4326")
clay_us = mask(resample(clay, ndvi_us), us_shp)

elev_log_us = log10(list_topo_us[[1]])
slope_log_us = log10(list_topo_us[[2]])

r <- ndvi_us$NDVI

lon <- init(r, "x") |> mask(r)
lat <- init(r, "y") |> mask(r)

for (year in yearinit:yearfin){
  print(year)
  year1 = year - 1 
  year2 = year - 2
  year3 = year - 3 
  year4 = year - 4 
  year5 = year - 5 
  
  tmax_y0 = rast(paste0("daymet_v4_tmax_annavg_na_", year, ".tif"))
  tmax_y1 = rast(paste0("daymet_v4_tmax_annavg_na_", year1, ".tif"))
  tmax_y2 = rast(paste0("daymet_v4_tmax_annavg_na_", year2,".tif"))
  tmax_y3 = rast(paste0("daymet_v4_tmax_annavg_na_", year3, ".tif"))
  tmax_y4 = rast(paste0("daymet_v4_tmax_annavg_na_", year4, ".tif"))
  tmax_y5 = rast(paste0("daymet_v4_tmax_annavg_na_", year5, ".tif"))
  
  tmin_y0 = rast(paste0("daymet_v4_tmin_annavg_na_", year, ".tif"))
  tmin_y1 = rast(paste0("daymet_v4_tmin_annavg_na_", year1, ".tif"))
  tmin_y2 = rast(paste0("daymet_v4_tmin_annavg_na_", year2, ".tif"))
  tmin_y3 = rast(paste0("daymet_v4_tmin_annavg_na_", year3, ".tif"))
  tmin_y4 = rast(paste0("daymet_v4_tmin_annavg_na_", year4, ".tif"))
  tmin_y5 = rast(paste0("daymet_v4_tmin_annavg_na_", year5, ".tif"))

  prcp_y0 = rast(paste0("daymet_v4_prcp_annttl_na_", year, ".tif"))
  prcp_y1 = rast(paste0("daymet_v4_prcp_annttl_na_", year1, ".tif"))
  prcp_y2 = rast(paste0("daymet_v4_prcp_annttl_na_", year2, ".tif"))
  prcp_y3 = rast(paste0("daymet_v4_prcp_annttl_na_", year3,".tif"))
  prcp_y4 = rast(paste0("daymet_v4_prcp_annttl_na_", year4, ".tif"))
  prcp_y5 = rast(paste0("daymet_v4_prcp_annttl_na_", year5, ".tif"))
  
  lulc_y0 = rast(paste0("CONUS_2022.tif"))
  lulc_y1 = rast(paste0("CONUS_2021.tif"))
  lulc_y2 = rast(paste0("CONUS_2020.tif"))
  lulc_y3 = rast(paste0("CONUS_2019.tif"))
  lulc_y4 = rast(paste0("CONUS_2018.tif"))
  lulc_y5 = rast(paste0("CONUS_2017.tif"))

  ndvi_mean = rast(paste0("wgs_landsat8_", year, "_full.tif"))
  ndvi_mean_y5 = rast(paste0("wgs_landsat8_", year1, "_full.tif"))
  ndvi_mean_y4 = rast(paste0("wgs_landsat8_", year2, "_full.tif"))
  ndvi_mean_y3 = rast(paste0("wgs_landsat8_", year3, "_full.tif"))
  ndvi_mean_y2 = rast(paste0("wgs_landsat8_", year4, "_full.tif"))
  ndvi_mean_y1 = rast(paste0("wgs_landsat8_", year5, "_full.tif"))

  ndvi_min = rast(paste0("Landsat7_ndvi_5_", year, ".tif"))
  ndvi_max = rast(paste0("Landsat7_ndvi_95_", year, ".tif"))
  ndvi_min_y1 = rast(paste0("Landsat7_ndvi_5_", year1, ".tif"))
  ndvi_max_y1 = rast(paste0("Landsat7_ndvi_95_", year1, ".tif"))
  ndvi_min_y2 = rast(paste0("Landsat7_ndvi_5_", year2, ".tif"))
  ndvi_max_y2 = rast(paste0("Landsat7_ndvi_95_", year2, ".tif"))
  ndvi_min_y3 = rast(paste0("Landsat7_ndvi_5_", year3, ".tif"))
  ndvi_max_y3 = rast(paste0("Landsat7_ndvi_95_", year4, ".tif"))
  ndvi_min_y4 = rast(paste0("Landsat7_ndvi_5_", year4, ".tif"))
  ndvi_max_y4 = rast(paste0("Landsat7_ndvi_95_", year4, ".tif"))
  ndvi_min_y5 = rast(paste0("Landsat7_ndvi_5_", year5, ".tif"))
  ndvi_max_y5 = rast(paste0("Landsat7_ndvi_5_", year5, ".tif"))
                                                        
  list_ndvi = c(ndvi_mean$NDVI, ndvi_mean_y1$NDVI, ndvi_mean_y2$NDVI, ndvi_mean_y3$NDVI, ndvi_mean_y4$NDVI,
                ndvi_mean_y5$NDVI)
  list_ndvi1 = c(ndvi_min$NDVI_p5, ndvi_min_y1$NDVI_p5, ndvi_min_y2$NDVI_p5, ndvi_min_y3$NDVI_p5,
                 ndvi_min_y4$NDVI_p5, ndvi_min_y5$NDVI_p5,
                 ndvi_max$NDVI_p95, ndvi_max_y1$NDVI_p95, ndvi_max_y2$NDVI_p95, ndvi_max_y3$NDVI_p95,
                 ndvi_max_y4$NDVI_p95, ndvi_max_y5$NDVI_p95)
  list_ndvi_us = mask(list_ndvi, us_shp)
  list_ndvi_us_1 = mask(resample(list_ndvi1, ndvi_us), us_shp)
  
  list_day = c(tmax_y0, tmax_y1, tmax_y2, tmax_y3, tmax_y4, tmax_y5,
               tmin_y0, tmin_y1, tmin_y2, tmin_y3, tmin_y4, tmin_y5,
               prcp_y0, prcp_y1, prcp_y2, prcp_y3, prcp_y4, prcp_y5)
  #ndvi_us = mask(ndvi_mean, us_shp)
  tmax_y0_pr = terra::project(list_day, "EPSG:4326")
  list_day_us = mask(resample(tmax_y0_pr, ndvi_us), us_shp)

  list_lulc = c(lulc_y0, lulc_y1, lulc_y2, lulc_y3, lulc_y4, lulc_y5)
  lulc_pr = terra::project(list_lulc, "EPSG:4326")
  list_lulc_us = mask(resample(lulc_pr, ndvi_us), us_shp)

  prec_log_y0 = log10(list_day_us$daymet_v4_prcp_annttl_na_2022)
  prec_log_y1 = log10(list_day_us$daymet_v4_prcp_annttl_na_2021)
  prec_log_y2 = log10(list_day_us$daymet_v4_prcp_annttl_na_2020)
  prec_log_y3 = log10(list_day_us$daymet_v4_prcp_annttl_na_2019)
  prec_log_y4 = log10(list_day_us$daymet_v4_prcp_annttl_na_2018)
  prec_log_y5 = log10(list_day_us$daymet_v4_prcp_annttl_na_2017)

  Year <- init(r, "y") |> mask(r)
  Year$val <- year
  Year_us = mask(Year, us_shp)
  
  soilmoist = rast(paste("file", year, "-year-fv08.1.nc_annual.nc"))
  soilmoist_us = mask(resample(soilmoist, ndvi_us), us_shp)
  
  stack_vars = c(list_ndvi_us, list_ndvi_us_1)
  stack_vars1 = c(lon, lat, Year_us$val, list_lulc_us$CONUS_2021, list_day_us$daymet_v4_tmin_annavg_na_2022,
                  list_day_us$daymet_v4_tmin_annavg_na_2021, list_day_us$daymet_v4_tmin_annavg_na_2020,
                  list_day_us$daymet_v4_tmin_annavg_na_2019, list_day_us$daymet_v4_tmin_annavg_na_2018,
                  list_day_us$daymet_v4_tmin_annavg_na_2017, list_day_us$daymet_v4_tmax_annavg_na_2022,
                  list_day_us$daymet_v4_tmax_annavg_na_2021, list_day_us$daymet_v4_tmax_annavg_na_2020,
                  list_day_us$daymet_v4_tmax_annavg_na_2019, list_day_us$daymet_v4_tmax_annavg_na_2018,
                  list_day_us$daymet_v4_tmax_annavg_na_2017)
  stack_vars2 = c(prec_log_y0, prec_log_y1, prec_log_y2, prec_log_y3, prec_log_y4, prec_log_y5,
                  clay_us, slope_log_us, list_topo_us[[3]], geol, soilmoist) #elev_log_us
  
  names(stack_vars) = c("SOC1987.NDVI_mean", "SOC1987.NDVI_mean_y1", "SOC1987.NDVI_mean_y2", "SOC1987.NDVI_mean_y3",
                        "SOC1987.NDVI_mean_y4", "SOC1987.NDVI_mean_y5", "SOC1987.NDVI_min", "SOC1987.NDVI_min_y1",
                        "SOC1987.NDVI_min_y2", "SOC1987.NDVI_min_y3", "SOC1987.NDVI_min_y4", "SOC1987.NDVI_min_y5",
                        "SOC1987.NDVI_max","SOC1987.NDVI_max_y1", "SOC1987.NDVI_max_y2", "SOC1987.NDVI_max_y3",
                        "SOC1987.NDVI_max_y4", "SOC1987.NDVI_max_y5")
  names(stack_vars1) = c("Longitude", "Latitude", "Year","LULC_y1",
                         "SOC1987.Tmin", "SOC1987.Tmin_yr1", "SOC1987.Tmin_yr2", "SOC1987.Tmin_yr3",
                         "SOC1987.Tmin_yr4","SOC1987.Tmin_yr5", "SOC1987.Tmax", "SOC1987.Tmax_yr1",
                         "SOC1987.Tmax_yr2",
                         "SOC1987.Tmax_yr3",
                         "SOC1987.Tmax_yr4", "SOC1987.Tmax_yr5", "SoilMoisture1")
  
  names(stack_vars2) = c("Precip_log", "Precip_y1_log", "Precip_y2_log", "Precip_y3_log", "Precip_y4_log",
                         "Precip_y5_log", "Clay", "Slope_log", "SOC1987.Aspect", "Bedrock", "SoilMoisture") #Elev.log
  stack_vars_new = c(stack_vars, stack_vars1, stack_vars2)

  #stack_vars_new[is.na(stack_vars_new)] <- 0
  #nas = is.na(stack_vars_new)
  #plot(nas)
  load('RF_01092024.RData')

  RF_preds = predict(model=RFtraining, object=stack_vars_new)

  RF_preds_SOC = exp(RF_preds)
  terra::writeCDF(RF_preds_SOC, paste0("RF_SOC_", year, ".nc"))
}

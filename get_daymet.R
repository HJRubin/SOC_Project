library(daymetr)
library(dplyr)
library(terra)
library(sp)
library(ggplot2)

#Need get_SOC_vars.R and Input_SOC_Obs.R

setwd("~/Dissertation Work/My Work/FAA/Data")
# df = read.csv("full_df_with_ndvi.csv")
df = read.csv("full_df5_with_ndvi_03212024.csv")
# soils_agg = read.csv("all5_soils_SOC_30cm_kgm2_03212024.csv")

# soils_agg = soils_agg[soils_agg$Year >= 1983 & soils_agg$Year < 1992,]
colnames(df)[colnames(df) == 'Year.1'] <- 'Year'
# colnames(soils_agg)[colnames(soils_agg) == 'sliceID'] <- 'SliceID'
# colnames(soils_agg)[colnames(soils_agg) == '.oldTop'] <- 'Top'
# colnames(soils_agg)[colnames(soils_agg) == '.oldBottom'] <- 'Bottom'
# df$Layer_Name = as.character(df$Layer_Name)
# df$SliceID = as.character(df$SliceID)
# soils_agg$SliceID = as.character(soils_agg$SliceID)
# df$Layer_Name = as.character(df$Layer_Name)
# df = df[df$Year < 1992,]
df_year = df[-23]
all_ndvi_pre_1 = all_ndvi_pre[-5]

df1 = left_join(df_year, soils_agg, by=c("Latitude", "Longitude", "Year", "SOC_kgm2","Cluster_ID"))
df_pre = left_join(all_ndvi_pre_1, soils_agg, by=c("Latitude", "Longitude", "Year", "SOC_kgm2","Cluster_ID"))

df1_v = vect(df1, geom = c("Longitude", "Latitude"))
df1_pre_v = vect(df_pre, geom = c("Longitude", "Latitude"))

daymet_df = data.frame(matrix(nrow = 0, ncol = 2))
daymet_df_5yr = data.frame(matrix(nrow = 0, ncol = 2))
daymet_df_4yr = data.frame(matrix(nrow = 0, ncol = 2))
daymet_df_3yr = data.frame(matrix(nrow = 0, ncol = 2))
daymet_df_2yr = data.frame(matrix(nrow = 0, ncol = 2))
daymet_df_1yr = data.frame(matrix(nrow = 0, ncol = 2))

daymet_df_pre = data.frame(matrix(nrow = 0, ncol = 2))
daymet_df_pre1 = data.frame(matrix(nrow = 0, ncol = 2))
daymet_df_pre2 = data.frame(matrix(nrow = 0, ncol = 2))
daymet_df_pre3 = data.frame(matrix(nrow = 0, ncol = 2))
daymet_df_pre4 = data.frame(matrix(nrow = 0, ncol = 2))
daymet_df_pre5 = data.frame(matrix(nrow = 0, ncol = 2))

get_daymet <- function(i, df1){
  
  # print(df1_v$Year[i])
  tryCatch(exp = {
  temp_daymet <- download_daymet(
    lat = df1_v$lat[i],
    lon = df1_v$lon[i],
    start = df1_v$Year[i] - 1, 
    end = df1_v$Year[i] - 1
  ) 
  Precip = aggregate(prcp..mm.day. ~ year, data = temp_daymet$data, FUN = sum)
  Tmax = aggregate(tmax..deg.c. ~ year, data = temp_daymet$data, FUN = mean)
  Tmin = aggregate(tmin..deg.c. ~ year, data = temp_daymet$data, FUN = mean)
  Snow = aggregate(swe..kg.m.2. ~ year, data = temp_daymet$data, FUN = sum)
  
  new_row = as.data.frame(Precip$year)
  new_row$Precip = Precip$prcp..mm.day.
  new_row$Tmax = Tmax$tmax..deg.c.
  new_row$Tmin = Tmin$tmin..deg.c.
  new_row$Snow = Snow$swe..kg.m.2.
  
  df1 = rbind(df1, new_row)}
  , error = function(e) NULL)
  return(df1)
}  

usa <- map_data("usa")
v = vect(usa, geom = c("long", "lat"))
pts = crop(df1_v, v)

# daymet_df_5yr = lapply(1:nrow(df1), get_daymet, daymet_df_5yr)
# daymet_df_4yr = lapply(1:nrow(df1), get_daymet, daymet_df_4yr)
# daymet_df_3yr = lapply(1:nrow(df1), get_daymet, daymet_df_3yr)
# daymet_df_2yr = lapply(1:nrow(df1), get_daymet, daymet_df_2yr)
# daymet_df_1yr = lapply(1:nrow(df1), get_daymet, daymet_df_1yr)
# daymet_df = lapply(1:nrow(df1), get_daymet, daymet_df)

# daymet_pre = lapply(1:nrow(df_pre), get_daymet, daymet_df_pre)
# daymet_pre1 = lapply(1:nrow(df_pre), get_daymet, daymet_df_pre1)
# daymet_pre2 = lapply(1:nrow(df_pre), get_daymet, daymet_df_pre2)
# daymet_pre3 = lapply(1:nrow(df_pre), get_daymet, daymet_df_pre3)
# daymet_pre4 = lapply(1:nrow(df_pre), get_daymet, daymet_df_pre4)
# daymet_pre5 = lapply(1:nrow(df_pre), get_daymet, daymet_df_pre5)


###############################HERE 09/15/2023
df_5yr = as.data.frame(df1)
df_4yr = as.data.frame(df1)
df_3yr = as.data.frame(df1)
df_2yr = as.data.frame(df1)
df_1yr = as.data.frame(df1)
df_yr = as.data.frame(df1)

df_pre_1yr = as.data.frame(df_pre)
df_pre_2yr = as.data.frame(df_pre)
df_pre_3yr = as.data.frame(df_pre)
df_pre_4yr = as.data.frame(df_pre)
df_pre_5yr = as.data.frame(df_pre)
df_pre_yr = as.data.frame(df_pre)


Preciprow = NA
Tmaxrow = NA
Tminrow = NA
Snowrow = NA

get_daymet_from_df = function(df3, daymetdf){
    for(i in 1:nrow(df3)){
    # for (j in nrow(df3[i])){
  firstrow = as.data.frame(daymetdf[i])#[j])
  # print(i)
  # print(firstrow)
  # print(Preciprow[i])
  if(is.null(firstrow$Precip)){
    Preciprow[i] = NA
    Tmaxrow[i] = NA
    Tminrow[i] = NA
    Snowrow[i] = NA
  }
  else{
    Preciprow[i] = firstrow$Precip#[[3]]
    Tmaxrow[i] = firstrow$Tmax#[[3]]
    Tminrow[i] = firstrow$Tmin#[[3]]
    Snowrow[i] = firstrow$Snow#[[3]]
    }
  }#}
  df3$Precip = Preciprow
  df3$Tmax = Tmaxrow
  df3$Tmin = Tminrow
  df3$Snow = Snowrow
  return(df3)}

# df_5yr_fin = get_daymet_from_df(df_5yr, daymet_df_5yr)
# df_4yr_fin = get_daymet_from_df(df_4yr, daymet_df_4yr)
# df_3yr_fin = get_daymet_from_df(df_3yr, daymet_df_3yr)
# df_2yr_fin = get_daymet_from_df(df_2yr, daymet_df_2yr)
# df_1yr_fin = get_daymet_from_df(df_1yr, daymet_df_1yr)
# df_yr_fin = get_daymet_from_df(df_yr, daymet_df)

# df_yr_pre = get_daymet_from_df(df_pre_yr, daymet_pre)
# df_yr_pre1 = get_daymet_from_df(df_pre_1yr, daymet_pre1)
# df_yr_pre2 = get_daymet_from_df(df_pre_2yr, daymet_pre2)
# df_yr_pre3 = get_daymet_from_df(df_pre_3yr, daymet_pre3)
# df_yr_pre4 = get_daymet_from_df(df_pre_4yr, daymet_pre4)
# df_yr_pre5 = get_daymet_from_df(df_pre_5yr, daymet_pre5)


merge_pre = merge(df_yr_pre, df_yr_pre1[, c("Latitude", "Longitude", "Year", "SOC_kgm2", "Snow",
                                            "Precip", "Tmax", "Tmin")],
                  by = c("Latitude", "Longitude", "Year", "SOC_kgm2"))
colnames(merge_pre)[colnames(merge_pre) == 'Snow.x'] <- 'Snow'
colnames(merge_pre)[colnames(merge_pre) == 'Tmax.x'] <- 'Tmax'
colnames(merge_pre)[colnames(merge_pre) == 'Tmin.x'] <- 'Tmin'
colnames(merge_pre)[colnames(merge_pre) == 'Precip.x'] <- 'Precip'
colnames(merge_pre)[colnames(merge_pre) == 'Snow.y'] <- 'Snow_yr1'
colnames(merge_pre)[colnames(merge_pre) == 'Tmax.y'] <- 'Tmax_yr1'
colnames(merge_pre)[colnames(merge_pre) == 'Tmin.y'] <- 'Tmin_yr1'
colnames(merge_pre)[colnames(merge_pre) == 'Precip.y'] <- 'Precip_yr1'
merge_pre2 = merge(merge_pre, df_yr_pre2[, c("Latitude", "Longitude", "Year", "SOC_kgm2", "Snow",
                                            "Precip", "Tmax", "Tmin")], 
                  by = c("Latitude", "Longitude", "Year", "SOC_kgm2"))
colnames(merge_pre2)[colnames(merge_pre2) == 'Snow.x'] <- 'Snow'
colnames(merge_pre2)[colnames(merge_pre2) == 'Tmax.x'] <- 'Tmax'
colnames(merge_pre2)[colnames(merge_pre2) == 'Tmin.x'] <- 'Tmin'
colnames(merge_pre2)[colnames(merge_pre2) == 'Precip.x'] <- 'Precip'
colnames(merge_pre2)[colnames(merge_pre2) == 'Snow.y'] <- 'Snow_yr2'
colnames(merge_pre2)[colnames(merge_pre2) == 'Tmax.y'] <- 'Tmax_yr2'
colnames(merge_pre2)[colnames(merge_pre2) == 'Tmin.y'] <- 'Tmin_yr2'
colnames(merge_pre2)[colnames(merge_pre2) == 'Precip.y'] <- 'Precip_yr2'

merge_pre3 = merge(merge_pre2, df_yr_pre3[, c("Latitude", "Longitude", "Year", "SOC_kgm2", "Snow",
                                            "Precip", "Tmax", "Tmin")], 
                  by = c("Latitude", "Longitude", "Year", "SOC_kgm2"))
colnames(merge_pre3)[colnames(merge_pre3) == 'Snow.x'] <- 'Snow'
colnames(merge_pre3)[colnames(merge_pre3) == 'Tmax.x'] <- 'Tmax'
colnames(merge_pre3)[colnames(merge_pre3) == 'Tmin.x'] <- 'Tmin'
colnames(merge_pre3)[colnames(merge_pre3) == 'Precip.x'] <- 'Precip'
colnames(merge_pre3)[colnames(merge_pre3) == 'Snow.y'] <- 'Snow_yr3'
colnames(merge_pre3)[colnames(merge_pre3) == 'Tmax.y'] <- 'Tmax_yr3'
colnames(merge_pre3)[colnames(merge_pre3) == 'Tmin.y'] <- 'Tmin_yr3'
colnames(merge_pre3)[colnames(merge_pre3) == 'Precip.y'] <- 'Precip_yr3'

merge_pre4 = merge(merge_pre3, df_yr_pre4[, c("Latitude", "Longitude", "Year", "SOC_kgm2", "Snow",
                                            "Precip", "Tmax", "Tmin")], 
                  by = c("Latitude", "Longitude", "Year", "SOC_kgm2"))
colnames(merge_pre4)[colnames(merge_pre4) == 'Snow.x'] <- 'Snow'
colnames(merge_pre4)[colnames(merge_pre4) == 'Tmax.x'] <- 'Tmax'
colnames(merge_pre4)[colnames(merge_pre4) == 'Tmin.x'] <- 'Tmin'
colnames(merge_pre4)[colnames(merge_pre4) == 'Precip.x'] <- 'Precip'
colnames(merge_pre4)[colnames(merge_pre4) == 'Snow.y'] <- 'Snow_yr4'
colnames(merge_pre4)[colnames(merge_pre4) == 'Tmax.y'] <- 'Tmax_yr4'
colnames(merge_pre4)[colnames(merge_pre4) == 'Tmin.y'] <- 'Tmin_yr4'
colnames(merge_pre4)[colnames(merge_pre4) == 'Precip.y'] <- 'Precip_yr4'

merge_pre5 = merge(merge_pre4, df_yr_pre5[, c("Latitude", "Longitude", "Year", "SOC_kgm2", "Snow",
                                            "Precip", "Tmax", "Tmin")], 
                  by = c("Latitude", "Longitude", "Year", "SOC_kgm2"))
colnames(merge_pre5)[colnames(merge_pre5) == 'Snow.x'] <- 'Snow'
colnames(merge_pre5)[colnames(merge_pre5) == 'Tmax.x'] <- 'Tmax'
colnames(merge_pre5)[colnames(merge_pre5) == 'Tmin.x'] <- 'Tmin'
colnames(merge_pre5)[colnames(merge_pre5) == 'Precip.x'] <- 'Precip'
colnames(merge_pre5)[colnames(merge_pre5) == 'Snow.y'] <- 'Snow_yr5'
colnames(merge_pre5)[colnames(merge_pre5) == 'Tmax.y'] <- 'Tmax_yr5'
colnames(merge_pre5)[colnames(merge_pre5) == 'Tmin.y'] <- 'Tmin_yr5'
colnames(merge_pre5)[colnames(merge_pre5) == 'Precip.y'] <- 'Precip_yr5'

#########################################
merge_df1 = merge(df_yr_fin, df_1yr_fin[, c("Latitude", "Longitude", "Year", "SOC_kgm2", "Snow",
                                        "Precip", "Tmax", "Tmin")], 
                  by = c("Latitude", "Longitude", "Year", "SOC_kgm2"))
colnames(merge_df1)[colnames(merge_df1) == 'Snow.x'] <- 'Snow'
colnames(merge_df1)[colnames(merge_df1) == 'Tmax.x'] <- 'Tmax'
colnames(merge_df1)[colnames(merge_df1) == 'Tmin.x'] <- 'Tmin'
colnames(merge_df1)[colnames(merge_df1) == 'Precip.x'] <- 'Precip'
colnames(merge_df1)[colnames(merge_df1) == 'Snow.y'] <- 'Snow_yr1'
colnames(merge_df1)[colnames(merge_df1) == 'Tmax.y'] <- 'Tmax_yr1'
colnames(merge_df1)[colnames(merge_df1) == 'Tmin.y'] <- 'Tmin_yr1'
colnames(merge_df1)[colnames(merge_df1) == 'Precip.y'] <- 'Precip_yr1'

merge_df2 = merge(merge_df1, df_2yr_fin[, c("Latitude", "Longitude", "Year", "SOC_kgm2", "Snow",
                                            "Precip", "Tmax", "Tmin")], 
                  by = c("Latitude", "Longitude", "Year", "SOC_kgm2"))
colnames(merge_df2)[colnames(merge_df2) == 'Snow.x'] <- 'Snow'
colnames(merge_df2)[colnames(merge_df2) == 'Tmax.x'] <- 'Tmax'
colnames(merge_df2)[colnames(merge_df2) == 'Tmin.x'] <- 'Tmin'
colnames(merge_df2)[colnames(merge_df2) == 'Precip.x'] <- 'Precip'
colnames(merge_df2)[colnames(merge_df2) == 'Snow.y'] <- 'Snow_yr2'
colnames(merge_df2)[colnames(merge_df2) == 'Tmax.y'] <- 'Tmax_yr2'
colnames(merge_df2)[colnames(merge_df2) == 'Tmin.y'] <- 'Tmin_yr2'
colnames(merge_df2)[colnames(merge_df2) == 'Precip.y'] <- 'Precip_yr2'

merge_df3 = merge(merge_df2, df_3yr_fin[, c("Latitude", "Longitude", "Year", "SOC_kgm2", "Snow",
                                            "Precip", "Tmax", "Tmin")], 
                  by = c("Latitude", "Longitude", "Year", "SOC_kgm2"))
colnames(merge_df3)[colnames(merge_df3) == 'Snow.x'] <- 'Snow'
colnames(merge_df3)[colnames(merge_df3) == 'Tmax.x'] <- 'Tmax'
colnames(merge_df3)[colnames(merge_df3) == 'Tmin.x'] <- 'Tmin'
colnames(merge_df3)[colnames(merge_df3) == 'Precip.x'] <- 'Precip'
colnames(merge_df3)[colnames(merge_df3) == 'Snow.y'] <- 'Snow_yr3'
colnames(merge_df3)[colnames(merge_df3) == 'Tmax.y'] <- 'Tmax_yr3'
colnames(merge_df3)[colnames(merge_df3) == 'Tmin.y'] <- 'Tmin_yr3'
colnames(merge_df3)[colnames(merge_df3) == 'Precip.y'] <- 'Precip_yr3'

merge_df4 = merge(merge_df3, df_4yr_fin[, c("Latitude", "Longitude", "Year", "SOC_kgm2", "Snow",
                                            "Precip", "Tmax", "Tmin")], 
                  by = c("Latitude", "Longitude", "Year", "SOC_kgm2"))

colnames(merge_df4)[colnames(merge_df4) == 'Snow.x'] <- 'Snow'
colnames(merge_df4)[colnames(merge_df4) == 'Tmax.x'] <- 'Tmax'
colnames(merge_df4)[colnames(merge_df4) == 'Tmin.x'] <- 'Tmin'
colnames(merge_df4)[colnames(merge_df4) == 'Precip.x'] <- 'Precip'
colnames(merge_df4)[colnames(merge_df4) == 'Snow.y'] <- 'Snow_yr4'
colnames(merge_df4)[colnames(merge_df4) == 'Tmax.y'] <- 'Tmax_yr4'
colnames(merge_df4)[colnames(merge_df4) == 'Tmin.y'] <- 'Tmin_yr4'
colnames(merge_df4)[colnames(merge_df4) == 'Precip.y'] <- 'Precip_yr4'

merge_df5 = merge(merge_df4, df_5yr_fin[, c("Latitude", "Longitude", "Year", "SOC_kgm2", "Snow",
                                            "Precip", "Tmax", "Tmin")], 
                  by = c("Latitude", "Longitude", "Year", "SOC_kgm2"))

colnames(merge_df5)[colnames(merge_df5) == 'Snow.x'] <- 'Snow'
colnames(merge_df5)[colnames(merge_df5) == 'Tmax.x'] <- 'Tmax'
colnames(merge_df5)[colnames(merge_df5) == 'Tmin.x'] <- 'Tmin'
colnames(merge_df5)[colnames(merge_df5) == 'Precip.x'] <- 'Precip'
colnames(merge_df5)[colnames(merge_df5) == 'Snow.y'] <- 'Snow_yr5'
colnames(merge_df5)[colnames(merge_df5) == 'Tmax.y'] <- 'Tmax_yr5'
colnames(merge_df5)[colnames(merge_df5) == 'Tmin.y'] <- 'Tmin_yr5'
colnames(merge_df5)[colnames(merge_df5) == 'Precip.y'] <- 'Precip_yr5'

# df2_vect = vect(merge_df5, geom = c("Longitude", "Latitude"))

merge_pre5$NDVI_mean_y1 = merge_pre5$NDVI_mean
merge_pre5$NDVI_mean_y2 = merge_pre5$NDVI_mean
merge_pre5$NDVI_mean_y3 = merge_pre5$NDVI_mean
merge_pre5$NDVI_mean_y4 = merge_pre5$NDVI_mean
merge_pre5$NDVI_mean_y5 = merge_pre5$NDVI_mean

merge_pre5$NDVI_max_y1 = merge_pre5$NDVI_max
merge_pre5$NDVI_max_y2 = merge_pre5$NDVI_max
merge_pre5$NDVI_max_y3 = merge_pre5$NDVI_max
merge_pre5$NDVI_max_y4 = merge_pre5$NDVI_max
merge_pre5$NDVI_max_y5 = merge_pre5$NDVI_max

merge_pre5$NDVI_min_y1 = merge_pre5$NDVI_min
merge_pre5$NDVI_min_y2 = merge_pre5$NDVI_min
merge_pre5$NDVI_min_y3 = merge_pre5$NDVI_min
merge_pre5$NDVI_min_y4 = merge_pre5$NDVI_min
merge_pre5$NDVI_min_y5 = merge_pre5$NDVI_min

merge_pre5$X.1 = merge_pre5$X.x

df_all = rbind(merge_df5, merge_pre5)

df3_vect = vect(df_all, geom = c("Longitude", "Latitude"))

write.csv(df_all, "intermediate_df_all_daymet_03222024.csv")

####################Topography#############################
# write.csv(as.data.frame(df2_vect), "all_ndvi_daymet_slope.csv")
# df2_vect = read.csv("all_ndvi_daymet_slope.csv")
crs(df3_vect) = "EPSG:4326"
df3_proj = project(df3_vect, crs(slope))

setwd("~/ArcGIS/Projects/FAA_SOC/")
geol = rast("SGMC_Geology_general.tif")
geol_proj = terra::project(geol, "epsg:4326")

df_extract = terra::extract(geol_proj, df3_vect)
df3_vect$Geol = df_extract$GENERALIZE

slope <- rast("slope.tif")
aspect <- rast("Agg_aspect.tif")
elevation <- rast("Agg_elev.tif")

elevation_proj = project(elevation, "epsg:4326")
slope_proj = project(slope, "epsg:4326")
aspect_proj = project(aspect, "epsg:4326")

elev_ext = terra::extract(elevation, df3_proj)
slope_ext = terra::extract(slope, df3_proj)
aspect_ext = terra::extract(aspect, df3_proj)

df3_proj$Elev = elev_ext$Band_1
df3_proj$Slope = slope_ext$Band_1
df3_proj$Aspect = aspect_ext$Band_1

df3_return = project(df3_proj, "epsg:4326")

####################LULC##################################
setwd("~/Dissertation Work/My Work/FAA/Data/LandUse/Copy")
# 
filenames <- list.files(pattern="*.tif", full.names=TRUE)

lulc_rast_list <- lapply(filenames, rast)

df3_return$Latitude = df3_return$lat
df3_return$Longitude = df3_return$lon

set_SOC_name = function(years){
  setNames(list(as.data.frame(df3_return[df3_return$Year == years,])), paste0("SOC", years))
}
SOC_list = lapply(1983:2018, set_SOC_name)

vect_SOC = function(i){
  vect(as.data.frame(SOC_list[i]), 
       geom = c(names(as.data.frame(SOC_list[i]))[57],names(as.data.frame(SOC_list[i]))[56]))
}
SOC_vects = lapply(5:36, vect_SOC)

proj_SOC = function(i){
  crs(SOC_vects[[i]]) = "epsg:4326"
  project(SOC_vects[[i]], crs(lulc_rast_list[[1]]))
}
SOC_vects_proj = lapply(1:32, proj_SOC)

#This gets year 0 and previous 5 years for 1989-2018
output_lulc = data.frame(matrix(nrow = 0, ncol = 6))
output_lulc1 = data.frame(matrix(nrow = 0, ncol = 6))
for (i in 6:33){
  # tryCatch(exp = {
  lulc_y0 = terra::extract(lulc_rast_list[[i]], SOC_vects_proj[[i-2]])
  lulc_y1 = terra::extract(lulc_rast_list[[i-1]], SOC_vects_proj[[i-2]])
  lulc_y2 = terra::extract(lulc_rast_list[[i-2]], SOC_vects_proj[[i-2]])
  lulc_y3 = terra::extract(lulc_rast_list[[i-3]], SOC_vects_proj[[i-2]])
  lulc_y4 = terra::extract(lulc_rast_list[[i-4]], SOC_vects_proj[[i-2]])
  lulc_y5 = terra::extract(lulc_rast_list[[i-5]], SOC_vects_proj[[i-2]])
  ids = as.data.frame(lulc_y0$ID)
  ids$LULC_y0 = lulc_y0[2]
  ids$LULC_y1 = lulc_y1[2]
  ids$LULC_y2 = lulc_y2[2]
  ids$LULC_y3 = lulc_y3[2]
  ids$LULC_y4 = lulc_y4[2]
  ids$LULC_y5 = lulc_y5[2]
  ids$Year = i + 1989
  # error = function(e) NULL)
  output_lulc = rbind(output_lulc, ids)
}

for (i in 3:5){
  lulc_y0 = terra::extract(lulc_rast_list[[i]], SOC_vects_proj[[i-2]])
  ids = as.data.frame(lulc_y0$ID)
  ids$LULC_y0 = lulc_y0[2]
  ids$Year = i + 1989
  output_lulc1 = rbind(output_lulc1, ids)
}

names(output_lulc) = c("ID", "LULC_y0", "LULC_y1", "LULC_y2", 'LULC_y3', 
                       "LULC_y4", "LULC_y5", "Year")
names(output_lulc1) = c("ID", "LULC_y0", "Year")

output_lulc1$LULC_y0 = output_lulc1$LULC_y0$CONUS_1992
output_lulc1$LULC_y1 = output_lulc1$LULC_y0
output_lulc1$LULC_y2 = output_lulc1$LULC_y0
output_lulc1$LULC_y3 = output_lulc1$LULC_y0
output_lulc1$LULC_y4 = output_lulc1$LULC_y0
output_lulc1$LULC_y5 = output_lulc1$LULC_y0

output_lulc$LULC_y0 = output_lulc$LULC_y0$CONUS_1995
output_lulc$LULC_y1 = output_lulc$LULC_y1$CONUS_1994
output_lulc$LULC_y2 = output_lulc$LULC_y2$CONUS_1993
output_lulc$LULC_y3 = output_lulc$LULC_y3$CONUS_1992
output_lulc$LULC_y4 = output_lulc$LULC_y4$CONUS_1991
output_lulc$LULC_y5 = output_lulc$LULC_y5$CONUS_1990

all_output = rbind(output_lulc, output_lulc1)


SOC_vects_proj_df = as.data.frame(SOC_vects_proj[[1]])

for (i in 1:32){
  names(SOC_vects_proj[[i]]) = names(SOC_vects_proj[[1]])
  SOC_vects_proj_df = rbind(SOC_vects_proj_df, as.data.frame(SOC_vects_proj[[i]]))
}

colnames(SOC_vects_proj_df)[colnames(SOC_vects_proj_df) == 'SOC1987.ID'] <- 'ID'
colnames(SOC_vects_proj_df)[colnames(SOC_vects_proj_df) == 'SOC1987.Year'] <- 'Year'

all_ndvi_daymet_slope_lulc = merge(SOC_vects_proj_df, all_output, by = c("ID", "Year"))

df4 = project(df3_vect, "epsg:5070")
df4_todf = as.data.frame(df4)
all_ndvi_daymet_slope_lulc$Latitude = all_ndvi_daymet_slope_lulc$SOC1987.lat
all_ndvi_daymet_slope_lulc$Longitude = all_ndvi_daymet_slope_lulc$SOC1987.lon
all_ndvi_daymet_slope_lulc$SOC_gcm2 = all_ndvi_daymet_slope_lulc$SOC1987.SOC_gcm2
df4_todf$Latitude = df4_todf$lat
df4_todf$Longitude = df4_todf$long

v = vect(all_ndvi_daymet_slope_lulc, geom = c("Longitude", "Latitude"))
df_extract = terra::extract(geol_proj, v)
v$Geol = df_extract$GENERALIZE

all_ndvi_daymet_slope_lulc_geol = as.data.frame(v)
  
# write.csv(all_ndvi_daymet_slope_lulc_geol, "all_ndvi_daymet_slope_lulc_geol.csv")
# df = read.csv("all_ndvi_daymet_slope_lulc.csv")

setwd("~/Dissertation Work/My Work/FAA/Data/NACP_MsTMIP_Unified_NA_SoilMap_1242/Data")

clay = rast("Unified_NA_Soil_Map_Subsoil_Clay_Fraction.tif")

ext_clay = terra::extract(clay, v)
v$Clay = ext_clay$Unified_NA_Soil_Map_Subsoil_Clay_Fraction

all_ndvi_daymet_slope_lulc_geol_clay = as.data.frame(v)

# write.csv(all_ndvi_daymet_slope_lulc_geol_clay, "all_ndvi_daymet_slope_lulc_geol_clay_03252024.csv")


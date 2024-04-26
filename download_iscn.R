#Written by Hannah Rubin
#Department of Civil and Environmental Engineering, UTK
#For FAA Funded Project
#February 2023

#This script takes soil carbon observations and matched landsat pixels
#and filters for completeness and quality.
#Also checks distribution and removes outliers.

library(dplyr)
library(ggplot2)
library(sf)

#First, import data
setwd("~/Dissertation Work/My Work/FAA/Data/Output")

temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.delim)

list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv), envir = .GlobalEnv)

wgs_landsat5 = do.call(rbind, mget(ls(pattern = "wgs_landsat5")))
wgs_landsat7 = do.call(rbind, mget(ls(pattern = "wgs_landsat7")))

wgs_landsat7 = wgs_landsat7 %>% select(-c("B6_VCID_2", "B8"))
colnames(wgs_landsat7)[colnames(wgs_landsat7) == "B6_VCID_1"] <- "B6"

wgs_landsat_pnts = rbind(wgs_landsat5, wgs_landsat7)

wgs_landsat_pnts = wgs_landsat_pnts %>%  
  group_by(Site_Name, observation_date) %>%
  mutate(ID = cur_group_id())

wgs_landsat_pnts_unq = wgs_landsat_pnts %>% select(-c("system.index"))
wgs_landsat_pnts_unq = unique(wgs_landsat_pnts_unq)

separated_coord = wgs_landsat_pnts_unq %>%
  separate(.geo, c("first", "lat", "lon"), ",")
library(splitstackshape)
separated_coord1 = cSplit(separated_coord, c("lat", "lon"), c("[", "]"), drop = FALSE)
separated_coord1$longitude = separated_coord1$lat_4
separated_coord1$latitude = separated_coord1$lon_1

wgs_landsat_pnts_unq_nogeo = separated_coord1 %>% select(-c("first", "lat", "lon", "lat_1",
                                    "lat_4","lon_1","lat_2", "lat_3", "lon_2", "lon_3", "lon_4"))

wgs_landsat_agg = aggregate(SOC_gcm2 ~ ID, data = wgs_landsat_pnts_unq_nogeo, FUN = mean)

wgs_landsat_merge = merge(wgs_landsat_agg, wgs_landsat_pnts_unq_nogeo[, 
                            c("B1", "B2", "B3", "B4", "B5", "B6", "B7", 
                              "NDVI", "Difference", "ID", "latitude", "longitude", "imDate")], by = "ID")
wgs_landsat_merge$abs_Difference = abs(wgs_landsat_merge$Difference)

df = wgs_landsat_merge %>% 
  group_by(ID) %>% 
  dplyr::slice(which.min(abs_Difference))

# write.csv(df, "wgs_landsat_points.csv")

setwd("C:/Users/User/OneDrive - University of Tennessee/Documents/R/SOC")
ISCN = read.csv("ISCN_pnts_clean_vars.csv")

ISCN_lats = ISCN %>% select(c("latitude", "longitude"))
ISCN_lats$latitude = round(ISCN_lats$latitude, 3)
ISCN_lats$longitude = round(ISCN_lats$longitude, 3)
df  = df %>% ungroup
df_lats = df %>% select(c("latitude", "longitude"))
df_lats$latitude = round(df_lats$latitude, 3)
df_lats$longitude = round(df_lats$longitude, 3)

lats = rbind(ISCN_lats, df_lats)
lats = unique(lats)

lats = lats %>%
group_by(latitude, longitude) %>%
mutate(location_ID = cur_group_id())

ISCN_id = merge(ISCN, lats, by = c("latitude", "longitude"))
df$latitude = round(df$latitude, 3)
df$longitude = round(df$longitude, 3)
df_id = merge(df, lats, by = c("latitude", "longitude"))

coordinates(ISCN_id) = ISCN_id[c("latitude", "longitude")]
coordinates(df_id) = df_id[c("latitude", "longitude")]
ISCN_sf <- st_as_sf(ISCN_id)           # convert to simple features
df_sf <- st_as_sf(df_id)           # convert to simple features


full_df = st_join(ISCN_sf, df_sf, join = st_nearest_feature)

write.csv(full_df, "full_df_for_ML.csv")

# ISCN_pnts = st_read("C:/Users/User/OneDrive - University of Tennessee/Documents/ArcGIS/
# Projects/FAA_SOC/FAA_SOC.gdb", layer = "ISCNData_wgs_Merge")
# DEM_pnts = st_read("C:/Users/User/OneDrive - University of Tennessee/Documents/
# ArcGIS/Projects/FAA_SOC/FAA_SOC.gdb", layer = "DEM_points_merge")
# 
# ISCN_pnts_clean = ISCN_pnts[!is.na(ISCN_pnts$F_soc__g_cm_2__),]
# ISCN_pnts_clean_vars = ISCN_pnts_clean %>% 
#   select(c("latitude", "longitude", "observation_date__YYYY_MM_DD_","F_soc__g_cm_2__",
#            "wc21_30s_prec_summer", "wc21_30s_tavg_summer", "SGMC_Geology_unit", "SGMC_Geology_general"))
# 
# ISCN_pnts_clean_vars$Elevation <- DEM_pnts$mean[match(ISCN_pnts_clean_vars$F_soc__g_cm_2__, 
# DEM_pnts$F_soc__g_cm_2__)]
# 
# write.csv(ISCN_pnts_clean_vars, "ISCN_pnts_clean_vars.csv")


# wgs_landsat = read.csv("wgs_landsat_points.csv")

library(tidyr)
library(dplyr)
setwd("~/R/SOC")
wosis = read.csv("wosis_latest/wosis_latest_orgc.csv")
wosis_us = wosis[wosis$country_name == "United States of America",]
wosis_us$orgc_value_avg_gcm3 = wosis_us$orgc_value_avg / 10

# wosis_us_surf = wosis_us[wosis_us$lower_depth == "10",]
# ISCN_surf = SOC_all_wgs84_1982[SOC_all_wgs84_1982$layer_bottom_cm == "10",]

plot(wosis_us_surf$X, wosis_us_surf$Y)
plot(ISCN_surf$Longitude, ISCN_surf$Latitude)

wosis_us_surf_date = separate_wider_delim(wosis_us, cols = orgc_date, delim = ",", 
                                          names = c("Date1", "Date2"), too_few = "debug", too_many = "debug")
wosis_us_surf_date_1 = separate_wider_delim(wosis_us_surf_date, cols = Date1, delim = ":", 
                                          names = c("num", "obs_date"), too_few = "debug", too_many = "debug")
wosis_us_surf_date_2 = separate_wider_delim(wosis_us_surf_date_1, cols = obs_date, delim = "}", 
                                            names = c("full_date", "ex"), too_few = "debug", too_many = "debug")
wosis_us_surf_date_2$full_date = as.Date(wosis_us_surf_date_2$full_date)

wosis_us_surf_1982 = wosis_us_surf_date_2[wosis_us_surf_date_2$full_date > 1982,]
wosis_us_surf_1982$Month = format(as.Date(wosis_us_surf_1982$full_date), "%m")
wosis_us_surf_1982$Year = format(as.Date(wosis_us_surf_1982$full_date), "%Y")
wosis_us_surf_1982 = wosis_us_surf_1982[!is.na(wosis_us_surf_1982$full_date), ]

wosis_sel = wosis_us_surf_1982 %>% select(c("X", "Y", "profile_layer_id", "upper_depth", "lower_depth", "orgc_value_avg",
                               "Month", "Year", "layer_name"))

colnames(wosis_sel)[colnames(wosis_sel) == 'X'] <- 'Longitude'
colnames(wosis_sel)[colnames(wosis_sel) == 'Y'] <- 'Latitude'
colnames(wosis_sel)[colnames(wosis_sel) == 'orgc_value_avg'] <- 'SOC_gcm2'
colnames(wosis_sel)[colnames(wosis_sel) == 'layer_name'] <- 'soil_horizon'
colnames(wosis_sel)[colnames(wosis_sel) == 'upper_depth'] <- 'layer_top_cm'
colnames(wosis_sel)[colnames(wosis_sel) == 'lower_depth'] <- 'layer_bottom_cm'
colnames(wosis_sel)[colnames(wosis_sel) == 'profile_layer_id'] <- 'Layer_Name'

ISCN_sel = SOC_all_wgs84_1982 %>% select(c("Latitude", "Longitude", "Month", "[layer_top (cm)]", "layer_bottom_cm", "SOC_gcm2",
                                "Year", "[hzn]", "Layer_Name"))

colnames(ISCN_sel)[colnames(ISCN_sel) == '[hzn]'] <- 'soil_horizon'
colnames(ISCN_sel)[colnames(ISCN_sel) == '[layer_top (cm)]'] <- 'layer_top_cm'

all_SOC_obs = rbind(ISCN_sel, wosis_sel)
all_SOC_obs$lat = all_SOC_obs$Latitude
all_SOC_obs$lon = all_SOC_obs$Longitude


######################################################
#####Next, average layers to get profiles

##See raca_clean.R 
######################################################

# zips = read.csv("C:/Users/User/OneDrive - University of Tennessee/Documents/Dissertation Work/My Work/FAA/Data/US Zip Codes from 2013 Government Data.txt")
# TN_samples = read_excel("C:/Users/User/OneDrive - University of Tennessee/Documents/Dissertation Work/My Work/FAA/Data/Total Carbon Query.xlsx")
# colnames(TN_samples)[colnames(TN_samples) == 'Zipcode'] <- 'ZIP'
# TN_samples$ZIP = as.numeric(TN_samples$ZIP)
# zips$ZIP = as.numeric(zips$ZIP)
# 
# TN_samples_zip = merge(TN_samples, zips, by = "ZIP")


remnant = read_excel("C:/Users/User/OneDrive - University of Tennessee/Documents/Dissertation Work/My Work/FAA/Data/remnant native SOC database for release sites.xlsx")
sites_US = remnant[remnant$Country == "USA",]

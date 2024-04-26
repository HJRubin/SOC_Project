library(ggplot2)
library(sp)
library(sf)
library(rgdal)
library(terra)
library(readxl)
library(tidyr)
library(dplyr)
library(aqp)
setwd("~/R/SOC")

################WOSIS#########################################################
wosis = read.csv("wosis_latest/wosis_latest_orgc.csv")
wosis_us = wosis[wosis$country_name == "United States of America",]
wosis_us$orgc_value_avg_gcm3 = wosis_us$orgc_value_avg / 10

bd = read.csv("wosis_latest/wosis_latest_bdfi33.csv")
bd_us = bd[bd$country_name == "United States of America",]

wosis_bd = merge(wosis_us, bd_us, by = c("X", "Y", "upper_depth", "lower_depth", "layer_name"))

# plot(wosis_us$X, wosis_us$Y)

wosis_us_surf_date = separate_wider_delim(wosis_bd, cols = orgc_date, delim = ",", 
                                          names = c("Date1", "Date2"), too_few = "debug", 
                                          too_many = "debug")
wosis_us_surf_date_1 = separate_wider_delim(wosis_us_surf_date, cols = Date1, delim = ":", 
                                            names = c("num", "obs_date"), too_few = "debug", 
                                            too_many = "debug")
wosis_us_surf_date_2 = separate_wider_delim(wosis_us_surf_date_1, cols = obs_date, delim = "}", 
                                            names = c("full_date", "ex"), too_few = "debug", 
                                            too_many = "debug")
wosis_us_surf_date_2$full_date = as.Date(wosis_us_surf_date_2$full_date)

wosis_us_surf_1982 = wosis_us_surf_date_2[wosis_us_surf_date_2$full_date > 1982,]
wosis_us_surf_1982$Year = format(as.Date(wosis_us_surf_1982$full_date), "%Y")
wosis_us_surf_1982 = wosis_us_surf_1982[!is.na(wosis_us_surf_1982$full_date), ]

wosis_sel = wosis_us_surf_1982 %>% select(c("X", "Y", "profile_layer_id.x", "upper_depth", "lower_depth", 
                                            "orgc_value_avg_gcm3", "bdfi33_value_avg", "Year"))

colnames(wosis_sel)[colnames(wosis_sel) == 'X'] <- 'Longitude'
colnames(wosis_sel)[colnames(wosis_sel) == 'Y'] <- 'Latitude'
colnames(wosis_sel)[colnames(wosis_sel) == 'upper_depth'] <- 'layer_top_cm'
colnames(wosis_sel)[colnames(wosis_sel) == 'lower_depth'] <- 'layer_bottom_cm'
colnames(wosis_sel)[colnames(wosis_sel) == 'profile_layer_id.x'] <- 'Layer_Name'

# wosis_sel$SOC_gcm3 = wosis_sel$orgc_value_avg_gcm3 * wosis_sel$bdfi33_value_avg *
#   (wosis_sel$layer_bottom_cm - wosis_sel$layer_top_cm)

wosis_sel$SOC_kgm2 = wosis_sel$orgc_value_avg_gcm3 / (wosis_sel$layer_bottom_cm - wosis_sel$layer_top_cm)

wosis_conus = wosis_sel[wosis_sel$Longitude >= -125,]
wosis_conus = wosis_conus %>% select(-c("orgc_value_avg_gcm3", "bdfi33_value_avg"))
plot(wosis_conus$Year, wosis_conus$SOC_kgm2)
# plot(wosis_conus$Year, wosis_conus$orgc_value_avg_gcm3)

#g/cm3 to g/cm2
# wosis_sel_tot = wosis_sel %>% select(-c("orgc_value_avg_gcm3"))
# wosis_sel_tot$LULC = NA

#############RACA###########################################################
raca = read.csv("RaCA_samples.csv")
raca_locs = read.csv("RaCa_general_location.csv")

raca_match = merge(raca, raca_locs, by.x ="rcasiteid", by.y = "RaCA_Id")

# plot(raca_match$Gen_long, raca_match$Gen_lat)

raca_match$Year = 2010

raca_match_sel = raca_match %>% dplyr::select(c("samp", "TOP", "BOT", "SOC_pred1", 
                                                 "Year", "Gen_long", "Gen_lat"))

colnames(raca_match_sel)[colnames(raca_match_sel) == 'Gen_long'] <- 'Longitude'
colnames(raca_match_sel)[colnames(raca_match_sel) == 'Gen_lat'] <- 'Latitude'
# colnames(raca_match_sel)[colnames(raca_match_sel) == 'SOC_pred1'] <- 'SOC_gcm2'
colnames(raca_match_sel)[colnames(raca_match_sel) == 'samp'] <- 'Layer_Name'
colnames(raca_match_sel)[colnames(raca_match_sel) == 'TOP'] <- 'layer_top_cm'
colnames(raca_match_sel)[colnames(raca_match_sel) == 'BOT'] <- 'layer_bottom_cm'

raca_match_sel = raca_match_sel[!is.na(raca_match_sel$SOC_pred1),]
raca_match_sel$LULC = NA

raca_match_sel$SOC_kgm2 = raca_match_sel$SOC_pred1 / 10
raca_match_sel = raca_match_sel %>% select(-c(SOC_pred1))

plot(raca_match_sel$Year, raca_match_sel$SOC_kgm2)
plot(raca_match_sel$Longitude, raca_match_sel$Latitude)

#################ISCN###############################################
setwd("~/Dissertation Work/My Work/FAA/Data")
soc = readxl::read_xlsx("ISCN_layers_072023.xlsx")

soc_complete = soc[!is.na(soc$`[soc (g cm-2)]`),]

SOC_nad27 = soc_complete[soc_complete$`[datum (datum)]` == "NAD27",]
SOC_nad83 = soc_complete[soc_complete$`[datum (datum)]` == "NAD83",]
SOC_wgs84 = soc_complete[soc_complete$`[datum (datum)]` == "WGS84",]
SOC_nad27$Longitude = SOC_nad27$`[long (dec. deg)]`
SOC_nad27$Latitude = SOC_nad27$`[lat (dec. deg)]`
SOC_nad83$Longitude = SOC_nad83$`[long (dec. deg)]`
SOC_nad83$Latitude = SOC_nad83$`[lat (dec. deg)]`
SOC_wgs84$Latitude = SOC_wgs84$`[lat (dec. deg)]`
SOC_wgs84$Longitude = SOC_wgs84$`[long (dec. deg)]`
SOC_wgs84 = SOC_wgs84[SOC_wgs84$`[site_name]` != "S08CI007004",]

coordinates(SOC_nad27) = ~Longitude+Latitude
coordinates(SOC_nad83) = ~Longitude+Latitude

proj4string(SOC_nad83) = CRS("+init=epsg:4269")
proj4string(SOC_nad27) = CRS("+init=epsg:4267")

SOC_nad83_vect = vect(SOC_nad83)
SOC_nad27_vect = vect(SOC_nad27)

SOC_nad83_wgs84 = terra::project(SOC_nad83_vect, "EPSG:4326")
SOC_nad27_wgs84 = terra::project(SOC_nad27_vect, "EPSG:4326")

SOC_nad83_df = as.data.frame(SOC_nad83_wgs84)
SOC_nad83_df$Latitude = SOC_nad83_df$`[lat (dec. deg)]`
SOC_nad83_df$Longitude = SOC_nad83_df$`[long (dec. deg)]`
SOC_nad27_df = as.data.frame(SOC_nad27_wgs84)
SOC_nad27_df$Latitude = SOC_nad27_df$`[lat (dec. deg)]`
SOC_nad27_df$Longitude = SOC_nad27_df$`[long (dec. deg)]`

SOC_all_wgs84 = rbind(SOC_wgs84, SOC_nad27_df, SOC_nad83_df)

SOC_all_wgs84_1982 = SOC_all_wgs84[SOC_all_wgs84$`[observation_date (YYYY-MM-DD)]` > 1982,]
SOC_all_wgs84_1982 = SOC_all_wgs84_1982[!is.na(SOC_all_wgs84_1982$Latitude), ]
SOC_all_wgs84_1982 = SOC_all_wgs84_1982[!is.na(SOC_all_wgs84_1982$Longitude), ]
SOC_all_wgs84_1982$Year = format(as.Date(SOC_all_wgs84_1982$`[observation_date (YYYY-MM-DD)]`), "%Y")
SOC_all_wgs84_1982$Month = format(as.Date(SOC_all_wgs84_1982$`[observation_date (YYYY-MM-DD)]`), "%m")
SOC_all_wgs84_1982 = SOC_all_wgs84_1982[!is.na(SOC_all_wgs84_1982$Year), ]
SOC_all_wgs84_1982 = SOC_all_wgs84_1982[!is.na(SOC_all_wgs84_1982$`[soc_carbon_flag]`), ]

c = SOC_all_wgs84_1982[SOC_all_wgs84_1982$Year == 2008 & SOC_all_wgs84_1982$`[dataset_name_sub]` == "NRCS Sept/2014"
                       & SOC_all_wgs84_1982$Longitude >=-73 & SOC_all_wgs84_1982$Latitude <= 40,]
SOC_all_wgs84_1982_1 = SOC_all_wgs84_1982 %>% anti_join(c)

colnames(SOC_all_wgs84_1982_1)[colnames(SOC_all_wgs84_1982_1) == '[soc (g cm-2)]'] <- 'SOC_gcm2'
colnames(SOC_all_wgs84_1982_1)[colnames(SOC_all_wgs84_1982_1) == '[layer_name]'] <- 'Layer_Name'
colnames(SOC_all_wgs84_1982_1)[colnames(SOC_all_wgs84_1982_1) == '[site_name]'] <- 'Site_Name'
colnames(SOC_all_wgs84_1982_1)[colnames(SOC_all_wgs84_1982_1) == '[layer_bot (cm)]'] <- 'layer_bottom_cm'

ISCN_sel = SOC_all_wgs84_1982_1 %>% select(c("Latitude", "Longitude", "[layer_top (cm)]", 
                                           "layer_bottom_cm", "SOC_gcm2",
                                           "Year", "Layer_Name"))

colnames(ISCN_sel)[colnames(ISCN_sel) == '[layer_top (cm)]'] <- 'layer_top_cm'
ISCN_sel$LULC = NA

ISCN_sel = ISCN_sel[ISCN_sel$Longitude >= -130 & ISCN_sel$Longitude <= -65,]
ISCN_sel = ISCN_sel[ISCN_sel$Latitude <= 50,]
ISCN_sel$SOC_kgm2 = ISCN_sel$SOC_gcm2 / 10

# a = ISCN_sel[ISCN_sel$Longitude >=-73 & ISCN_sel$Latitude <= 40, ]

# plot(a$Longitude, a$Latitude)
# plot(b$Longitude, b$Latitude)
ISCN_sel = ISCN_sel %>% select(-c(SOC_gcm2))

plot(ISCN_sel$Longitude, ISCN_sel$Latitude)
plot(ISCN_sel$Year, ISCN_sel$SOC_kgm2)

###########PNAS###############################################################
PNAS  = read_excel("remnant native SOC database for release sites.xlsx")

PNAS_sel = PNAS %>% select(c("Site ID", "Year", "Latitude", "Longitude", "30 cm SOC", "LULC category"))

colnames(PNAS_sel)[colnames(PNAS_sel) == '30 cm SOC'] <- 'SOC_gcm2'
colnames(PNAS_sel)[colnames(PNAS_sel) == 'Site ID'] <- 'Layer_Name'
colnames(PNAS_sel)[colnames(PNAS_sel) == 'LULC category'] <- 'LULC'
PNAS_sel$layer_bottom_cm = 30
PNAS_sel$layer_top_cm = 0

PNAS_sel$SOC_kgm2 = PNAS_sel$SOC_gcm2 / 10 #kg/m2 to g/cm2
PNAS_sel = PNAS_sel %>% select(-c("SOC_gcm2"))

points(PNAS_sel$Longitude, PNAS_sel$Latitude, col = "red")
plot(PNAS_sel$Year, PNAS_sel$SOC_kgm2)

###########Nature###############################################################
nature = read_excel("SOC perennials DATABASE sites.xlsx")

nature_sel = nature %>% select(c("plotID", "year_measure", "Latitud", "Longitud", "soil_from_cm_current", 
                                 "soil_to_cm_current", "SOC_Mg_ha_current", "current_land_use*"))

colnames(nature_sel)[colnames(nature_sel) == 'soil_from_cm_current'] <- 'layer_top_cm'
colnames(nature_sel)[colnames(nature_sel) == 'soil_to_cm_current'] <- 'layer_bottom_cm'
colnames(nature_sel)[colnames(nature_sel) == 'year_measure'] <- 'Year'
colnames(nature_sel)[colnames(nature_sel) == 'Latitud'] <- 'Latitude'
colnames(nature_sel)[colnames(nature_sel) == 'Longitud'] <- 'Longitude'
colnames(nature_sel)[colnames(nature_sel) == 'plotID'] <- 'Layer_Name'
colnames(nature_sel)[colnames(nature_sel) == 'current_land_use*'] <- 'LULC'

nature_sel$SOC_Mg_ha_current = as.numeric(nature_sel$SOC_Mg_ha_current)
nature_sel$Year = as.numeric(nature_sel$Year)

nat_mg_ha = nature_sel[!is.na(nature_sel$SOC_Mg_ha_current), ]
nat_mg_ha = nat_mg_ha[!is.na(nat_mg_ha$Year), ]
nat_mg_ha$SOC_kgm2 = as.numeric(nat_mg_ha$SOC_Mg_ha_current) / 1e+10 #mg/ha to g/cm2

nature_sel_tot = nat_mg_ha %>% select(-c("SOC_Mg_ha_current",))
                                 
points(nature_sel_tot$Longitude, nature_sel_tot$Latitude, col = "blue")
plot(nature_sel_tot$Year, nature_sel_tot$SOC_kgm2)

#############SoDaH##############################################################
SoDaH	= read.csv("SoDaH_US_only.csv")

# sodah_us = SoDaH[SoDaH$long < -50 & SoDaH$lat > 20, ]

# write.csv(sodah_us, "SoDaH_US_only.csv")

sodah_sel = SoDaH %>% select(c("new_date", "lat", "long", "layer_bot",
                                 "layer_top", "lyr_soc", "site_code", "eco_region"))

colnames(sodah_sel)[colnames(sodah_sel) == 'long'] <- 'Longitude'
colnames(sodah_sel)[colnames(sodah_sel) == 'lat'] <- 'Latitude'
colnames(sodah_sel)[colnames(sodah_sel) == 'layer_bot'] <- 'layer_bottom_cm'
colnames(sodah_sel)[colnames(sodah_sel) == 'layer_top'] <- 'layer_top_cm'
colnames(sodah_sel)[colnames(sodah_sel) == 'site_code'] <- 'Layer_Name'
colnames(sodah_sel)[colnames(sodah_sel) == 'lyr_soc'] <- 'SOC_gcm2'
colnames(sodah_sel)[colnames(sodah_sel) == 'eco_region'] <- 'LULC'
colnames(sodah_sel)[colnames(sodah_sel) == 'new_date'] <- 'Year'

sodah_sel = sodah_sel[!is.na(sodah_sel$Latitude), ]
sodah_sel = sodah_sel[!is.na(sodah_sel$SOC_gcm2), ]
sodah_sel$SOC_kgm2 = sodah_sel$SOC_gcm2 / 10

sodah_sel = sodah_sel %>% select(-c(SOC_gcm2))

points(sodah_sel$Longitude, sodah_sel$Latitude, col = "orange")
plot(sodah_sel$Year, sodah_sel$SOC_kgm2)

#######Harmonize#####################################################################

wosis_conus$Dataset = "WOSIS" #31740
PNAS_sel$Dataset = "PNAS Paper"  #172
sodah_sel$Dataset = "SoDaH" #21830
nature_sel_tot$Dataset = "Nature Paper" #Not sure about units or values here - maybe for land use
ISCN_sel$Dataset = "ISCN" #83798
raca_match_sel$Dataset = "RACA" #144554

# sodah_sel_agg = aggregate(SOC_kgm2 ~ Latitude + Longitude + Year + Layer_Name + 
#                             LULC + layer_top_cm + layer_bottom_cm + Dataset,
#                           data = sodah_sel, FUN = mean)
# ISCN_agg = aggregate(SOC_kgm2 ~ Latitude + Longitude + Year + Layer_Name + 
#                             layer_top_cm + layer_bottom_cm + Dataset,
#                           data = ISCN_sel, FUN = mean)
# raca_agg = aggregate(SOC_kgm2 ~ Latitude + Longitude + Year + Layer_Name + 
#                        layer_top_cm + layer_bottom_cm + Dataset,
#                      data = raca_match_sel, FUN = mean)

# ISCN_agg$Dataset = "ISCN"
ISCN_sel$LULC = "None"
# raca_agg$Dataset = "RACA"
raca_match_sel$LULC = "None"
wosis_conus$LULC  = "None"

all_soils = rbind(wosis_conus, ISCN_sel, raca_match_sel, PNAS_sel, sodah_sel) #, nature_sel_tot) #282094
all_soils1 = all_soils[all_soils$Longitude > -130, ] #279862

ggplot(all_soils1, aes(Longitude, Latitude, color = Dataset)) +
  geom_point(alpha = 0.1) + 
  theme_bw() + 
  scale_color_manual(values = c("darkblue", "darkgreen", "orange", "pink", "brown"))

ggplot(all_soils, aes(SOC_kgm2, fill = Dataset)) +
  geom_histogram(aes(y =after_stat(density))) + 
  theme_bw() + 
  facet_wrap("Dataset")  
  # xlim(0,100)


all_soils_ID = transform(all_soils1, Cluster_ID = 
                     match(paste0(Latitude, Longitude, Year, LULC, Dataset),
                           unique(paste0(Latitude, Longitude, Year, LULC, Dataset))))

all_soils_ID_complete = all_soils_ID[!is.na(all_soils_ID$layer_top_cm), ] #278196
all_soils_ID_complete = all_soils_ID_complete[!is.na(all_soils_ID_complete$layer_bottom_cm), ] #277776

all_soils_30 = all_soils_ID_complete[all_soils_ID_complete$layer_top_cm < 30,] #153497

depths(all_soils_30) <- Cluster_ID ~ layer_top_cm + layer_bottom_cm
all_soils_30$thickness = all_soils_30$layer_bottom_cm - all_soils_30$layer_top_cm

# s1 = all_soils_30[1:10]
# plotSPC(s1, color = 'SOC_kgm2')

s <- dice(all_soils_30, fm=0:30 ~ .)
s2 = as(s, 'data.frame')
s2_30 = s2[s2$layer_bottom_cm <= 30, ]

soils_agg = aggregate(SOC_kgm2 ~ Latitude + Longitude + Year + Cluster_ID ,
                      s2_30, FUN=mean)

# write.csv(soils_agg, "all5_soils_SOC_30cm_kgm2_03212024.csv")



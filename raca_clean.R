setwd("~/R/SOC")
library(aqp)

# raca = read.csv("RaCA_SOC_pedons.csv")
raca = read.csv("RaCA_samples.csv")
raca_locs = read.csv("RaCa_general_location.csv")

raca_match = merge(raca, raca_locs, by.x ="rcasiteid", by.y = "RaCA_Id")

# raca_match$SOCstock30_gcm2 = raca_match$SOCstock30  * 1000

raca_match$Year = 2010
raca_match$Month = 6

hist(raca_match$SOC_pred1)
# hist(wosis_us$orgc_value_avg_gcm3)
# hist(SOC_all_wgs84_1982$SOC_gcm2)

nwca11 = read.csv("nwca2011_soilchem.csv")
nwca16 = read.csv("nwca_2016_soil_horizon_chemistry_-_data_csv.csv")

min(SOC_all_wgs84_1982$SOC_gcm2)
max(SOC_all_wgs84_1982$SOC_gcm2)
min(wosis_us$orgc_value_avg_gcm3)
max(wosis_us$orgc_value_avg_gcm3)
min(raca_match$SOC_pred1, na.rm = TRUE)
max(raca_match$SOC_pred1, na.rm = TRUE)

raca_match_sel = raca_match %>% dplyr::select(c("samp", "TOP", "BOT", "SOC_pred1", 
                                                "hzn_desgn",
                                            "Month", "Year", "Gen_long", "Gen_lat"))

colnames(raca_match_sel)[colnames(raca_match_sel) == 'Gen_long'] <- 'Longitude'
colnames(raca_match_sel)[colnames(raca_match_sel) == 'Gen_lat'] <- 'Latitude'
colnames(raca_match_sel)[colnames(raca_match_sel) == 'SOC_pred1'] <- 'SOC_gcm2'
colnames(raca_match_sel)[colnames(raca_match_sel) == 'samp'] <- 'Layer_Name'
colnames(raca_match_sel)[colnames(raca_match_sel) == 'hzn_desgn'] <- 'soil_horizon'
colnames(raca_match_sel)[colnames(raca_match_sel) == 'TOP'] <- 'layer_top_cm'
colnames(raca_match_sel)[colnames(raca_match_sel) == 'BOT'] <- 'layer_bottom_cm'

# ISCN_sel = SOC_all_wgs84_1982 %>% select(c("Latitude", "Longitude", "Month",
# "[layer_top (cm)]", "layer_bottom_cm", "SOC_gcm2",
#                                            "Year", "[hzn]", "Layer_Name"))


wri = rbind(wosis_sel, ISCN_sel, raca_match_sel)
wri_ID = transform(wri, Cluster_ID = 
                     match(paste0(Latitude, Longitude, Year, Month),
                           unique(paste0(Latitude, Longitude, Year, Month))))
wri_complete = wri_ID[!is.na(wri_ID$layer_top_cm), ]
wri_complete = wri_complete[!is.na(wri_complete$layer_bottom_cm), ]

wri_30 = wri_complete[wri_complete$layer_top_cm < 30,]


depths(wri_30) <- Cluster_ID ~ layer_top_cm + layer_bottom_cm
hzdesgnname(wri_30) <- "soil_horizon"
wri_30$thickness = wri_30$layer_bottom_cm - wri_30$layer_top_cm

coordinates(wri_30) = ~ Latitude + Longitude
logiccheck = checkHzDepthLogic(wri_30)
logiccheck = logiccheck[logiccheck$valid == TRUE,]
wri_30_good = wri_30[wri_30$Cluster_ID %in% logiccheck$Cluster_ID, ]

# s1 = wri_30_good[1:10]
# plotSPC(s1, color = 'SOC_gcm2')

s <- dice(wri_30_good, fm=0:30 ~ .)

s2 = as(s, 'data.frame')

s2_30 = s2[s2$layer_bottom_cm <= 30, ]

s2_30$hzID = as.numeric(s2_30$hzID)
wri_unique_profiles = s2_30[!duplicated(s2_30[,
                          c("Latitude", "Longitude", "Year","SOC_gcm2", "hzID")]),]


wri_agg = aggregate(SOC_gcm2 ~ Latitude + Longitude + Year + Month + Cluster_ID , 
                    wri_unique_profiles, FUN=sum)

wri_agg = wri_agg[wri_agg$SOC_gcm2 > 0, ]

write.csv(wri_agg, "wosis_raca_iscn_soil_30cm.csv")

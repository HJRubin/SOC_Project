setwd("~/Dissertation Work/My Work/FAA/Data/")

tmax5 = rast("wc2.1_30s_tmax/wc2.1_30s_tmax_05.tif")
tmax6 = rast("wc2.1_30s_tmax/wc2.1_30s_tmax_06.tif")
tmax7 = rast("wc2.1_30s_tmax/wc2.1_30s_tmax_07.tif")
tmax8 = rast("wc2.1_30s_tmax/wc2.1_30s_tmax_08.tif")
tmax9 = rast("wc2.1_30s_tmax/wc2.1_30s_tmax_09.tif")
tmax10 = rast("wc2.1_30s_tmax/wc2.1_30s_tmax_10.tif")

tmin5 = rast("wc2.1_30s_tmin/wc2.1_30s_tmin_05.tif")
tmin6 = rast("wc2.1_30s_tmin/wc2.1_30s_tmin_06.tif")
tmin7 = rast("wc2.1_30s_tmin/wc2.1_30s_tmin_07.tif")
tmin8 = rast("wc2.1_30s_tmin/wc2.1_30s_tmin_08.tif")
tmin9 = rast("wc2.1_30s_tmin/wc2.1_30s_tmin_09.tif")
tmin10 = rast("wc2.1_30s_tmin/wc2.1_30s_tmin_10.tif")

tavg5 = rast("wc2.1_30s_tavg/wc2.1_30s_tavg_05.tif")
tavg6 = rast("wc2.1_30s_tavg/wc2.1_30s_tavg_06.tif")
tavg7 = rast("wc2.1_30s_tavg/wc2.1_30s_tavg_07.tif")
tavg8 = rast("wc2.1_30s_tavg/wc2.1_30s_tavg_08.tif")
tavg9 = rast("wc2.1_30s_tavg/wc2.1_30s_tavg_09.tif")
tavg10 = rast("wc2.1_30s_tavg/wc2.1_30s_tavg_10.tif")

prec5 = rast("wc2.1_30s_prec/wc2.1_30s_prec_05.tif")
prec6 = rast("wc2.1_30s_prec/wc2.1_30s_prec_06.tif")
prec7 = rast("wc2.1_30s_prec/wc2.1_30s_prec_07.tif")
prec8 = rast("wc2.1_30s_prec/wc2.1_30s_prec_08.tif")
prec9 = rast("wc2.1_30s_prec/wc2.1_30s_prec_09.tif")
prec10 = rast("wc2.1_30s_prec/wc2.1_30s_prec_10.tif")

setwd("~/R/SOC")
full_df_ext_m = read_excel("df_from_arc_SOC.xlsx")
full_df_summer = full_df_ext_month[as.numeric(full_df_ext_month$Month.y) < 11 & as.numeric(full_df_ext_month$Month.y) > 4,]
full_df_summer$Month.y = as.numeric(full_df_summer$Month.y)

df5 = vect(full_df_summer[full_df_summer$Month.y == 5, ], geom = c("longitude", "latitude"))
df6 = vect(full_df_summer[full_df_summer$Month.y == 6, ], geom = c("longitude", "latitude"))
df7 = vect(full_df_summer[full_df_summer$Month.y == 7, ], geom = c("longitude", "latitude"))
df8 = vect(full_df_summer[full_df_summer$Month.y == 8, ], geom = c("longitude", "latitude"))
df9 = vect(full_df_summer[full_df_summer$Month.y == 9, ], geom = c("longitude", "latitude"))
df10 = vect(full_df_summer[full_df_summer$Month.y == 10, ], geom = c("longitude", "latitude"))

stack5 = c(tmax5, tmin5, tavg5, prec5)
stack6 = c(tmax6, tmin6, tavg6, prec6)
stack7 = c(tmax7, tmin7, tavg7, prec7)
stack8 = c(tmax8, tmin8, tavg8, prec8)
stack9 = c(tmax9, tmin9, tavg9, prec9)
stack10 = c(tmax10, tmin10, tavg10, prec10)

df5_m = terra::extract(stack5, df5)
df6_m = terra::extract(stack6, df6)
df7_m = terra::extract(stack7, df7)
df8_m = terra::extract(stack8, df8)
df9_m = terra::extract(stack9, df9)
df10_m = terra::extract(stack10, df10)

df5_clim = merge(df5_m, df5, by = "ID")
df6_clim = merge(df6_m, df6, by = "ID")
df7_clim = merge(df7_m, df7, by = "ID")
df8_clim = merge(df8_m, df8, by = "ID")
df9_clim = merge(df9_m, df9, by = "ID")
df10_clim = merge(df10_m, df10, by = "ID")

colnames(df5_clim)[colnames(df5_clim) == 'wc2.1_30s_tmax_05'] <- 'wc2.1_30s_tmax'
colnames(df5_clim)[colnames(df5_clim) == 'wc2.1_30s_tmin_05'] <- 'wc2.1_30s_tmin'
colnames(df5_clim)[colnames(df5_clim) == 'wc2.1_30s_tavg_05'] <- 'wc2.1_30s_tavg'
colnames(df5_clim)[colnames(df5_clim) == 'wc2.1_30s_prec_05'] <- 'wc2.1_30s_prec'
colnames(df6_clim)[colnames(df6_clim) == 'wc2.1_30s_tmax_06'] <- 'wc2.1_30s_tmax'
colnames(df6_clim)[colnames(df6_clim) == 'wc2.1_30s_tmin_06'] <- 'wc2.1_30s_tmin'
colnames(df6_clim)[colnames(df6_clim) == 'wc2.1_30s_tavg_06'] <- 'wc2.1_30s_tavg'
colnames(df6_clim)[colnames(df6_clim) == 'wc2.1_30s_prec_06'] <- 'wc2.1_30s_prec'
colnames(df7_clim)[colnames(df7_clim) == 'wc2.1_30s_tmax_07'] <- 'wc2.1_30s_tmax'
colnames(df7_clim)[colnames(df7_clim) == 'wc2.1_30s_tmin_07'] <- 'wc2.1_30s_tmin'
colnames(df7_clim)[colnames(df7_clim) == 'wc2.1_30s_tavg_07'] <- 'wc2.1_30s_tavg'
colnames(df7_clim)[colnames(df7_clim) == 'wc2.1_30s_prec_07'] <- 'wc2.1_30s_prec'
colnames(df8_clim)[colnames(df8_clim) == 'wc2.1_30s_tmax_08'] <- 'wc2.1_30s_tmax'
colnames(df8_clim)[colnames(df8_clim) == 'wc2.1_30s_tmin_08'] <- 'wc2.1_30s_tmin'
colnames(df8_clim)[colnames(df8_clim) == 'wc2.1_30s_tavg_08'] <- 'wc2.1_30s_tavg'
colnames(df8_clim)[colnames(df8_clim) == 'wc2.1_30s_prec_08'] <- 'wc2.1_30s_prec'
colnames(df9_clim)[colnames(df9_clim) == 'wc2.1_30s_tmax_09'] <- 'wc2.1_30s_tmax'
colnames(df9_clim)[colnames(df9_clim) == 'wc2.1_30s_tmin_09'] <- 'wc2.1_30s_tmin'
colnames(df9_clim)[colnames(df9_clim) == 'wc2.1_30s_tavg_09'] <- 'wc2.1_30s_tavg'
colnames(df9_clim)[colnames(df9_clim) == 'wc2.1_30s_prec_09'] <- 'wc2.1_30s_prec'
colnames(df10_clim)[colnames(df10_clim) == 'wc2.1_30s_tmax_10'] <- 'wc2.1_30s_tmax'
colnames(df10_clim)[colnames(df10_clim) == 'wc2.1_30s_tmin_10'] <- 'wc2.1_30s_tmin'
colnames(df10_clim)[colnames(df10_clim) == 'wc2.1_30s_tavg_10'] <- 'wc2.1_30s_tavg'
colnames(df10_clim)[colnames(df10_clim) == 'wc2.1_30s_prec_10'] <- 'wc2.1_30s_prec'

full_df_clim = rbind(df5_clim, df6_clim, df7_clim, df8_clim, df9_clim, df10_clim)

# write.csv(full_df_clim, "full_df_clim_08012023.csv")


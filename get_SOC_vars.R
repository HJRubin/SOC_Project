#get landsat ndvi mean, min, max
library(terra)
#LS5 - 1985 - 2011
#LS7 - 1999 - 2018
#LS4 - 1983, 1987:1992

setwd("~/Dissertation Work/My Work/FAA/Data/")
# soils_agg = read.csv("all_soils_SOC_30cm.csv")
soils_agg = read.csv("all5_soils_SOC_30cm_kgm2_03212024.csv")
soils_agg$lon = soils_agg$Longitude
soils_agg$lat = soils_agg$Latitude
set_SOC_name = function(years){
  setNames(list(as.data.frame(soils_agg[soils_agg$Year == years,])), paste0("SOC", years))
}
SOC_list = lapply(1983:2018, set_SOC_name)
###############HERE 9/12
vect_SOC = function(list1){
  # print(names(list1[1]))
  vect(as.data.frame(list1[1]), geom = c(names(as.data.frame(list1[1]))[3],names(as.data.frame(list1[1]))[2]))
}
# output = list()
SOC_vects = lapply(SOC_list, vect_SOC)

setwd("~/Dissertation Work/My Work/FAA/Data/Landsat_full_yrs_NDVI")

rast_landsat = function(years, satellite){
  setNames(list(rast(paste0(satellite,years,"_full.tif"))), paste0(satellite, years))
  # setNames(list(rast(paste0(satellite,years,".tif"))), paste0(satellite, years))
}
ls5 = lapply(1987:1998, rast_landsat, satellite = "wgs_landsat5_")
ls7 = lapply(1999:2022, rast_landsat, satellite = "wgs_landsat7_")
ls4 = lapply(1983, rast_landsat, satellite = "wgs_landsat4_")

setwd("~/Dissertation Work/My Work/FAA/Data/Landsat_tmax_tmin_NDVI")

ls5_max = lapply(1984:1998, rast_landsat, satellite = "landsat5_ndvi_95_")
ls7_max = lapply(1999:2022, rast_landsat, satellite = "landsat7_ndvi_95_")
ls4_max = lapply(1983, rast_landsat, satellite = "landsat4_ndvi_95_")
ls5_min = lapply(1984:1998, rast_landsat, satellite = "landsat5_ndvi_5_")
ls7_min = lapply(1999:2022, rast_landsat, satellite = "landsat7_ndvi_5_")
ls4_min = lapply(1983, rast_landsat, satellite = "landsat4_ndvi_5_")

#Extract points from landsat pixel

SOC_ls4 = terra::extract(rast(ls4[[1]]), SOC_vects[[1]])
SOC_ls4_min = terra::extract(rast(ls4_min[[1]]), SOC_vects[[1]])
SOC_ls4_max = terra::extract(rast(ls4_max[[1]]), SOC_vects[[1]])

#This gets year 0 and previous 5 years for 1992-1998
output_ls5_ndvi = data.frame(matrix(nrow = 0, ncol = 4))
colnames(output_ls5_ndvi) = c("ID", "NDVI_mean", "NDVI_max", "NDVI_min")
for (i in 6:12){
  SOC_ls5 = terra::extract(rast(ls5[[i]]), SOC_vects[[i+4]])
  SOC_ls5_y1 = terra::extract(rast(ls5[[i-1]]), SOC_vects[[i+4]])
  SOC_ls5_y2 = terra::extract(rast(ls5[[i-2]]), SOC_vects[[i+4]])
  SOC_ls5_y3 = terra::extract(rast(ls5[[i-3]]), SOC_vects[[i+4]])
  SOC_ls5_y4 = terra::extract(rast(ls5[[i-4]]), SOC_vects[[i+4]])
  SOC_ls5_y5 = terra::extract(rast(ls5[[i-5]]), SOC_vects[[i+4]])
  
  SOC_ls5_max = terra::extract(rast(ls5_max[[i]]), SOC_vects[[i+4]])
  SOC_ls5_max_y1 = terra::extract(rast(ls5_max[[i-1]]), SOC_vects[[i+4]])
  SOC_ls5_max_y2 = terra::extract(rast(ls5_max[[i-2]]), SOC_vects[[i+4]])
  SOC_ls5_max_y3 = terra::extract(rast(ls5_max[[i-3]]), SOC_vects[[i+4]])
  SOC_ls5_max_y4 = terra::extract(rast(ls5_max[[i-4]]), SOC_vects[[i+4]])
  SOC_ls5_max_y5 = terra::extract(rast(ls5_max[[i-5]]), SOC_vects[[i+4]])
  
  SOC_ls5_min = terra::extract(rast(ls5_min[[i]]), SOC_vects[[i+4]])
  SOC_ls5_min_y1 = terra::extract(rast(ls5_min[[i-1]]), SOC_vects[[i+4]])
  SOC_ls5_min_y2 = terra::extract(rast(ls5_min[[i-2]]), SOC_vects[[i+4]])
  SOC_ls5_min_y3 = terra::extract(rast(ls5_min[[i-3]]), SOC_vects[[i+4]])
  SOC_ls5_min_y4 = terra::extract(rast(ls5_min[[i-4]]), SOC_vects[[i+4]])
  SOC_ls5_min_y5 = terra::extract(rast(ls5_min[[i-5]]), SOC_vects[[i+4]])

  ids = as.data.frame(SOC_ls5$ID)
  ids$NDVI_mean = SOC_ls5$NDVI
  ids$NDVI_mean_y1 = SOC_ls5_y1$NDVI
  ids$NDVI_mean_y2 = SOC_ls5_y2$NDVI
  ids$NDVI_mean_y3 = SOC_ls5_y3$NDVI
  ids$NDVI_mean_y4 = SOC_ls5_y4$NDVI
  ids$NDVI_mean_y5 = SOC_ls5_y5$NDVI

  ids$NDVI_max = SOC_ls5_max$NDVI
  ids$NDVI_max_y1 = SOC_ls5_max_y1$NDVI
  ids$NDVI_max_y2 = SOC_ls5_max_y2$NDVI
  ids$NDVI_max_y3 = SOC_ls5_max_y3$NDVI
  ids$NDVI_max_y4 = SOC_ls5_max_y4$NDVI
  ids$NDVI_max_y5 = SOC_ls5_max_y5$NDVI

  ids$NDVI_min = SOC_ls5_min$NDVI
  ids$NDVI_min_y1 = SOC_ls5_min_y1$NDVI
  ids$NDVI_min_y2 = SOC_ls5_min_y2$NDVI
  ids$NDVI_min_y3 = SOC_ls5_min_y3$NDVI
  ids$NDVI_min_y4 = SOC_ls5_min_y4$NDVI
  ids$NDVI_min_y5 = SOC_ls5_min_y5$NDVI

  ids$Year = i + 1986
  output_ls5_ndvi = rbind(output_ls5_ndvi, ids)
}

#This gets year 0 and previous 5 years for 2004-2018
output_ls7_ndvi = data.frame(matrix(nrow = 0, ncol = 4))
colnames(output_ls7_ndvi) = c("ID", "NDVI_mean", "NDVI_max", "NDVI_min")
for (i in 6:20){
  SOC_ls7 = terra::extract(rast(ls7[[i]]), SOC_vects[[i+16]])
  SOC_ls7_y1 = terra::extract(rast(ls7[[i-1]]), SOC_vects[[i+16]])
  SOC_ls7_y2 = terra::extract(rast(ls7[[i-2]]), SOC_vects[[i+16]])
  SOC_ls7_y3 = terra::extract(rast(ls7[[i-3]]), SOC_vects[[i+16]])
  SOC_ls7_y4 = terra::extract(rast(ls7[[i-4]]), SOC_vects[[i+16]])
  SOC_ls7_y5 = terra::extract(rast(ls7[[i-5]]), SOC_vects[[i+16]])
  
  SOC_ls7_max = terra::extract(rast(ls7_max[[i-1]]), SOC_vects[[i+16]])
  SOC_ls7_max_y1 = terra::extract(rast(ls7_max[[i-1]]), SOC_vects[[i+16]])
  SOC_ls7_max_y2 = terra::extract(rast(ls7_max[[i-2]]), SOC_vects[[i+16]])
  SOC_ls7_max_y3 = terra::extract(rast(ls7_max[[i-3]]), SOC_vects[[i+16]])
  SOC_ls7_max_y4 = terra::extract(rast(ls7_max[[i-4]]), SOC_vects[[i+16]])
  SOC_ls7_max_y5 = terra::extract(rast(ls7_max[[i-5]]), SOC_vects[[i+16]])
  
  SOC_ls7_min = terra::extract(rast(ls7_min[[i]]), SOC_vects[[i+16]])
  SOC_ls7_min_y1 = terra::extract(rast(ls7_min[[i-1]]), SOC_vects[[i+16]]) 
  SOC_ls7_min_y2 = terra::extract(rast(ls7_min[[i-2]]), SOC_vects[[i+16]])
  SOC_ls7_min_y3 = terra::extract(rast(ls7_min[[i-3]]), SOC_vects[[i+16]])
  SOC_ls7_min_y4 = terra::extract(rast(ls7_min[[i-4]]), SOC_vects[[i+16]])
  SOC_ls7_min_y5 = terra::extract(rast(ls7_min[[i-5]]), SOC_vects[[i+16]])

  ids = as.data.frame(SOC_ls7$ID)

  ids$NDVI_mean = SOC_ls7$NDVI
  ids$NDVI_mean_y1 = SOC_ls7_y1$NDVI
  ids$NDVI_mean_y2 = SOC_ls7_y2$NDVI
  ids$NDVI_mean_y3 = SOC_ls7_y3$NDVI
  ids$NDVI_mean_y4 = SOC_ls7_y4$NDVI
  ids$NDVI_mean_y5 = SOC_ls7_y5$NDVI
  
  ids$NDVI_max = SOC_ls7_max$NDVI
  ids$NDVI_max_y1 = SOC_ls7_max_y1$NDVI
  ids$NDVI_max_y2 = SOC_ls7_max_y2$NDVI
  ids$NDVI_max_y3 = SOC_ls7_max_y3$NDVI
  ids$NDVI_max_y4 = SOC_ls7_max_y4$NDVI
  ids$NDVI_max_y5 = SOC_ls7_max_y5$NDVI
  
  ids$NDVI_min = SOC_ls7_min$NDVI
  ids$NDVI_min_y1 = SOC_ls7_min_y1$NDVI
  ids$NDVI_min_y2 = SOC_ls7_min_y2$NDVI
  ids$NDVI_min_y3 = SOC_ls7_min_y3$NDVI
  ids$NDVI_min_y4 = SOC_ls7_min_y4$NDVI
  ids$NDVI_min_y5 = SOC_ls7_min_y5$NDVI
  
  ids$Year = i + 1998
  output_ls7_ndvi = rbind(output_ls7_ndvi, ids)
}

###################################################################
#This gets year 0 and previous 5 years for 1999-2003
SOC_ls7 = terra::extract(rast(ls7[[1]]), SOC_vects[[17]])
SOC_ls5_y1 = terra::extract(rast(ls5[[12]]), SOC_vects[[17]])
SOC_ls5_y2 = terra::extract(rast(ls5[[11]]), SOC_vects[[17]])
SOC_ls5_y3 = terra::extract(rast(ls5[[10]]), SOC_vects[[17]])
SOC_ls5_y4 = terra::extract(rast(ls5[[9]]), SOC_vects[[17]])
SOC_ls5_y5 = terra::extract(rast(ls5[[8]]), SOC_vects[[17]])

SOC_ls7_max = terra::extract(rast(ls7_max[[1]]), SOC_vects[[17]])
SOC_ls5_max_y1 = terra::extract(rast(ls5_max[[15]]), SOC_vects[[17]])
SOC_ls5_max_y2 = terra::extract(rast(ls5_max[[14]]), SOC_vects[[17]])
SOC_ls5_max_y3 = terra::extract(rast(ls5_max[[13]]), SOC_vects[[17]])
SOC_ls5_max_y4 = terra::extract(rast(ls5_max[[12]]), SOC_vects[[17]])
SOC_ls5_max_y5 = terra::extract(rast(ls5_max[[1]]), SOC_vects[[17]])

SOC_ls7_min = terra::extract(rast(ls7_min[[1]]), SOC_vects[[17]])
SOC_ls5_min_y1 = terra::extract(rast(ls5_min[[15]]), SOC_vects[[17]])
SOC_ls5_min_y2 = terra::extract(rast(ls5_min[[14]]), SOC_vects[[17]])
SOC_ls5_min_y3 = terra::extract(rast(ls5_min[[13]]), SOC_vects[[17]])
SOC_ls5_min_y4 = terra::extract(rast(ls5_min[[12]]), SOC_vects[[17]])
SOC_ls5_min_y5 = terra::extract(rast(ls5_min[[11]]), SOC_vects[[17]])

ids4 = as.data.frame(SOC_ls7$ID)
ids4$NDVI_mean = SOC_ls7$NDVI
ids4$NDVI_mean_y1 = SOC_ls5_y1$NDVI
ids4$NDVI_mean_y2 = SOC_ls5_y2$NDVI
ids4$NDVI_mean_y3 = SOC_ls5_y3$NDVI
ids4$NDVI_mean_y4 = SOC_ls5_y4$NDVI
ids4$NDVI_mean_y5 = SOC_ls5_y5$NDVI

ids4$NDVI_max = SOC_ls7_max$NDVI
ids4$NDVI_max_y1 = SOC_ls5_max_y1$NDVI
ids4$NDVI_max_y2 = SOC_ls5_max_y2$NDVI
ids4$NDVI_max_y3 = SOC_ls5_max_y3$NDVI
ids4$NDVI_max_y4 = SOC_ls5_max_y4$NDVI
ids4$NDVI_max_y5 = SOC_ls5_max_y5$NDVI

ids4$NDVI_min = SOC_ls7_min$NDVI
ids4$NDVI_min_y1 = SOC_ls5_min_y1$NDVI
ids4$NDVI_min_y2 = SOC_ls5_min_y2$NDVI
ids4$NDVI_min_y3 = SOC_ls5_min_y3$NDVI
ids4$NDVI_min_y4 = SOC_ls5_min_y4$NDVI
ids4$NDVI_min_y5 = SOC_ls5_min_y5$NDVI

ids4$Year = 1999

SOC_ls7 = terra::extract(rast(ls7[[2]]), SOC_vects[[18]])
SOC_ls5_y1 = terra::extract(rast(ls7[[1]]), SOC_vects[[18]])
SOC_ls5_y2 = terra::extract(rast(ls5[[12]]), SOC_vects[[18]])
SOC_ls5_y3 = terra::extract(rast(ls5[[11]]), SOC_vects[[18]])
SOC_ls5_y4 = terra::extract(rast(ls5[[10]]), SOC_vects[[18]])
SOC_ls5_y5 = terra::extract(rast(ls5[[9]]), SOC_vects[[18]])

SOC_ls7_max = terra::extract(rast(ls7_max[[2]]), SOC_vects[[18]])
SOC_ls5_max_y1 = terra::extract(rast(ls7_max[[1]]), SOC_vects[[18]])
SOC_ls5_max_y2 = terra::extract(rast(ls5_max[[15]]), SOC_vects[[18]])
SOC_ls5_max_y3 = terra::extract(rast(ls5_max[[14]]), SOC_vects[[18]])
SOC_ls5_max_y4 = terra::extract(rast(ls5_max[[13]]), SOC_vects[[18]])
SOC_ls5_max_y5 = terra::extract(rast(ls5_max[[12]]), SOC_vects[[18]])

SOC_ls7_min = terra::extract(rast(ls7_min[[2]]), SOC_vects[[18]])
SOC_ls5_min_y1 = terra::extract(rast(ls7_min[[1]]), SOC_vects[[18]])
SOC_ls5_min_y2 = terra::extract(rast(ls5_min[[15]]), SOC_vects[[18]])
SOC_ls5_min_y3 = terra::extract(rast(ls5_min[[14]]), SOC_vects[[18]])
SOC_ls5_min_y4 = terra::extract(rast(ls5_min[[13]]), SOC_vects[[18]])
SOC_ls5_min_y5 = terra::extract(rast(ls5_min[[12]]), SOC_vects[[18]])

ids3 = as.data.frame(SOC_ls7$ID)
ids3$NDVI_mean = SOC_ls7$NDVI
ids3$NDVI_mean_y1 = SOC_ls5_y1$NDVI
ids3$NDVI_mean_y2 = SOC_ls5_y2$NDVI
ids3$NDVI_mean_y3 = SOC_ls5_y3$NDVI
ids3$NDVI_mean_y4 = SOC_ls5_y4$NDVI
ids3$NDVI_mean_y5 = SOC_ls5_y5$NDVI

ids3$NDVI_max = SOC_ls7_max$NDVI
ids3$NDVI_max_y1 = SOC_ls5_max_y1$NDVI
ids3$NDVI_max_y2 = SOC_ls5_max_y2$NDVI
ids3$NDVI_max_y3 = SOC_ls5_max_y3$NDVI
ids3$NDVI_max_y4 = SOC_ls5_max_y4$NDVI
ids3$NDVI_max_y5 = SOC_ls5_max_y5$NDVI

ids3$NDVI_min = SOC_ls7_min$NDVI
ids3$NDVI_min_y1 = SOC_ls5_min_y1$NDVI
ids3$NDVI_min_y2 = SOC_ls5_min_y2$NDVI
ids3$NDVI_min_y3 = SOC_ls5_min_y3$NDVI
ids3$NDVI_min_y4 = SOC_ls5_min_y4$NDVI
ids3$NDVI_min_y5 = SOC_ls5_min_y5$NDVI

ids3$Year = 2000

SOC_ls7 = terra::extract(rast(ls7[[3]]), SOC_vects[[19]])
SOC_ls5_y1 = terra::extract(rast(ls7[[2]]), SOC_vects[[19]])
SOC_ls5_y2 = terra::extract(rast(ls7[[1]]), SOC_vects[[19]])
SOC_ls5_y3 = terra::extract(rast(ls5[[12]]), SOC_vects[[19]])
SOC_ls5_y4 = terra::extract(rast(ls5[[11]]), SOC_vects[[19]])
SOC_ls5_y5 = terra::extract(rast(ls5[[10]]), SOC_vects[[19]])

SOC_ls7_max = terra::extract(rast(ls7_max[[3]]), SOC_vects[[19]])
SOC_ls5_max_y1 = terra::extract(rast(ls7_max[[2]]), SOC_vects[[19]])
SOC_ls5_max_y2 = terra::extract(rast(ls7_max[[1]]), SOC_vects[[19]])
SOC_ls5_max_y3 = terra::extract(rast(ls5_max[[15]]), SOC_vects[[19]])
SOC_ls5_max_y4 = terra::extract(rast(ls5_max[[14]]), SOC_vects[[19]])
SOC_ls5_max_y5 = terra::extract(rast(ls5_max[[13]]), SOC_vects[[19]])

SOC_ls7_min = terra::extract(rast(ls7_min[[3]]), SOC_vects[[19]])
SOC_ls5_min_y1 = terra::extract(rast(ls7_min[[2]]), SOC_vects[[19]])
SOC_ls5_min_y2 = terra::extract(rast(ls7_min[[1]]), SOC_vects[[19]])
SOC_ls5_min_y3 = terra::extract(rast(ls5_min[[15]]), SOC_vects[[19]])
SOC_ls5_min_y4 = terra::extract(rast(ls5_min[[14]]), SOC_vects[[19]])
SOC_ls5_min_y5 = terra::extract(rast(ls5_min[[13]]), SOC_vects[[19]])

ids2 = as.data.frame(SOC_ls7$ID)
ids2$NDVI_mean = SOC_ls7$NDVI
ids2$NDVI_mean_y1 = SOC_ls5_y1$NDVI
ids2$NDVI_mean_y2 = SOC_ls5_y2$NDVI
ids2$NDVI_mean_y3 = SOC_ls5_y3$NDVI
ids2$NDVI_mean_y4 = SOC_ls5_y4$NDVI
ids2$NDVI_mean_y5 = SOC_ls5_y5$NDVI

ids2$NDVI_max = SOC_ls7_max$NDVI
ids2$NDVI_max_y1 = SOC_ls5_max_y1$NDVI
ids2$NDVI_max_y2 = SOC_ls5_max_y2$NDVI
ids2$NDVI_max_y3 = SOC_ls5_max_y3$NDVI
ids2$NDVI_max_y4 = SOC_ls5_max_y4$NDVI
ids2$NDVI_max_y5 = SOC_ls5_max_y5$NDVI

ids2$NDVI_min = SOC_ls7_min$NDVI
ids2$NDVI_min_y1 = SOC_ls5_min_y1$NDVI
ids2$NDVI_min_y2 = SOC_ls5_min_y2$NDVI
ids2$NDVI_min_y3 = SOC_ls5_min_y3$NDVI
ids2$NDVI_min_y4 = SOC_ls5_min_y4$NDVI
ids2$NDVI_min_y5 = SOC_ls5_min_y5$NDVI

ids2$Year = 2001

SOC_ls7 = terra::extract(rast(ls7[[4]]), SOC_vects[[20]])
SOC_ls5_y1 = terra::extract(rast(ls7[[3]]), SOC_vects[[20]])
SOC_ls5_y2 = terra::extract(rast(ls7[[2]]), SOC_vects[[20]])
SOC_ls5_y3 = terra::extract(rast(ls7[[1]]), SOC_vects[[20]])
SOC_ls5_y4 = terra::extract(rast(ls5[[12]]), SOC_vects[[20]])
SOC_ls5_y5 = terra::extract(rast(ls5[[11]]), SOC_vects[[20]])

SOC_ls7_max = terra::extract(rast(ls7_max[[4]]), SOC_vects[[20]])
SOC_ls5_max_y1 = terra::extract(rast(ls7_max[[3]]), SOC_vects[[20]])
SOC_ls5_max_y2 = terra::extract(rast(ls7_max[[2]]), SOC_vects[[20]])
SOC_ls5_max_y3 = terra::extract(rast(ls7_max[[1]]), SOC_vects[[20]])
SOC_ls5_max_y4 = terra::extract(rast(ls5_max[[15]]), SOC_vects[[20]])
SOC_ls5_max_y5 = terra::extract(rast(ls5_max[[14]]), SOC_vects[[20]])

SOC_ls7_min = terra::extract(rast(ls7_min[[4]]), SOC_vects[[20]])
SOC_ls5_min_y1 = terra::extract(rast(ls7_min[[3]]), SOC_vects[[20]])
SOC_ls5_min_y2 = terra::extract(rast(ls7_min[[2]]), SOC_vects[[20]])
SOC_ls5_min_y3 = terra::extract(rast(ls7_min[[1]]), SOC_vects[[20]])
SOC_ls5_min_y4 = terra::extract(rast(ls5_min[[15]]), SOC_vects[[20]])
SOC_ls5_min_y5 = terra::extract(rast(ls5_min[[14]]), SOC_vects[[20]])

ids1 = as.data.frame(SOC_ls7$ID)
ids1$NDVI_mean = SOC_ls7$NDVI
ids1$NDVI_mean_y1 = SOC_ls5_y1$NDVI
ids1$NDVI_mean_y2 = SOC_ls5_y2$NDVI
ids1$NDVI_mean_y3 = SOC_ls5_y3$NDVI
ids1$NDVI_mean_y4 = SOC_ls5_y4$NDVI
ids1$NDVI_mean_y5 = SOC_ls5_y5$NDVI

ids1$NDVI_max = SOC_ls7_max$NDVI
ids1$NDVI_max_y1 = SOC_ls5_max_y1$NDVI
ids1$NDVI_max_y2 = SOC_ls5_max_y2$NDVI
ids1$NDVI_max_y3 = SOC_ls5_max_y3$NDVI
ids1$NDVI_max_y4 = SOC_ls5_max_y4$NDVI
ids1$NDVI_max_y5 = SOC_ls5_max_y5$NDVI

ids1$NDVI_min = SOC_ls7_min$NDVI
ids1$NDVI_min_y1 = SOC_ls5_min_y1$NDVI
ids1$NDVI_min_y2 = SOC_ls5_min_y2$NDVI
ids1$NDVI_min_y3 = SOC_ls5_min_y3$NDVI
ids1$NDVI_min_y4 = SOC_ls5_min_y4$NDVI
ids1$NDVI_min_y5 = SOC_ls5_min_y5$NDVI

ids1$Year = 2002

SOC_ls7 = terra::extract(rast(ls7[[5]]), SOC_vects[[21]])
SOC_ls5_y1 = terra::extract(rast(ls7[[4]]), SOC_vects[[21]])
SOC_ls5_y2 = terra::extract(rast(ls7[[3]]), SOC_vects[[21]])
SOC_ls5_y3 = terra::extract(rast(ls7[[2]]), SOC_vects[[21]])
SOC_ls5_y4 = terra::extract(rast(ls7[[1]]), SOC_vects[[21]])
SOC_ls5_y5 = terra::extract(rast(ls5[[12]]), SOC_vects[[21]])

SOC_ls7_max = terra::extract(rast(ls7_max[[5]]), SOC_vects[[21]])
SOC_ls5_max_y1 = terra::extract(rast(ls7_max[[4]]), SOC_vects[[21]])
SOC_ls5_max_y2 = terra::extract(rast(ls7_max[[3]]), SOC_vects[[21]])
SOC_ls5_max_y3 = terra::extract(rast(ls7_max[[2]]), SOC_vects[[21]])
SOC_ls5_max_y4 = terra::extract(rast(ls7_max[[1]]), SOC_vects[[21]])
SOC_ls5_max_y5 = terra::extract(rast(ls5_max[[15]]), SOC_vects[[21]])

SOC_ls7_min = terra::extract(rast(ls7_min[[5]]), SOC_vects[[21]])
SOC_ls5_min_y1 = terra::extract(rast(ls7_min[[4]]), SOC_vects[[21]])
SOC_ls5_min_y2 = terra::extract(rast(ls7_min[[3]]), SOC_vects[[21]])
SOC_ls5_min_y3 = terra::extract(rast(ls7_min[[2]]), SOC_vects[[21]])
SOC_ls5_min_y4 = terra::extract(rast(ls7_min[[1]]), SOC_vects[[21]])
SOC_ls5_min_y5 = terra::extract(rast(ls5_min[[15]]), SOC_vects[[21]])

ids = as.data.frame(SOC_ls7$ID)
ids$NDVI_mean = SOC_ls7$NDVI
ids$NDVI_mean_y1 = SOC_ls5_y1$NDVI
ids$NDVI_mean_y2 = SOC_ls5_y2$NDVI
ids$NDVI_mean_y3 = SOC_ls5_y3$NDVI
ids$NDVI_mean_y4 = SOC_ls5_y4$NDVI
ids$NDVI_mean_y5 = SOC_ls5_y5$NDVI

ids$NDVI_max = SOC_ls7_max$NDVI
ids$NDVI_max_y1 = SOC_ls5_max_y1$NDVI
ids$NDVI_max_y2 = SOC_ls5_max_y2$NDVI
ids$NDVI_max_y3 = SOC_ls5_max_y3$NDVI
ids$NDVI_max_y4 = SOC_ls5_max_y4$NDVI
ids$NDVI_max_y5 = SOC_ls5_max_y5$NDVI

ids$NDVI_min = SOC_ls7_min$NDVI
ids$NDVI_min_y1 = SOC_ls5_min_y1$NDVI
ids$NDVI_min_y2 = SOC_ls5_min_y2$NDVI
ids$NDVI_min_y3 = SOC_ls5_min_y3$NDVI
ids$NDVI_min_y4 = SOC_ls5_min_y4$NDVI
ids$NDVI_min_y5 = SOC_ls5_min_y5$NDVI

ids$Year = 2003

output_ls5_ndvi_bridge = rbind(ids4, ids3 ,ids2, ids1, ids)

############################################################################
for(i in 1:length(SOC_vects)){
  SOC_vects[[i]]$ID = 1:nrow(SOC_vects[[i]])  
  }

num1 = 1
num3 = 0
all_ndvi = data.frame(matrix(nrow = 0, ncol = 4))
for (i in 10:16){
  SOC_df = as.data.frame(unlist(SOC_vects[[i]]))
  num2 = nrow(SOC_df)
  num3 = num3 + num2
  ndvi_df = output_ls5_ndvi[num1:num3,]
  colnames(ndvi_df)[colnames(ndvi_df) == 'SOC_ls5$ID'] <- 'ID'
  merge1 = merge(ndvi_df, SOC_df, by = "ID")
  colnames(merge1) = c("ID", "NDVI_mean", "NDVI_mean_y1", "NDVI_mean_y2", "NDVI_mean_y3","NDVI_mean_y4" ,
                       "NDVI_mean_y5", "NDVI_max" ,"NDVI_max_y1","NDVI_max_y2", "NDVI_max_y3","NDVI_max_y4",
                       "NDVI_max_y5","NDVI_min" ,"NDVI_min_y1","NDVI_min_y2","NDVI_min_y3",
                       "NDVI_min_y4","NDVI_min_y5","Year", "X","Year", "Cluster_ID","SOC_kgm2", "Latitude", "Longitude")
                       # "hzID",
                       # "Cluster_ID"  ,"layer_top_cm", "layer_bottom_cm",
                       # "Layer_Name","Year", "SOC_kgm2", "LULC",
                       # "Dataset" ,"Thickness" , "SliceID", 
                       # "Top", "Bottom"  )   
  all_ndvi = rbind(all_ndvi, merge1)
  num1 = num1 + num2
}

num1 = 1
num3 = 0
all_ndvi_2 = data.frame(matrix(nrow = 0, ncol = 4))
for (i in 17:21){
  SOC_df = as.data.frame(unlist(SOC_vects[[i]]))
  num2 = nrow(SOC_df)
  num3 = num3 + num2
  ndvi_df = output_ls5_ndvi_bridge[num1:num3,]
  colnames(ndvi_df)[colnames(ndvi_df) == 'SOC_ls7$ID'] <- 'ID'
  merge1 = merge(ndvi_df, SOC_df, by = "ID")
  colnames(merge1) = c("ID", "NDVI_mean", "NDVI_mean_y1", "NDVI_mean_y2", "NDVI_mean_y3","NDVI_mean_y4" ,
                       "NDVI_mean_y5", "NDVI_max" ,"NDVI_max_y1","NDVI_max_y2", "NDVI_max_y3","NDVI_max_y4",
                       "NDVI_max_y5","NDVI_min" ,"NDVI_min_y1","NDVI_min_y2","NDVI_min_y3",
                       "NDVI_min_y4","NDVI_min_y5","Year", "X", "Year", "Cluster_ID", "SOC_kgm2","Latitude", "Longitude")
                       # "hzID",
                       # "Cluster_ID"  ,"layer_top_cm", "layer_bottom_cm",
                       # "Layer_Name","Year", "SOC_kgm2", "LULC",
                       # "Dataset" ,"Thickness" , "SliceID", 
                       # "Top", "Bottom"  )   
  all_ndvi_2 = rbind(all_ndvi_2, merge1)
  num1 = num1 + num2
}

num1 = 1
num3 = 0
all_ndvi_3 = data.frame(matrix(nrow = 0, ncol = 4))
for (i in 22:36){
  SOC_df = as.data.frame(unlist(SOC_vects[[i]]))
  num2 = nrow(SOC_df)
  num3 = num3 + num2
  ndvi_df = output_ls7_ndvi[num1:num3,]
  colnames(ndvi_df)[colnames(ndvi_df) == 'SOC_ls7$ID'] <- 'ID'
  merge1 = merge(ndvi_df, SOC_df, by = "ID")
  colnames(merge1) = c("ID", "NDVI_mean", "NDVI_mean_y1", "NDVI_mean_y2", "NDVI_mean_y3","NDVI_mean_y4" ,
                       "NDVI_mean_y5", "NDVI_max" ,"NDVI_max_y1","NDVI_max_y2", "NDVI_max_y3","NDVI_max_y4",
                       "NDVI_max_y5","NDVI_min" ,"NDVI_min_y1","NDVI_min_y2","NDVI_min_y3",
                       "NDVI_min_y4","NDVI_min_y5","Year", "X","Year", "Cluster_ID","SOC_kgm2", "Latitude", "Longitude")
                       # "hzID",
                       # "Cluster_ID"  ,"layer_top_cm", "layer_bottom_cm",
                       # "Layer_Name","Year", , "LULC",
                       # "Dataset" ,"Thickness" , "SliceID", 
                       # "Top", "Bottom"  )   
  all_ndvi_3 = rbind(all_ndvi_3, merge1)
  num1 = num1 + num2
}


full_df_with_ndvi = rbind(all_ndvi, all_ndvi_2, all_ndvi_3)

# write.csv(full_df_with_ndvi, "full_df5_with_ndvi_03212024.csv")


#####################################
###########Pre-1992#############
#This gets year 0 and previous 5 years for 1992-1998
output_pre1992_ndvi = data.frame(matrix(nrow = 0, ncol = 4))
colnames(output_pre1992_ndvi) = c("ID", "NDVI_mean", "NDVI_max", "NDVI_min")
for (i in 1:5){
  SOC_ls5 = terra::extract(rast(ls5[[i]]), SOC_vects[[i+4]])
  SOC_ls5_max = terra::extract(rast(ls5_max[[i]]), SOC_vects[[i+4]])
  SOC_ls5_min = terra::extract(rast(ls5_min[[i]]), SOC_vects[[i+4]])
  ids = as.data.frame(SOC_ls5$ID)
  ids$NDVI_mean = SOC_ls5$NDVI
  ids$NDVI_max = SOC_ls5_max$NDVI
  ids$NDVI_min = SOC_ls5_min$NDVI
  ids$Year = i + 1986
  output_pre1992_ndvi = rbind(output_pre1992_ndvi, ids)
}

num1 = 1
num3 = 0
all_ndvi_pre = data.frame(matrix(nrow = 0, ncol = 4))
for (i in 5:9){
  SOC_df = as.data.frame(unlist(SOC_vects[[i]]))
  num2 = nrow(SOC_df)
  num3 = num3 + num2
  ndvi_df = output_pre1992_ndvi[num1:num3,]
  colnames(ndvi_df)[colnames(ndvi_df) == 'SOC_ls5$ID'] <- 'ID'
  merge1 = merge(ndvi_df, SOC_df, by = "ID")
  colnames(merge1) = c("ID", "NDVI_mean","NDVI_max","NDVI_min","Year", "X","Year", "Cluster_ID","SOC_kgm2", "Latitude", "Longitude")
  all_ndvi_pre = rbind(all_ndvi_pre, merge1)
  num1 = num1 + num2
}

#Now go to get_daymet.R
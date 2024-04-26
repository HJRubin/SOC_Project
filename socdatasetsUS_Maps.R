library(terra)
pacman::p_load(viridis,mapproj,RColorBrewer,egg,ggpubr,sf)
#check POLARIS,GSOCmap,. SOC_Mexico_CONUS(1991–2010),Ramcharan----
setwd("/Users/zhuonanwang/SOC_MajorRevision_data")
list.files()

crs_target = "+proj=longlat +datum=WGS84 +no_defs"
us_extent <- ext(-180, -55, 15, 80)


#0-30cm----------
#1.RF; 2.POLARIS; 3.SOC_Mexico_CONUS; 4.GSOCmap1.5.0; 5.soc_Ramcharan

#1. RF
input_folder <- paste0("/Users/zhuonanwang/SOC_MajorRevision_data/OUTPUT/",30,"cm_","noMicrobe","_","withPed20clsenvVarRFlogsoc","/")
rasterfile=paste0(input_folder,"sites_30cm_PCs_eachcluster/mean_20RF.tif")
rf_soc_stat <- rast(rasterfile)[[1]]
plot(rf_soc_stat)
origin(rf_soc_stat) #0 0

usCONUS_extent <- ext(-126.5, -66.4, 24.5, 50.7)
#crop to get CONUS
rf_soc_stat_CONUS <- crop(rf_soc_stat,usCONUS_extent)
plot(rf_soc_stat_CONUS)
crs(rf_soc_stat_CONUS, proj=TRUE)
res(rf_soc_stat_CONUS)
origin(rf_soc_stat_CONUS) #0.000000e+00 -7.105427e-15
#writeRaster(rf_soc_stat_CONUS, "rf_soc_CONUS0_0_30cm_30arcseconds.tif", overwrite=TRUE)

#2.POLARIS
polaris30cm <- rast("polaris_socstock_30cm.tif") #kg/m2
plot(polaris30cm)
crs(polaris30cm, proj=TRUE)
res(polaris30cm)
origin(polaris30cm) #0.000000e+00 -7.105427e-15
polaris30cm_CONUS <- resample(polaris30cm, rf_soc_stat_CONUS, method="bilinear")
plot(polaris30cm_CONUS)
origin(polaris30cm_CONUS) #0.000000e+00 -7.105427e-15
#writeRaster(polaris30cm_CONUS, "polaris30cm_CONUS_0_30cm_30arcseconds.tif", overwrite=TRUE)

#3.SOC_Mexico_CONUS
SOC_CONUS <- rast("SOC_prediction_1991_2010_US.tif") #kg/m2
plot(SOC_CONUS)
SOC_CONUS1km <- aggregate(SOC_CONUS, fact=4)#aggregate to 1km
SOC_prediction_1991_2010_US_wgs84 <- project(SOC_CONUS1km,crs_target)
SOC_prediction_1991_2010_US_wgs84_30arc <- resample(SOC_prediction_1991_2010_US_wgs84, rf_soc_stat_CONUS, method="bilinear")
plot(SOC_prediction_1991_2010_US_wgs84_30arc)
crs(SOC_prediction_1991_2010_US_wgs84_30arc, proj=TRUE)
res(SOC_prediction_1991_2010_US_wgs84_30arc)
origin(SOC_prediction_1991_2010_US_wgs84_30arc)#0.000000e+00 -7.105427e-15
#writeRaster(SOC_prediction_1991_2010_US_wgs84_30arc, "SOC_prediction_1991_2010_US_wgs84_30arc.tif", overwrite=TRUE)

#4.GSOCmap1.5.0;
GSOCmap_us=rast("GSOCmap_us_0_30cm_1km.tif")#kg/m2
plot(GSOCmap_us)
#crop to get CONUS
GSOCmap_us_CONUS <- crop(GSOCmap_us,usCONUS_extent)
plot(GSOCmap_us_CONUS)
crs(GSOCmap_us_CONUS, proj=TRUE)
res(GSOCmap_us_CONUS)
origin(GSOCmap_us_CONUS) #0.000000e+00 -7.105427e-15
#writeRaster(GSOCmap_us_CONUS, "GSOCmap_us_CONUS_0_30cm_30arc.tif", overwrite=TRUE)


#5.soc_Ramcharan
soc_Ramcharan_0_30cm_1km <- rast( "soc_Ramcharan_0_30cm_1km.tif")
plot(soc_Ramcharan_0_30cm_1km)
soc_Ramcharan_0_30cm_1km_wgs84 <- project(soc_Ramcharan_0_30cm_1km,crs_target)
plot(soc_Ramcharan_0_30cm_1km_wgs84)
ext(soc_Ramcharan_0_30cm_1km_wgs84)
soc_Ramcharan_0_30cm_1km_wgs84 <- crop(soc_Ramcharan_0_30cm_1km_wgs84,usCONUS_extent)
plot(soc_Ramcharan_0_30cm_1km_wgs84)
soc_Ramcharan_0_30cm_1km_wgs84_30arc <- resample(soc_Ramcharan_0_30cm_1km_wgs84, rf_soc_stat_CONUS, method="bilinear")
origin(soc_Ramcharan_0_30cm_1km_wgs84_30arc)
plot(soc_Ramcharan_0_30cm_1km_wgs84_30arc)
#writeRaster(soc_Ramcharan_0_30cm_1km_wgs84_30arc, "soc_Ramcharan_0_30cm_1km_wgs84_30arc.tif", overwrite=TRUE)

#start to plot:0-30cm depth
rf_soc_stat
rf_soc_stat_CONUS
polaris30cm_CONUS
SOC_prediction_1991_2010_US_wgs84_30arc
GSOCmap_us_CONUS
soc_Ramcharan_0_30cm_1km_wgs84_30arc

rf_soc_stat_CONUS <- rast("rf_soc_CONUS0_0_30cm_30arcseconds.tif")
polaris30cm_CONUS <- rast("polaris30cm_CONUS_0_30cm_30arcseconds.tif")
SOC_prediction_1991_2010_US_wgs84_30arc <- rast("SOC_prediction_1991_2010_US_wgs84_30arc.tif")
GSOCmap_us_CONUS <- rast("GSOCmap_us_CONUS_0_30cm_30arc.tif")
soc_Ramcharan_0_30cm_1km_wgs84_30arc <- rast("soc_Ramcharan_0_30cm_1km_wgs84_30arc.tif")

minmax(rf_soc_stat)
minmax(rf_soc_stat_CONUS)
minmax(polaris30cm_CONUS)
minmax(SOC_prediction_1991_2010_US_wgs84_30arc)
minmax(GSOCmap_us)
minmax(soc_Ramcharan_0_30cm_1km_wgs84_30arc)

cluster_df.pretty_breaks <- c(0,2,3,4,5,6,7,8,9,11,13,15,20,30,50,60,80,100,450) #18 cols
#My color:coul
display.brewer.pal(10, "BrBG")
coul <- brewer.pal(10, "BrBG")
coul <- colorRampPalette(coul)(20)
coul <-coul[c(1:17,length(coul))]
barplot(rep(1, length(coul)), col = coul , main="BrBG") 
names(coul) <- c(paste0(cluster_df.pretty_breaks)[-c(1,length(cluster_df.pretty_breaks))]," ")

pt=12 #default
# title_string="test"
plot_raster <- function(rast_file,title_string){
  cluster <- rast_file
  cluster_df <- as.data.frame(cluster, xy = T)
  names(cluster_df)[3] <- "mean"
  cluster_df.brks <- cluster_df.pretty_breaks
  cluster_df.labels <- c(round(cluster_df.brks,2)[-c(1,length(cluster_df.brks))]," ")
  
  # define a new variable on the data set just as above
  cluster_df$brks <- cut(cluster_df$mean, 
                         breaks = cluster_df.brks, 
                         include.lowest = TRUE, 
                         labels = cluster_df.labels)
  brks_scale <- levels(cluster_df$brks)
  labels_scale <- (brks_scale)

  conus_df <- cluster_df %>% dplyr::filter(y>23&y<51&x<(-65.)&x>(-126.5))
  
  conus_state <-  st_read("/Users/zhuonanwang/SOC_MajorRevision_data/INPUT/us_shp/conus_state.shp")
  
  conus_df.map <- ggplot()+
    geom_raster(data = conus_df, aes(x = x, y = y,fill=brks))+
    geom_sf(data=conus_state,fill="transparent", color="white", size=0.05) + #lines
    #coord_sf(expand = FALSE)+
    scale_fill_manual(values = (coul),
                      breaks = (brks_scale),
                      labels = (labels_scale),
                      guide = guide_legend(
                        direction = "horizontal",
                        keyheight = unit(3, units = "mm"),
                        keywidth = unit(7, units = "mm"),
                        title = expression(~Kg~C~m^{-2}), 
                        title.position = "bottom",
                        # exactly at the right end of each legend key
                        # title.hjust = 0.5,
                        label.hjust = 1.5,
                        # label.vjust = 1.1,
                        # label.theme = element_text(angle = 0),
                        # ncol = 1,
                        nrow=1,
                        
                        # reverse = T,# also the guide needs to be reversed
                        label.position = "bottom"
                      )  
    )+
    theme_bw(base_size = pt)+ 
    scale_y_continuous(breaks = seq(25, 50, by = 10),limits = c(24, 51),
                       labels=as.character(seq(25, 50, by = 10))) +
    scale_x_continuous(breaks = seq(-120, -76, by = 20),limits = c(-126, -66),expand = c(0, 0),
                       labels=as.character(seq(-120, -76, by = 20)))+
    theme(
      panel.background = element_rect(fill = '#ffffff', colour = 'black'),#D5DBDB
      panel.grid.minor=element_blank(),
      panel.grid.major=element_blank(),
      panel.border = element_rect(fill=NA, colour = "black", size=1.),#size
      #legend.title=element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),#x,y:longitude,latitude
      axis.text=element_text(color="black",size=9),
      legend.position = "bottom",
      legend.spacing.x =unit(0,'cm'),
      #legend.spacing.y = unit(-0.1, 'cm'),legend.spacing.x = unit(-0.1, 'cm') 
      plot.title = element_text(hjust = 0.5,size = 11),
      # plot.title = element_text(color = "red", size = 12, face = "bold"),
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(0,0,0,0)
    )+ 
    labs(title = title_string)
  
  return(conus_df.map)
}

{
  rfplot_plot <- plot_raster(rf_soc_stat_CONUS,title_string="Rep SOC")
  polaris30cm_CONUS_plot <- plot_raster(polaris30cm_CONUS,title_string="polaris30cm")
  SOC_prediction_1991_2010_US_wgs84_30arc_plot <- plot_raster(SOC_prediction_1991_2010_US_wgs84_30arc,title_string="SOC_prediction_1991_2010")
  GSOCmap_us_CONUS_plot <- plot_raster(GSOCmap_us_CONUS,title_string="GSOCmap_plot")
  soc_Ramcharan_0_30cm_1km_wgs84_30arc_plot <- plot_raster(soc_Ramcharan_0_30cm_1km_wgs84_30arc,title_string="Ramcharan et al.")
}


library(ggpubr)
#pt=12
final <- ggarrange(rfplot_plot, 
                   polaris30cm_CONUS_plot, 
                   GSOCmap_us_CONUS_plot,
                   soc_Ramcharan_0_30cm_1km_wgs84_30arc_plot,
                   SOC_prediction_1991_2010_US_wgs84_30arc_plot,
                   #+theme_bw(base_size = pt), 
                   # labels = c("A", "B", "C","D","E"),
                   common.legend = TRUE, legend="bottom",
                   ncol = 3, nrow = 2)

ggsave(paste0("CONUS_30cm_SOC_raster.tiff"),
       final,
       width = 25 , height = 13, units = "cm",
       # width = 38 , height = 10, units = "cm",
       #type = "cairo", 
       compression="lzw",bg="white",
       dpi = 300)
#==============================================================================================
#0-100cm-------------
#1. RF
input_folder <- paste0("/Users/zhuonanwang/SOC_MajorRevision_data/OUTPUT/",100,"cm_","noMicrobe","_","withPed20clsenvVarRFlogsoc","/")
rasterfile=paste0(input_folder,"sites_100cm_PCs_eachcluster/mean_20RF.tif")
rf_soc_stat <- rast(rasterfile)[[1]]
plot(rf_soc_stat)
origin(rf_soc_stat) #0 0

usCONUS_extent <- ext(-126.5, -66.4, 24.5, 50.7)
#crop to get CONUS
rf_soc_stat_CONUS <- crop(rf_soc_stat,usCONUS_extent)
plot(rf_soc_stat_CONUS)
crs(rf_soc_stat_CONUS, proj=TRUE)
res(rf_soc_stat_CONUS)
origin(rf_soc_stat_CONUS) #0.000000e+00 -7.105427e-15
#writeRaster(rf_soc_stat_CONUS, "rf_soc_CONUS0_0_100cm_30arcseconds.tif", overwrite=TRUE)

#POLARIS 
polaris100cm <- rast("polaris_socstock_100cm.tif") #kg/m2
plot(polaris100cm)
crs(polaris100cm, proj=TRUE)
res(polaris100cm)
origin(polaris100cm) #0.000000e+00 -7.105427e-15
polaris100cm_CONUS <- resample(polaris100cm, rf_soc_stat_CONUS, method="bilinear")
plot(polaris100cm_CONUS)
origin(polaris100cm_CONUS) #0.000000e+00 -7.105427e-15
#writeRaster(polaris100cm_CONUS, "polaris30cm_CONUS_0_100cm_30arcseconds.tif", overwrite=TRUE)

#Ramcharan 100m resolution----
soc_Ramcharan_0_100cm_1km <- rast( "soc_Ramcharan_0_100cm_1km.tif")
plot(soc_Ramcharan_0_100cm_1km)
soc_Ramcharan_0_100cm_1km_wgs84 <- project(soc_Ramcharan_0_100cm_1km,crs_target)
plot(soc_Ramcharan_0_100cm_1km_wgs84)
ext(soc_Ramcharan_0_100cm_1km_wgs84)
soc_Ramcharan_0_100cm_1km_wgs84 <- crop(soc_Ramcharan_0_100cm_1km_wgs84,usCONUS_extent)
plot(soc_Ramcharan_0_100cm_1km_wgs84)
soc_Ramcharan_0_100cm_1km_wgs84_30arc <- resample(soc_Ramcharan_0_100cm_1km_wgs84, rf_soc_stat_CONUS, method="bilinear")
origin(soc_Ramcharan_0_100cm_1km_wgs84_30arc)
plot(soc_Ramcharan_0_100cm_1km_wgs84_30arc)
#writeRaster(soc_Ramcharan_0_100cm_1km_wgs84_30arc, "soc_Ramcharan_0_100cm_1km_wgs84_30arc.tif", overwrite=TRUE)

rf_soc_stat_CONUS <- rast("rf_soc_CONUS0_0_100cm_30arcseconds.tif")
polaris100cm_CONUS <- rast("polaris30cm_CONUS_0_100cm_30arcseconds.tif")
soc_Ramcharan_0_100cm_1km_wgs84_30arc <- rast("soc_Ramcharan_0_100cm_1km_wgs84_30arc.tif")

minmax(rf_soc_stat_CONUS)
minmax(polaris100cm_CONUS)
minmax(soc_Ramcharan_0_100cm_1km_wgs84_30arc)

cluster_df.pretty_breaks <- c(0,2,3,4,5,6,7,8,9,11,13,15,20,30,50,60,100,200,700) #18 cols
#My color:coul
display.brewer.pal(10, "BrBG")
coul <- brewer.pal(10, "BrBG")
coul <- colorRampPalette(coul)(20)
coul <-coul[c(1:17,length(coul))]
barplot(rep(1, length(coul)), col = coul , main="BrBG") 
names(coul) <- c(paste0(cluster_df.pretty_breaks)[-c(1,length(cluster_df.pretty_breaks))]," ")


{
  rfplot_plot <- plot_raster(rf_soc_stat_CONUS,title_string="Rep SOC")
  polaris100cm_CONUS_plot <- plot_raster(polaris100cm_CONUS,title_string="polaris100cm")
  soc_Ramcharan_0_100cm_1km_wgs84_30arc_plot <- plot_raster(soc_Ramcharan_0_100cm_1km_wgs84_30arc,title_string="Ramcharan et al.")
}


library(ggpubr)
#pt=12
final <- ggarrange(rfplot_plot, 
                   polaris100cm_CONUS_plot, 
                   soc_Ramcharan_0_100cm_1km_wgs84_30arc_plot,
                   #+theme_bw(base_size = pt), 
                   # labels = c("A", "B", "C","D","E"),
                   common.legend = TRUE, legend="bottom",
                   ncol = 3, nrow = 2)

ggsave(paste0("CONUS_100cm_SOC_raster.tiff"),
       final,
       width = 25 , height = 13, units = "cm",
       # width = 38 , height = 10, units = "cm",
       #type = "cairo", 
       compression="lzw",bg="white",
       dpi = 300)

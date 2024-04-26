library(ggplot2)
library(terra)
library(tidyterra)

usa <- map_data("usa")

full_df = full_df[complete.cases(full_df),]

# all = rbind(testing, training)
ggplot() +
  geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = NA, color = "black") +
  coord_fixed(1.3) +
  geom_point(data = full_df, aes(SOC1987.long, SOC1987.lat, color = SOC_gcm2)) +
  theme_classic()

all_output_geol$Year = as.numeric(all_output_geol$Year)

for(row in 1:nrow(full_df)){
  if (as.numeric(full_df$Year)[row] < 1980){
    full_df$Decade[row] = "1970s"
  }
  else if (as.numeric(full_df$Year)[row] >= 2010){
    full_df$Decade[row] = "2010s"
  }
  else if (as.numeric(full_df$Year)[row] >= 2000){
    full_df$Decade[row] = "2000s"
  }
  else if (as.numeric(full_df$Year)[row] >= 1990){
    full_df$Decade[row] = "1990s"
  }
  else if (as.numeric(full_df$Year)[row] >= 1980){
    full_df$Decade[row] = "1980s"
  }
  else{print(row)
    print("NA")}
}

ggplot() +
  geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = NA, color = "black") +
  coord_fixed(1.3) +
  geom_point(data = full_df, aes(SOC1987.long, SOC1987.lat, color = Decade)) +
  scale_color_manual(values = c("darkblue", "darkred", "darkgreen", "grey")) +
  theme_bw()

count_soils = all_output_geol %>% count(Year)

ggplot(data = count_soils, aes(Year, n)) + 
  geom_point() +
  geom_line() + 
  theme_bw()

all_output_geol$SOC = all_output_geol$SOC_gcm2 / 10

soc_ave = aggregate(SOC_gcm2 ~ Year, data = all_output_geol, FUN = median)
soc_ave$Value = "Median"
soc_min = aggregate(SOC_gcm2 ~ Year, data = all_output_geol, FUN = min)
soc_min$Value = "Minimum"
soc_max = aggregate(SOC_gcm2 ~ Year, data = all_output_geol, FUN = max)
soc_max$Value = "Maximum"

soc_plot = rbind(soc_ave, soc_min, soc_max)

ggplot(data = soc_plot, aes(Year, SOC_gcm2, color = Value)) + 
  geom_point() +
  scale_color_manual(values = c("lightblue", "darkblue", "blue")) +
  geom_line() + 
  theme_bw() + 
  scale_y_continuous(trans='log2') + 
  stat_smooth(method = "lm", col = "black")

lm(SOC ~ Year ,data = all_output_geol)


###########################################################
##########################################################

df1$SOC = df1$SOC_gcm2 / 10
df_agg = aggregate(SOC ~ Year, data = all_output_geol, FUN = median)

df_agg = df_agg[df_agg$SOC < 5,]

ggplot(df_agg, aes(Year, SOC)) + 
  geom_point() + 
  theme_bw() +
  # ylim(0,3) + 
  stat_smooth(method = "lm", col = "darkgreen")


v = vect(usa, geom = c("long", "lat"))
m = crop(lu, v1)
lu = rast("LandUse/CONUS_2022.tif")
  
ggplot() +
  geom_spatraster(data = m, aes(fill = NDVI)) +
  geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = NA, color = "black") +
  # scale_fill_viridis_d() +
  theme_bw() +
  theme(
    legend.position = "bottom") + 
  ylab("Latitude") + 
  xlab("Longitude")

###########################################################

# hist(training_2$SOC_gcm2)
# hist(testing_2$SOC_gcm2)
hist(training_2$SOC1987.NDVI_max)
hist(training_2$SOC1987.NDVI_mean)
hist(training_2$SOC1987.Precip)
hist(training_2$SOC1987.Elev)
hist(training_2$SOC1987.Slope)
###########################################################

soc = rast("C:/Users/User/Documents/SOC/RF_SOC_2022_2.nc")

ggplot() +
  geom_spatraster(data = soc, aes(fill = RF_SOC_2022_2)) +
  geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = NA, color = "black") +
  # scale_fill_viridis_d() +
  theme_bw() +
  theme(legend.position = "bottom") + 
  ylab("Latitude") + 
  xlab("Longitude")


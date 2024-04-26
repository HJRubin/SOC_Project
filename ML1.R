#Written Nov 2020 by David Lutz for Secchi depth Rubin et al. manuscript
#edited Jan 2021 for final v of MS
#updated by Hannah Rubin 2023 for SOC Rubin et al. manuscript
setwd("~/R/SOC")

library(tidyverse)
library(dplyr)
library(xgboost)
library(caTools)
library(caret)
library(ggplot2)
library(data.table)
library(Metrics)
library(gridExtra)
library(randomForest)
library(readxl)
library(sf)
library(sp)
library(gstat)
library(automap)
library(e1071)
library(gbm)
library(ie2misc)
library(maps)
library(ggplot2)
library(stars)
library(terra)
library(neuralnet)
library(readxl)
set.seed(300)

# a = full_df[duplicated(full_df[c("Year", "X.soc..g.cm.2..", "X.lat..dec..deg..", "X.long..dec..deg..")]),]

# ggplot(wri_agg, aes(Longitude, Latitude, color = SOC_gcm2)) +
#   geom_point()

# Read in data
full_df = read.csv("all_ndvi_daymet_slope_lulc_geol_clay_03252024.csv")
# full_df = all_ndvi_daymet_slope_lulc_geol_clay
full_df$Latitude = full_df$SOC1987.lat
full_df$Longitude = full_df$SOC1987.lon

locs = unique(full_df[c("Latitude", "Longitude")])
locs$id_col = 1:6894 #17526
full_df_id = merge(full_df, locs, by = c("Latitude", "Longitude"))

#SOIL MOISTURE HERE
full_df_soilmoist = as.data.frame(SPDF)
sm = full_df_soilmoist$SoilMoisture
# write.csv(sm, "soil_moisture_03252024.csv")
# sm = read.csv("soil_moisture_02092024.csv")

full_df_soilmoist$SoilMoisture = unlist(full_df_soilmoist$SoilMoisture)

#Add CMIP6 model to downscale
# soc_ens = rast("hist/cSoilFast_Lmon_ensemble_historical_rensemblei1p1f2_gn_185001-201412_remapbil.nc") #kg/m2 carbon mass


sample <- sample.int(n = nrow(locs), size = floor(.6*nrow(locs)), replace = F)
locs_tr  <- locs[sample, ]
locs_ts  <- locs[-sample, ]

gens = as.data.frame(unique(full_df_soilmoist$Geol))
gens = as.data.frame(gens[-1,])
gens$Bedrock = 1:28
gens$Geol = gens$`gens[-1, ]`
full_df_id_p = merge(full_df_soilmoist, gens, by = c("Geol"))

full_nona = full_df_id_p[!is.na(full_df_id_p$SOC1987.NDVI_max_y5),]
full_nona_again = full_nona #rbind(full_nona_nw, wosis_combo)

full_nona_again = full_nona_again[full_nona_again$SOC1987.NDVI_mean >= 0, ]
full_nona_again = full_nona_again[full_nona_again$SOC1987.NDVI_mean_y1 >= 0, ]
full_nona_again = full_nona_again[full_nona_again$SOC1987.NDVI_mean_y2 >= 0, ]
full_nona_again = full_nona_again[full_nona_again$SOC1987.NDVI_mean_y3 >= 0, ]
full_nona_again = full_nona_again[full_nona_again$SOC1987.NDVI_mean_y4 >= 0, ]
full_nona_again = full_nona_again[full_nona_again$SOC1987.NDVI_mean_y5 >= 0, ]

full_nona_again = full_nona_again[full_nona_again$SOC1987.NDVI_max >= 0, ]
full_nona_again = full_nona_again[full_nona_again$SOC1987.NDVI_max_y1 >= 0, ]
full_nona_again = full_nona_again[full_nona_again$SOC1987.NDVI_max_y2 >= 0, ]
full_nona_again = full_nona_again[full_nona_again$SOC1987.NDVI_max_y3 >= 0, ]
full_nona_again = full_nona_again[full_nona_again$SOC1987.NDVI_max_y4 >= 0, ]
full_nona_again = full_nona_again[full_nona_again$SOC1987.NDVI_max_y5 >= 0, ]

full_nona_again = full_nona_again[full_nona_again$SOC1987.NDVI_min >= 0, ]
full_nona_again = full_nona_again[full_nona_again$SOC1987.NDVI_min_y1 >= 0, ]
full_nona_again = full_nona_again[full_nona_again$SOC1987.NDVI_min_y2 >= 0, ]
full_nona_again = full_nona_again[full_nona_again$SOC1987.NDVI_min_y3 >= 0, ]
full_nona_again = full_nona_again[full_nona_again$SOC1987.NDVI_min_y4 >= 0, ]
full_nona_again = full_nona_again[full_nona_again$SOC1987.NDVI_min_y5 >= 0, ]

full_nona_again$Precip_log = log10(full_nona_again$SOC1987.Precip)
full_nona_again$Precip_y1_log = log10(full_nona_again$SOC1987.Precip_yr1)
full_nona_again$Precip_y2_log = log10(full_nona_again$SOC1987.Precip_yr2)
full_nona_again$Precip_y3_log = log10(full_nona_again$SOC1987.Precip_yr3)
full_nona_again$Precip_y4_log = log10(full_nona_again$SOC1987.Precip_yr4)
full_nona_again$Precip_y5_log = log10(full_nona_again$SOC1987.Precip_yr5)

full_nona_again = full_nona_again[full_nona_again$SOC1987.Elev > 0, ]
full_nona_again$Elev_log = log10(full_nona_again$SOC1987.Elev)
full_nona_again$Slope_log = log10(full_nona_again$SOC1987.Slope)
full_nona_again$SOC_kgm2_log = log10(full_nona_again$SOC1987.SOC_kgm2)

training = full_nona_again[full_nona_again$id_col %in% locs_tr$id_col, ]
testing = full_nona_again[full_nona_again$id_col %in% locs_ts$id_col, ]
training = training[complete.cases(training),]
testing = testing[complete.cases(testing),]

train1 = training %>% select(c("SOC1987.NDVI_mean", "SOC1987.NDVI_mean_y1", "SOC1987.NDVI_mean_y2", "SOC1987.NDVI_mean_y3",
                                 "SOC1987.NDVI_mean_y4", "SOC1987.NDVI_mean_y5", "SOC1987.NDVI_min", "SOC1987.NDVI_min_y1",
                                 "SOC1987.NDVI_min_y2", "SOC1987.NDVI_min_y3", "SOC1987.NDVI_min_y4", "SOC1987.NDVI_min_y5",
                                 "SOC1987.NDVI_max","SOC1987.NDVI_max_y1", "SOC1987.NDVI_max_y2", "SOC1987.NDVI_max_y3",
                                 "SOC1987.NDVI_max_y4", "SOC1987.NDVI_max_y5", "Longitude", "Latitude", 
                               "LULC_y0", "LULC_y1", "LULC_y2", "LULC_y3", "LULC_y4", "LULC_y5",
                                 "SOC1987.Tmin", "SOC1987.Tmin_yr1", "SOC1987.Tmin_yr2", "SOC1987.Tmin_yr3", 
                                 "SOC1987.Tmin_yr4","SOC1987.Tmin_yr5", "SOC1987.Tmax", "SOC1987.Tmax_yr1", "SOC1987.Tmax_yr2", "SOC1987.Tmax_yr3", 
                                 "SOC1987.Tmax_yr4", "SOC1987.Tmax_yr5","Precip_log", "Precip_y1_log", "Precip_y2_log", "Precip_y3_log", "Precip_y4_log", 
                                 "Precip_y5_log", "SOC_kgm2_log", "SoilMoisture"))
test1 = testing %>% select(c("SOC1987.NDVI_mean", "SOC1987.NDVI_mean_y1", "SOC1987.NDVI_mean_y2", "SOC1987.NDVI_mean_y3",
                                 "SOC1987.NDVI_mean_y4", "SOC1987.NDVI_mean_y5", "SOC1987.NDVI_min", "SOC1987.NDVI_min_y1",
                                 "SOC1987.NDVI_min_y2", "SOC1987.NDVI_min_y3", "SOC1987.NDVI_min_y4", "SOC1987.NDVI_min_y5",
                                 "SOC1987.NDVI_max","SOC1987.NDVI_max_y1", "SOC1987.NDVI_max_y2", "SOC1987.NDVI_max_y3",
                                 "SOC1987.NDVI_max_y4", "SOC1987.NDVI_max_y5", "Longitude", "Latitude", 
                             "LULC_y0", "LULC_y1", "LULC_y2", "LULC_y3", "LULC_y4", "LULC_y5",
                               "SOC1987.Tmin", "SOC1987.Tmin_yr1", "SOC1987.Tmin_yr2", "SOC1987.Tmin_yr3", 
                               "SOC1987.Tmin_yr4","SOC1987.Tmin_yr5", "SOC1987.Tmax", "SOC1987.Tmax_yr1", "SOC1987.Tmax_yr2", "SOC1987.Tmax_yr3", 
                               "SOC1987.Tmax_yr4", "SOC1987.Tmax_yr5", "Precip_log", "Precip_y1_log", "Precip_y2_log", "Precip_y3_log", "Precip_y4_log", 
                               "Precip_y5_log", "SOC_kgm2_log", "SoilMoisture"))
#################################################
usa <- map_data("usa")

ggplot() +
  geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = NA, color = "black") +
  coord_fixed(1.3) +
  geom_point(data = training, aes(Longitude, Latitude)) +
  theme_classic() + 
  theme(text = element_text(size=20)) + 
  ylab("Latitude") + 
  xlab("Longitude")

testing = testing[testing$Longitude <= -65,]

ggplot() +
  geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = NA, color = "black") +
  coord_fixed(1.3) +
  geom_point(data = testing, aes(Longitude, Latitude)) +
  theme_classic() + 
  theme(text = element_text(size=20)) + 
  ylab("Latitude") + 
  xlab("Longitude")
#
# hist(as.numeric(training$Year))
# hist(as.numeric(testing$Year))

# varImp(mod, scale = FALSE) #caret package

##########################################################################
#Random Forest Construction
######################################################################
RFtraining <- randomForest(SOC_kgm2_log ~ .,
                           data = train1, ntree=128,
                           na.action = na.exclude, importance = TRUE)

# save(RFtraining, file = "RF_03252024.RData")

# grid_rf <- expand.grid(mtry = c(1:20))
# train_ctrl <- trainControl(method="cv",
#                            number=5, 
#                            search = "grid")
# RF_model <- train(SOC_gcm2 ~ .,
#                        data = training_2,
#                        method = "rf", # this will use the randomForest::randomForest function
#                        metric = "RMSE", # which metric should be optimized for 
#                        trControl = train_ctrl, 
#                        tuneGrid = grid_rf,
#                        # options to be passed to randomForest
#                        ntree = 741,
#                        keep.forest=TRUE,
#                        importance=TRUE
# ) 


#Now, predict OOB values using the RF object
TestingPredict <- predict(RFtraining, test1, type="response", predict.all=FALSE)
TestingPredictData <- as.data.frame(TestingPredict)
test1$RFpredicted <- TestingPredictData$TestingPredict
#appends RF build data with predicteds from RF model
train1$RFpredicted <- RFtraining$predicted

#2d histogram plot build. This is the main way we visualize RF performance
FigureRF <- ggplot(train1, aes(SOC_kgm2_log, RFpredicted)) + 
  geom_bin2d(bins = 50 ) +
  scale_fill_gradient(low = "midnightblue", high = "cyan", limits = c(0,600)) + 
  xlab("Measured SOC (g/cm2)") + 
  ylab("Predicted SOC (g/cm2)") +
  theme_bw() +
  geom_abline(intercept = 0, slope = 1, linewidth = 1, color = "red") +
  xlim(-3,3) + ylim(-3,3)

#2d histogram plot build. Here we are building a similar figure as before, but with OOB data
FigureRFTesting <- ggplot(test1, aes(SOC_kgm2_log, RFpredicted)) + 
  geom_bin2d(bins = 50 ) +
  scale_fill_gradient(low = "tomato4", high = "darkgoldenrod1", limits = c(0,100)) + 
  xlab("Measured SOC (g/cm2)") + ylab("Predicted SOC (g/cm2)") +
  theme_bw() +
  geom_abline(intercept = 0, slope = 1, linewidth = 1, color = "red") + 
  xlim(-2,2) + ylim(-2,2)

#place both plots, 90% and 10% side by side using grid.arrange
RF_fig <- grid.arrange(FigureRF, FigureRFTesting, ncol=2)

varImpPlot(RFtraining)
importance(RFtraining)

# #rsq function
rsq <- function(x,y) summary(lm(y~x))$r.squared

#R2
rsq(train1$SOC_kgm2_log, train1$RFpredicted)
rsq(test1$SOC_kgm2_log, test1$RFpredicted)
#MAE
sum(abs(train1$SOC_kgm2_log - train1$RFpredicted))/length(train1$SOC_kgm2_log)
sum(abs(test1$SOC_kgm2_log - test1$RFpredicted), na.rm = TRUE)/length(test1$SOC_kgm2_log)
#RMSE
rmse(train1$SOC_kgm2_log, train1$RFpredicted)
rmse(test1$SOC_kgm2_log, test1$RFpredicted)
#slope
sum((train1$SOC_kgm2_log - mean(train1$SOC_kgm2_log)) * 
(train1$RFpredicted - mean(train1$RFpredicted))) /
  sum((train1$SOC_kgm2_log - mean(train1$SOC_kgm2_log))^2)
sum((test1$SOC_kgm2_log - mean(test1$SOC_kgm2_log)) * 
(test1$RFpredicted - mean(test1$RFpredicted))) /
  sum((test1$SOC_kgm2_log - mean(test1$SOC_kgm2_log))^2)


###########################################################
#Linear Regression Construction
###########################################################
LMtraining <- lm(SOC_kgm2_log ~ .,data = train1)

LMTestingPredict <- predict(LMtraining, test1)
LMTestingPredictData <- as.data.frame(LMTestingPredict)
test1$LMpredicted <- LMTestingPredictData$LMTestingPredict
#appends LM build data with predicteds from LM model
train1$LMpredicted <- LMtraining$fitted.values

#2d histogram plot build. This is the main way we visualize performance
FigureLM <- ggplot(training, aes(SOC_kgm2_log, LMpredicted)) + 
  geom_bin2d(bins = 50 ) +
  scale_fill_gradient(low = "midnightblue", high = "cyan", limits = c(0,600)) + 
  xlab("Measured SOC (g/cm2)") + 
  ylab("Predicted SOC (g/cm2)") +
  theme_bw() +
  geom_abline(intercept = 0, slope = 1, linewidth = 1, color = "red")
  # xlim(0,20) + ylim(0,20)
# FigureLM
#Now, predict OOB values using the LM object

#2d histogram plot build. Here we are building a similar figure as before, but with OOB data
FigureLMTesting <- ggplot(testing, aes(SOC_kgm2_log, LMpredicted)) + 
  geom_bin2d(bins = 50) +
  scale_fill_gradient(low = "tomato4", high = "darkgoldenrod1", limits = c(0,100)) +
  xlab("Measured SOC (g/cm2)") + ylab("Predicted SOC (g/cm2)") +
  theme_bw() +
  geom_abline(intercept = 0, slope = 1, linewidth = 1, color = "red")
  # xlim(0,20) + ylim(0,20)
# FigureLMTesting
#place both plots, 80% and 20% side by side using grid.arrange
LM_fig <- grid.arrange(FigureLM, FigureLMTesting, ncol=2)

# R2
rsq(train1$SOC_kgm2_log, train1$LMpredicted)
rsq(test1$SOC_kgm2_log, test1$LMpredicted)
# MAE
sum(abs(train1$SOC_kgm2_log - train1$LMpredicted))/length(train1$SOC_gcm2)
sum(abs(test1$SOC_gcm2 - test1$LMpredicted))/length(test1$SOC_gcm2)
# RMSE
rmse(train1$SOC_gcm2, train1$LMpredicted)                             
rmse(test1$SOC_gcm2, test1$LMpredicted)
# Slope
sum((train1$SOC_gcm2 - mean(train1$SOC_gcm2)) * (train1$LMpredicted - mean(train1$LMpredicted))) /
  sum((train1$SOC_gcm2 - mean(train1$SOC_gcm2))^2)
sum((test1$SOC_gcm2 - mean(test1$SOC_gcm2)) * (test1$LMpredicted - mean(test1$LMpredicted))) /
  sum((test1$SOC_gcm2 - mean(test1$SOC_gcm2))^2)
###########################################################
#Gradient Boosting 1
###########################################################
Info_numeric <- train1 %>% dplyr::select(-c("SOC_kgm2_log"))
dtrain <- xgb.DMatrix(data = as.matrix(Info_numeric), label = train1$SOC_kgm2_log)

Info_numeric_test <- test1 %>% dplyr::select(-c("SOC_kgm2_log"))

dtest <- xgb.DMatrix(data = as.matrix(Info_numeric_test), label = test1$SOC_kgm2_log)

GBTraining <- xgboost(
    data = dtrain,
    nrounds = 2500,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 2,
    eta = .2
  )   

#Now, predict OOB values using the RF object
TestingGBPredict <- predict(GBTraining, dtest, type="response", predict.all=FALSE)
TestingGBPredictData <- as.data.frame(TestingGBPredict)
test1$GBpredicted <- TestingGBPredictData$TestingGBPredict
train1$GBpredicted <- predict(GBTraining, dtrain, type="response", predict.all=FALSE)

importance_matrix = xgb.importance(colnames(dtrain), model = GBTraining)

FigureGB <- ggplot(training, aes(SOC_kgm2_log, GBpredicted)) + 
  geom_bin2d(bins = 50 ) +
  scale_fill_gradient(low = "midnightblue", high = "cyan", limits = c(0,600)) + 
  xlab("Measured SOC (g/cm2)") + 
  ylab("Predicted SOC (g/cm2)") +
  theme_bw() +
  geom_abline(intercept = 0, slope = 1, linewidth = 1, color = "red")  
# xlim(0,20) + ylim(0,20)

#2d histogram plot build. Here we are building a similar figure as before, but with OOB data
FigureGBTesting <- ggplot(testing, aes(SOC_kgm2_log, GBpredicted)) + 
  geom_bin2d(bins = 50 ) +
  scale_fill_gradient(low = "tomato4", high = "darkgoldenrod1", limits = c(0,100)) + 
  xlab("Measured SOC (g/cm2)") + ylab("Predicted SOC (g/cm2)") +
  theme_bw() +
  geom_abline(intercept = 0, slope = 1, linewidth = 1, color = "red")
# xlim(0,20) + ylim(0,20)

#place both plots, 90% and 10% side by side using grid.arrange
GB_fig <- grid.arrange(FigureGB, FigureGBTesting, ncol=2)

#R2
rsq(train1$SOC_gcm2, train1$GBpredicted)
rsq(test1$SOC_gcm2, test1$GBpredicted)
#MAE
sum(abs(train1$SOC_gcm2 - train1$GBpredicted))/length(train1$SOC_gcm2)
sum(abs(test1$SOC_gcm2 - test1$GBpredicted))/length(test1$SOC_gcm2)
#RMSE
rmse(train1$SOC_gcm2, train1$GBpredicted)
rmse(test1$SOC_gcm2, test1$GBpredicted)
#calculate the slope of predicted and observed
sum((train1$SOC_gcm2 - mean(train1$SOC_gcm2)) * (train1$GBpredicted - mean(train1$GBpredicted))) /
  sum((train1$SOC_gcm2 - mean(train1$SOC_gcm2))^2)
sum((test1$SOC_gcm2 - mean(test1$SOC_gcm2)) * 
(test1$GBpredicted - mean(test1$GBpredicted))) /
  sum((test1$SOC_gcm2 - mean(test1$SOC_gcm2))^2)

###########################################################
#Gradient Boosting 2
###########################################################
GBTraining1 <- gbm(SOC_kgm2_log ~ ., 
                  data = train1, distribution = "gaussian", n.trees = 100, shrinkage = 0.01,
                  interaction.depth = 3, cv.folds = 10)


#Now, predict OOB values using the RF object
TestingGBPredict1 <- predict(GBTraining1, test1)
testingGBPredictData1 <- as.data.frame(TestingGBPredict1)
test1$GBpredicted1 <- testingGBPredictData1$TestingGBPredict1
train1$GBpredicted1 <- predict.gbm(GBTraining1, train1)

FigureGB <- ggplot(training, aes(SOC_kgm2_log, GBpredicted1)) + 
  geom_bin2d(bins = 50 ) +
  scale_fill_gradient(low = "midnightblue", high = "cyan", limits = c(0,600)) + 
  xlab("Measured SOC (g/cm2)") + 
  ylab("Predicted SOC (g/cm2)") +
  theme_bw() +
  geom_abline(intercept = 0, slope = 1, linewidth = 1, color = "red")  
# xlim(0,20) + ylim(0,20)

#2d histogram plot build. Here we are building a similar figure as before, but with OOB data
FigureGBtesting <- ggplot(testing, aes(SOC_kgm2_log, GBpredicted1)) + 
  geom_bin2d(bins = 50 ) +
  scale_fill_gradient(low = "tomato4", high = "darkgoldenrod1", limits = c(0,100)) + 
  xlab("Measured SOC (g/cm2)") + ylab("Predicted SOC (g/cm2)") +
  theme_bw() +
  geom_abline(intercept = 0, slope = 1, linewidth = 1, color = "red")
# xlim(0,20) + ylim(0,20)


#place both plots, 90% and 10% side by side using grid.arrange
GB_fig <- grid.arrange(FigureGB, FigureGBtesting, ncol=2)

summary.gbm(GBTraining1)


#R2
rsq(train1$SOC_kgm2_log, train1$GBpredicted1)
rsq(test1$SOC_kgm2_log, test1$GBpredicted1)
#MAE
sum(abs(train1$SOC_gcm2 - train1$GBpredicted1))/length(train1$SOC_gcm2)
sum(abs(test1$SOC_gcm2 - test1$GBpredicted1))/length(test1$SOC_gcm2)
#RMSE
rmse(train1$SOC_gcm2, train1$GBpredicted1)
rmse(test1$SOC_gcm2, test1$GBpredicted1)
#Slope
sum((train1$SOC_gcm2 - mean(train1$SOC_gcm2)) * (train1$GBpredicted1 - mean(train1$GBpredicted1))) /
  sum((train1$SOC_gcm2 - mean(train1$SOC_gcm2))^2)
sum((test1$SOC_gcm2 - mean(test1$SOC_gcm2)) * 
(test1$GBpredicted1 - mean(test1$GBpredicted1))) /
  sum((test1$SOC_gcm2 - mean(test1$SOC_gcm2))^2)

###########################################################
#Regression Kriging
###########################################################
setwd("~/ArcGIS/Projects/FAA_SOC")

krig_test = read_excel("testing_101023_krig.xls")
krig_train = read_excel("training_101023_krig.xls")

FigureK <- ggplot(krig_train, aes(SOC_kgm2_log, GALayerToRas1_1)) + 
  geom_bin2d(bins = 50 ) +
  scale_fill_gradient(low = "midnightblue", high = "cyan", limits = c(0,600)) + 
  xlab("Measured SOC (g/cm2)") + 
  ylab("Predicted SOC (g/cm2)") +
  theme_bw() +
  geom_abline(intercept = 0, slope = 1, linewidth = 1, color = "red")  
# xlim(0,20) + ylim(0,20)

#2d histogram plot build. Here we are building a similar figure as before, but with OOB data
FigureK_test <- ggplot(krig_test, aes(SOC_kgm2_log, GALayerToRas1)) + 
  geom_bin2d(bins = 50 ) +
  scale_fill_gradient(low = "tomato4", high = "darkgoldenrod1", limits = c(0,100)) + 
  xlab("Measured SOC (g/cm2)") + ylab("Predicted SOC (g/cm2)") +
  theme_bw() +
  geom_abline(intercept = 0, slope = 1, linewidth = 1, color = "red")
# xlim(0,20) + ylim(0,20)

#place both plots, 90% and 10% side by side using grid.arrange
GB_fig <- grid.arrange(FigureK, FigureK_test, ncol=2)

rsq(krig_train$SOC_kgm2_log, krig_train$GALayerToRas1_1)
rsq(krig_test$SOC_kgm2_log, krig_test$GALayerToRas1)

sum(abs(krig_train$SOC_kgm2_log - krig_train$GALayerToRas1_1))/length(krig_train$SOC_kgm2_log)
sum(abs(krig_test$SOC_kgm2_log - krig_test$GALayerToRas1), na.rm = TRUE)/length(krig_test$SOC_kgm2_log)

rmse(krig_train$SOC_kgm2_log, krig_train$GALayerToRas1_1)
rmse(krig_test$SOC_kgm2_log, krig_test$GALayerToRas1)

sum((krig_train$SOC_kgm2_log - mean(krig_train$SOC_kgm2_log)) * 
(krig_train$GALayerToRas1_1 - mean(krig_train$GALayerToRas1_1))) /
  sum((krig_train$SOC_kgm2_log - mean(krig_train$SOC_kgm2_log))^2)
sum((krig_test$SOC_kgm2_log - mean(krig_test$SOC_kgm2_log)) * 
(krig_test$GALayerToRas1 - mean(krig_test$GALayerToRas1))) /
  sum((krig_test$SOC_kgm2_log - mean(krig_test$SOC_kgm2_log))^2)

###########################################################
#SVM
###########################################################
# svmfit = svm(SOC_gcm2 ~ .,
#              data = training, kernel = "linear", cost = 10, scale = TRUE)

tune.out <- tune(svm, SOC_kgm2_log ~ ., 
                 data = train1, kernel = "linear", #sigmoid, poly, radial
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
bestmod <- tune.out$best.model
# svm.pred = predict(bestmod, train_svm)

#Now, predict OOB values using the SVM object
TestingSVMPredict <- predict(bestmod, test1)
test1$SVMpredicted <- as.numeric(TestingSVMPredict)
train1$SVMpredicted <- as.numeric(bestmod$fitted)

FigureSVM <- ggplot(training, aes(SOC_kgm2_log, SVMpredicted)) + 
  geom_bin2d(bins = 50 ) +
  scale_fill_gradient(low = "midnightblue", high = "cyan", limits = c(0,600)) + 
  xlab("Measured SOC (g/cm2)") + 
  ylab("Predicted SOC (g/cm2)") +
  theme_bw() +
  geom_abline(intercept = 0, slope = 1, linewidth = 1, color = "red")  
# xlim(0,20) + ylim(0,20)

#2d histogram plot build. Here we are building a similar figure as before, but with OOB data
FigureSVMTesting <- ggplot(test1, aes(SOC_kgm2_log, SVMpredicted)) + 
  geom_bin2d(bins = 50 ) +
  scale_fill_gradient(low = "tomato4", high = "darkgoldenrod1", limits = c(0,100)) + 
  xlab("Measured SOC (g/cm2)") + ylab("Predicted SOC (g/cm2)") +
  theme_bw() +
  geom_abline(intercept = 0, slope = 1, linewidth = 1, color = "red")
# xlim(0,20) + ylim(0,20)

#place both plots, 90% and 10% side by side using grid.arrange
SVM_fig <- grid.arrange(FigureSVM, FigureSVMtest1, ncol=2)

#R2 
rsq(train1$SOC_gcm2, train1$SVMpredicted)
rsq(test1$SOC_gcm2, test1$SVMpredicted)
#MAE
sum(abs(train1$SOC_gcm2 - train1$SVMpredicted))/length(train1$SOC_gcm2)
sum(abs(test1$SOC_gcm2 - test1$SVMpredicted))/length(test1$SOC_gcm2)
#RMSE
rmse(train1$SOC_gcm2, train1$SVMpredicted)
rmse(test1$SOC_gcm2, test1$SVMpredicted)
#Slope
sum((train1$SOC_gcm2 - mean(train1$SOC_gcm2)) * 
(train1$SVMpredicted - mean(train1$SVMpredicted))) /
  sum((train1$SOC_gcm2 - mean(train1$SOC_gcm2))^2)
sum((test1$SOC_gcm2 - mean(test1$SOC_gcm2)) * 
(test1$SVMpredicted - mean(test1$SVMpredicted))) /
  sum((test1$SOC_gcm2 - mean(test1$SOC_gcm2))^2)
###########################################################
#KNN
###########################################################

trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
grid <- expand.grid(.k=seq(1,20,by=1))
fit.knn <- train(SOC_kgm2_log ~ . , data=train1, method="knn", tuneGrid=grid, trControl=trainControl)
knn.k2 <- fit.knn$bestTune # keep this optimal k for test1 with stand alone knn() function in next section
print(fit.knn)

prediction <- predict(fit.knn, newdata = test1)
test1$knn_pred = prediction
train1$knn_pred = fit.knn$trainingData
train1$outcome = train1$knn_pred$.outcome

Figureknn <- ggplot(train1, aes(SOC_kgm2_log, outcome)) + 
  geom_bin2d(bins = 50 ) +
  scale_fill_gradient(low = "midnightblue", high = "cyan", limits = c(0,600)) + 
  xlab("Measured SOC (g/cm2)") + 
  ylab("Predicted SOC (g/cm2)") +
  theme_bw() +
  geom_abline(intercept = 0, slope = 1, linewidth = 1, color = "red")  

#2d histogram plot build. Here we are building a similar figure as before, but with OOB data
Figureknntest1 <- ggplot(test1, aes(SOC_kgm2_log, knn_pred)) + 
  geom_bin2d(bins = 50 ) +
  scale_fill_gradient(low = "tomato4", high = "darkgoldenrod1", limits = c(0,100)) + 
  xlab("Measured SOC (g/cm2)") + ylab("Predicted SOC (g/cm2)") +
  theme_bw() +
  geom_abline(intercept = 0, slope = 1, linewidth = 1, color = "red")

knn_fig <- grid.arrange(Figureknn, Figureknntest1, ncol=2)

#R2
rsq(train1$SOC_gcm2, train1$outcome) #R2 
rsq(test1$SOC_gcm2, test1$knn_pred) #R2 
#MAE
sum(abs(train1$SOC_gcm2 - train1$outcome))/length(train1$SOC_gcm2)  #MAE
sum(abs(test1$SOC_gcm2 - test1$knn_pred))/length(test1$SOC_gcm2)  #MAE
#RMSE
rmse(train1$SOC_gcm2, train1$outcome)  #RMSE
rmse(test1$SOC_gcm2, test1$knn_pred)  #RMSE

sum((train1$SOC_gcm2 - mean(train1$SOC_gcm2)) * 
(train1$outcome - mean(train1$knn_pred$.outcome))) /
  sum((train1$SOC_gcm2 - mean(train1$SOC_gcm2))^2)
sum((test1$SOC_gcm2 - mean(test1$SOC_gcm2)) * 
(test1$knn_pred - mean(test1$knn_pred))) /
  sum((test1$SOC_gcm2 - mean(test1$SOC_gcm2))^2)

###########################################################
#ANN
###########################################################
k <- 10
cv.error <- matrix(nrow = k, ncol = 4)

maxs <- apply(train1, 2, max, na.rm = TRUE) 
mins <- apply(train1, 2, min, na.rm = TRUE) 
ann_train <- as.data.frame(scale(train1, center = mins, scale = maxs - mins))
maxs_test <- apply(test1, 2, max) 
mins_test <- apply(test1, 2, min)
ann_test <- as.data.frame(scale(test1, center = mins_test, scale = maxs_test - mins_test))

folds <- sample(1:k, nrow(ann_train), replace = TRUE)

for(i in 1:k){
  print(i)
  nn <- neuralnet(SOC_kgm2_log ~ .,
                  data=ann_train,
                  hidden=1,
                  linear.output=T, stepmax=10000000)
  print("nn")
  pr.nn <- neuralnet::compute(nn,ann_test)
  pr.nn_res <- pr.nn$net.result*(max(ann_train$SOC_gcm2)-min(ann_train$SOC_gcm2))+min(ann_train$SOC_gcm2)
  test.cv.r <- (ann_test$SOC_gcm2)*(max(ann_train$SOC_gcm2)-min(ann_train$SOC_gcm2))+min(ann_train$SOC_gcm2)
  # cv.error  cv.error[i,] <- c(RMSE(nn$response, nn$net.result[[1]]), 
                    # MAE(nn$response, nn$net.result[[1]]),
  #                  rsq(nn$response, nn$net.result[[1]]), 
  #                  sum((nn$response - mean(nn$response)) * (nn$net.result[[1]] - mean(nn$net.result[[1]]))) /
  #                    sum((nn$response - mean(nn$response))^2))
  cv.error[i,] <- c(RMSE(ann_test$SOC_gcm2, pr.nn$net.result), 
                    MAE(ann_test$SOC_gcm2, pr.nn$net.result),
                    rsq(ann_test$SOC_gcm2, pr.nn$net.result), 
                    sum((ann_test$SOC_gcm2 - mean(ann_test$SOC_gcm2)) * 
                          (pr.nn$net.result - mean(pr.nn$net.result))) /
                      sum((ann_test$SOC_gcm2 - mean(ann_test$SOC_gcm2))^2))
  
}

mean(cv.error[,1], na.rm = TRUE) #RMSE
mean(cv.error[,2], na.rm = TRUE) #MAE
mean(cv.error[,3], na.rm = TRUE) #R2
mean(cv.error[,4], na.rm = TRUE) #Slope

results = as.data.frame(nn$data)
results$outputs = nn$net.result[[1]]
test_results = as.data.frame(ann_test$SOC_kgm2_log)
test_results$outputs = pr.nn$net.result

FigureANN  = ggplot(results, aes(SOC_kgm2_log, outputs)) + 
  geom_bin2d(bins = 50 ) +
  scale_fill_gradient(low = "midnightblue", high = "cyan", limits = c(0,600)) + 
  xlab("Measured SOC (g/cm2)") + 
  ylab("Predicted SOC (g/cm2)") +
  theme_bw() +
  geom_abline(intercept = 0, slope = 1, linewidth = 1, color = "red")

#2d histogram plot build. Here we are building a similar figure as before, but with OOB data
FigureANNTesting <- ggplot(test_results, aes(`ann_test$SOC_kgm2_log`, outputs)) + 
  geom_bin2d(bins = 50) +
  scale_fill_gradient(low = "tomato4", high = "darkgoldenrod1", limits = c(0,100)) +
  xlab("Measured SOC (g/cm2)") + ylab("Predicted SOC (g/cm2)") +
  theme_bw() +
  geom_abline(intercept = 0, slope = 1, linewidth = 1, color = "red")

LM_fig <- grid.arrange(FigureANN, FigureANNTesting, ncol=2)

#R2
rsq(ann_train$SOC_kgm2_log,  )
rsq(ann_test$SOC_gcm2, test_results$outputs)

#MAE
sum(abs(ann_train$SOC_kgm2_log - ann_train$outcome))/length(ann_train$SOC_gcm2)  #MAE
sum(abs(ann_test$SOC_kgm2_log - test_results$outputs))/length(test_results$outputs)  #MAE
#RMSE
rmse(ann_train$SOC_gcm2, ann_train$outcome)  #RMSE
rmse(ann_test$SOC_gcm2, test_results$outputs)  #RMSE

sum((ann_train$SOC_gcm2 - mean(ann_train$SOC_gcm2)) * (ann_train$outcome - mean(ann_train$knn_pred$.outcome))) /
  sum((ann_train$SOC_gcm2 - mean(ann_train$SOC_gcm2))^2)
sum((ann_test$SOC_gcm2 - mean(ann_test$SOC_gcm2)) * (test_results$outputs - mean(test_results$outputs))) /
  sum((ann_test$SOC_gcm2 - mean(ann_test$SOC_gcm2))^2)
#RMSE
#Slope

#########################################################

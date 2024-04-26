<<<<<<< HEAD
setwd("C:/Users/User/Documents/SOC")
library(terra)
library(ggplot2)
library(ncdf4)
library(tidyterra)

#hist
soc_ukesm = rast("hist/cSoilFast_Lmon_UKESM1-0-LL_historical_rensemblei1p1f2_gn_185001-201412_remapbil.nc") #kg/m2 carbon mass
soc_miroc = rast("hist/cSoilFast_Lmon_MIROC-ES2L_historical_rensemblei1p1f2_gn_185001-201412.nc") #kg/m2 carbon mass
soc_cesm2 = rast("hist/cSoilFast_Lmon_CESM2_historical_rensemblei1p1f1_gn_185001-201412_remapbil.nc") #kg/m2 carbon mass
soc_access = rast("hist/cSoilFast_Lmon_ACCESS-ESM1-5_historical_rensemblei1p1f1_gn_185001-201412_remapbil.nc") #kg/m2 carbon mass
soc_noresm = rast("hist/cSoilFast_Lmon_NorESM2-LM_historical_rensemblei1p1f1_gn_185001-201412_remapbil.nc") #kg/m2 carbon mass
soc_ens = rast("hist/cSoilFast_Lmon_ensemble_historical_rensemblei1p1f2_gn_185001-201412_remapbil.nc") #kg/m2 carbon mass

#ssp126
soc_ukesm_126 = rast("ssp126/cSoilFast_Lmon_UKESM1-0-LL_ssp126_rensi1p1f2_gn_201501-210012_remapbil.nc")
soc_miroc_126 = rast("ssp126/cSoilFast_Lmon_MIROC-ES2L_ssp126_rensi1p1f2_gn_201501-210012.nc")
soc_cesm2_126 = rast("ssp126/cSoilFast_Lmon_CESM2_ssp126_rensi1p1f1_gn_201501-210012_remapbil.nc")
soc_access_126 = rast("ssp126/cSoilFast_Lmon_ACCESS-ESM1-5_ssp126_rensi1p1f1_gn_201501-210012_remapbil.nc")
soc_noresm_126 = rast("ssp126/cSoilFast_Lmon_NorESM2-LM_ssp126_r1i1p1f1_gn_201501-210012_remapbil.nc")
soc_ens_126 = rast("ssp126/cSoilFast_Lmon_ensemble_ssp126_rensi1p1f2_gn_201501-210012_remapbil.nc")

#ssp126-370lu
soc_ukesm_126_370 = rast("ssp126-370lu/cSoilFast_Lmon_UKESM1-0-LL_ssp126-ssp370Lu_rensi1p1f2_gn_201501-210012_remapbil.nc")
soc_miroc_126_370 = rast("ssp126-370lu/cSoilFast_Lmon_MIROC-ES2L_ssp126-ssp370Lu_r1i1p1f2_gn_201501-210012.nc")
soc_cesm2_126_370 = rast("ssp126-370lu/cSoilFast_Lmon_CESM2_ssp126-ssp370Lu_rensi1p1f1_gn_201501-210012_remapbil.nc")
soc_access_126_370 = rast("ssp126-370lu/cSoilFast_Lmon_ACCESS-ESM1-5_ssp126-ssp370Lu_rensi1p1f1_gn_201501-210012_remapbil.nc")
soc_noresm_126_370 = rast("ssp126-370lu/cSoilFast_Lmon_NorESM2-LM_ssp126-ssp370Lu_r1i1p1f1_gn_201501-209912_remapbil.nc")
soc_ens_126_370 = rast("ssp126-370lu/cSoilFast_Lmon_ensmean_ssp126-ssp370Lu_rensi1p1f1_gn_201501-210012.nc")

soc_ukesm_1984_2014_mean = mean(soc_ukesm[[1620:1980]])
soc_miroc_1984_2014_mean = mean(soc_miroc[[1620:1980]])
soc_cesm2_1984_2014_mean = mean(soc_cesm2[[1620:1980]])
soc_access_1984_2014_mean = mean(soc_access[[1620:1980]])
soc_noresm_1984_2014_mean = mean(soc_noresm[[1620:1980]])
soc_ens_1984_2014_mean = mean(soc_ens[[1620:1980]])

soc_ukesm_ssp126_2015_2045_mean = mean(soc_ukesm_126[[672:1032]])
soc_miroc_ssp126_2015_2045_mean = mean(soc_miroc_126[[672:1032]])
soc_cesm2_ssp126_2015_2045_mean = mean(soc_cesm2_126[[672:1032]])
soc_access_ssp126_2015_2045_mean = mean(soc_access_126[[672:1032]])
soc_noresm_ssp126_2015_2045_mean = mean(soc_noresm_126[[672:1032]])
soc_ens_ssp126_2015_2045_mean = mean(soc_ens_126[[672:1032]])

soc_ukesm_ssp126_370_2015_2045_mean = mean(soc_ukesm_126_370[[672:1032]])
soc_miroc_ssp126_370_2015_2045_mean = mean(soc_miroc_126_370[[672:1032]])
soc_cesm2_ssp126_370_2015_2045_mean = mean(soc_cesm2_126[[672:1032]])
soc_access_ssp126_370_2015_2045_mean = mean(soc_access_126_370[[672:1032]])
soc_noresm_ssp126_370_2015_2045_mean = mean(soc_noresm_126_370[[672:1032]])
soc_ens_ssp126_370_2015_2045_mean = mean(soc_ens_126_370[[672:1032]])

usa = vect("~/ArcGIS/s_11au16.shp")
usa_pr = project(usa, crs(soc_ens_1984_2014_mean))
soc_ens_1984_2014_mean_r = rotate(soc_ens_1984_2014_mean)
usa_soc_ens = mask(soc_ens_1984_2014_mean_r, usa_pr)

soc_noresm_1984_2014_mean_r = rotate(soc_noresm_1984_2014_mean)
usa_soc_noresm = mask(soc_noresm_1984_2014_mean_r, usa_pr)

soc_ukesm_1984_2014_mean_r = rotate(soc_ukesm_1984_2014_mean)
usa_soc_ukesm = mask(soc_ukesm_1984_2014_mean_r, usa_pr)

soc_miroc_1984_2014_mean_r = rotate(soc_miroc_1984_2014_mean)
usa_soc_miroc = mask(soc_miroc_1984_2014_mean_r, usa_pr)

soc_cesm2_1984_2014_mean_r = rotate(soc_cesm2_1984_2014_mean)
usa_soc_cesm2 = mask(soc_cesm2_1984_2014_mean_r, usa_pr)

soc_access_1984_2014_mean_r = rotate(soc_access_1984_2014_mean)
usa_soc_access = mask(soc_access_1984_2014_mean_r, usa_pr)

###SSP126
soc_noresm_fut_mean_r = rotate(soc_noresm_ssp126_2015_2045_mean)
usa_soc_noresm_ssp126 = mask(soc_noresm_fut_mean_r, usa_pr)

soc_ukesm_fut_mean_r = rotate(soc_ukesm_ssp126_2015_2045_mean)
usa_soc_ukesm_ssp126 = mask(soc_ukesm_fut_mean_r, usa_pr)

soc_miroc_fut_mean_r = rotate(soc_miroc_ssp126_2015_2045_mean)
usa_soc_miroc_ssp126 = mask(soc_miroc_fut_mean_r, usa_pr)

soc_cesm2_fut_mean_r = rotate(soc_cesm2_ssp126_2015_2045_mean)
usa_soc_cesm2_ssp126 = mask(soc_cesm2_fut_mean_r, usa_pr)

soc_access_fut_mean_r = rotate(soc_access_ssp126_2015_2045_mean)
usa_soc_access_ssp126 = mask(soc_access_fut_mean_r, usa_pr)

soc_ens_fut_mean_r = rotate(soc_ens_ssp126_2015_2045_mean)
usa_soc_ens_fut_ssp126 = mask(soc_ens_fut_mean_r, usa_pr)

###SSP126-370LU
soc_noresm_fut_mean_r_370 = rotate(soc_noresm_ssp126_370_2015_2045_mean)
usa_soc_noresm_ssp126_370 = mask(soc_noresm_fut_mean_r_370, usa_pr)

soc_ukesm_fut_mean_r_370 = rotate(soc_ukesm_ssp126_370_2015_2045_mean)
usa_soc_ukesm_ssp126_370 = mask(soc_ukesm_fut_mean_r_370, usa_pr)

soc_miroc_fut_mean_r_370 = rotate(soc_miroc_ssp126_370_2015_2045_mean)
usa_soc_miroc_ssp126_370 = mask(soc_miroc_fut_mean_r_370, usa_pr)

soc_cesm2_fut_mean_r_370 = rotate(soc_cesm2_ssp126_370_2015_2045_mean)
usa_soc_cesm2_ssp126_370 = mask(soc_cesm2_fut_mean_r_370, usa_pr)

soc_access_fut_mean_r_370 = rotate(soc_access_ssp126_370_2015_2045_mean)
usa_soc_access_ssp126_370 = mask(soc_access_fut_mean_r_370, usa_pr)

soc_ens_fut_mean_r_370 = rotate(soc_ens_ssp126_370_2015_2045_mean)
usa_soc_ens_fut_ssp126_370 = mask(soc_ens_fut_mean_r_370, usa_pr)

ggplot() +
  geom_spatraster(data = usa_soc_ens, aes(fill = mean)) +
  theme_bw() +
  geom_spatvector(data = usa_pr, fill = NA) +
  theme(legend.position = "bottom") + 
  ylab("Latitude") +
  xlab("Longitude") +
  xlim(-130, -60) +
  ylim(19, 53) +
  scale_fill_gradientn(colors = c("white", "beige", "lightgreen","green4", "darkgreen"))

hist_ukesm = get_nc_data(nc_open("hist/cSoilFast_Lmon_UKESM1-0-LL_historical_rensemblei1p1f2_gn_185001-201412_remapbil.nc"), 
                        "UKESM1-0-LL", "cSoilFast", 1850:2014)
hist_miroc = get_nc_data(nc_open("hist/cSoilFast_Lmon_MIROC-ES2L_historical_rensemblei1p1f2_gn_185001-201412.nc"), 
                         "MIROC-ES2L", "cSoilFast", 1850:2014)
hist_cesm2 = get_nc_data(nc_open("hist/cSoilFast_Lmon_CESM2_historical_rensemblei1p1f1_gn_185001-201412_remapbil.nc"), 
                         "CESM2", "cSoilFast", 1850:2014)
hist_access = get_nc_data(nc_open("hist/cSoilFast_Lmon_ACCESS-ESM1-5_historical_rensemblei1p1f1_gn_185001-201412_remapbil.nc"), 
                         "ACCESS-ESM1-5", "cSoilFast", 1850:2014)
hist_noresm = get_nc_data(nc_open("hist/cSoilFast_Lmon_NorESM2-LM_historical_rensemblei1p1f1_gn_185001-201412_remapbil.nc"), 
                          "NorESM2-LM", "cSoilFast", 1850:2014)
hist_ens = get_nc_data(nc_open("hist/cSoilFast_Lmon_ensemble_historical_rensemblei1p1f2_gn_185001-201412_remapbil.nc"), 
                          "Ensemble", "cSoilFast", 1850:2014)

usa_means = setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("Scenario", "Model", "Mean"))
usa_means[1,] = c("Historical (1984-2014)","Ensemble", mean(hist_ens$Mean[1609:1980]) * 8.0804643e+12)
usa_means[2,] = c("Historical (1984-2014)","MIROC-ES2L", mean(hist_miroc$Mean[1609:1980]) * 8.0804643e+12)
usa_means[3,] = c("Historical (1984-2014)","UKESM1-0-LL", mean(hist_ukesm$Mean[1609:1980]) * 8.0804643e+12)
usa_means[4,] = c("Historical (1984-2014)","CESM2", mean(hist_cesm2$Mean[1609:1980]) * 8.0804643e+12)
usa_means[5,] = c("Historical (1984-2014)","NorESM2-LM", mean(hist_noresm$Mean[1609:1980]) * 8.0804643e+12)
usa_means[6,] = c("Historical (1984-2014)","ACCESS-ESM1-5", mean(hist_access$Mean[1609:1980]) * 8.0804643e+12)

ssp126_ukesm = get_nc_data(nc_open("ssp126/cSoilFast_Lmon_UKESM1-0-LL_ssp126_rensi1p1f2_gn_201501-210012_remapbil.nc"), 
                         "UKESM1-0-LL", "cSoilFast", 2015:2100)
ssp126_miroc = get_nc_data(nc_open("ssp126/cSoilFast_Lmon_MIROC-ES2L_ssp126_rensi1p1f2_gn_201501-210012.nc"), 
                         "MIROC-ES2L", "cSoilFast", 2015:2100)
ssp126_cesm2 = get_nc_data(nc_open("ssp126/cSoilFast_Lmon_CESM2_ssp126_rensi1p1f1_gn_201501-210012_remapbil.nc"), 
                         "CESM2", "cSoilFast", 2015:2100)
ssp126_access = get_nc_data(nc_open("ssp126/cSoilFast_Lmon_ACCESS-ESM1-5_ssp126_rensi1p1f1_gn_201501-210012_remapbil.nc"), 
                          "ACCESS-ESM1-5", "cSoilFast", 2015:2100)
ssp126_noresm = get_nc_data(nc_open("ssp126/cSoilFast_Lmon_NorESM2-LM_ssp126_r1i1p1f1_gn_201501-210012_remapbil.nc"), 
                          "NorESM2-LM", "cSoilFast", 2015:2100)
ssp126_ens = get_nc_data(nc_open("ssp126/cSoilFast_Lmon_ensemble_ssp126_rensi1p1f2_gn_201501-210012_remapbil.nc"), 
                       "Ensemble", "cSoilFast", 2015:2100)

usa_means[7,] = c("SSP126 (2070-2100)","Ensemble", mean(ssp126_ens$Mean[661:1020], na.rm = TRUE) * 8.0804643e+12)
usa_means[8,] = c("SSP126 (2070-2100)","MIROC-ES2L",  mean(ssp126_miroc$Mean[661:1020], na.rm = TRUE) * 8.0804643e+12)
usa_means[9,] = c("SSP126 (2070-2100)","UKESM1-0-LL",  mean(ssp126_ukesm$Mean[661:1020], na.rm = TRUE) * 8.0804643e+12)
usa_means[10,] = c("SSP126 (2070-2100)","CESM2",  mean(ssp126_cesm2$Mean[661:1020], na.rm = TRUE) * 8.0804643e+12)
usa_means[11,] = c("SSP126 (2070-2100)","NorESM2-LM",  mean(ssp126_noresm$Mean[661:1020], na.rm = TRUE) * 8.0804643e+12)
usa_means[12,] = c("SSP126 (2070-2100)","ACCESS-ESM1-5",  mean(ssp126_access$Mean[661:1020], na.rm = TRUE) * 8.0804643e+12)

ssp126370_ukesm = get_nc_data(nc_open("ssp126-370lu/cSoilFast_Lmon_UKESM1-0-LL_ssp126-ssp370Lu_rensi1p1f2_gn_201501-210012_remapbil.nc"), 
                           "UKESM1-0-LL", "cSoilFast", 2015:2100)
ssp126370_miroc = get_nc_data(nc_open("ssp126-370lu/cSoilFast_Lmon_MIROC-ES2L_ssp126-ssp370Lu_r1i1p1f2_gn_201501-210012.nc"), 
                           "MIROC-ES2L", "cSoilFast", 2015:2100)
ssp126370_cesm2 = get_nc_data(nc_open("ssp126-370lu/cSoilFast_Lmon_CESM2_ssp126-ssp370Lu_rensi1p1f1_gn_201501-210012_remapbil.nc"), 
                           "CESM2", "cSoilFast", 2015:2100)
ssp126370_access = get_nc_data(nc_open("ssp126-370lu/cSoilFast_Lmon_ACCESS-ESM1-5_ssp126-ssp370Lu_rensi1p1f1_gn_201501-210012_remapbil.nc"), 
                            "ACCESS-ESM1-5", "cSoilFast", 2015:2100)
ssp126370_noresm = get_nc_data(nc_open("ssp126-370lu/cSoilFast_Lmon_NorESM2-LM_ssp126-ssp370Lu_r1i1p1f1_gn_201501-209912_remapbil.nc"),
                            "NorESM2-LM", "cSoilFast", 2015:2099)
ssp126370_ens = get_nc_data(nc_open("ssp126-370lu/cSoilFast_Lmon_ensmean_ssp126-ssp370Lu_rensi1p1f1_gn_201501-210012.nc"),
                         "Ensemble", "cSoilFast", 2015:2099)

usa_means[13,] = c("SSP126-370Lu (2070-2100)","Ensemble", mean(ssp126370_ens$Mean[661:1020], na.rm = TRUE) * 8.0804643e+12)
usa_means[14,] = c("SSP126-370Lu (2070-2100)","MIROC-ES2L", mean(ssp126370_miroc$Mean[661:1020], na.rm = TRUE) * 8.0804643e+12)
usa_means[15,] = c("SSP126-370Lu (2070-2100)","UKESM1-0-LL", mean(ssp126370_ukesm$Mean[661:1020], na.rm = TRUE) * 8.0804643e+12)
usa_means[16,] = c("SSP126-370Lu (2070-2100)","CESM2", mean(ssp126370_cesm2$Mean[661:1020], na.rm = TRUE) * 8.0804643e+12)
usa_means[17,] = c("SSP126-370Lu (2070-2100)","NorESM2-LM", mean(ssp126370_noresm$Mean[661:1020], na.rm = TRUE) * 8.0804643e+12)
usa_means[18,] = c("SSP126-370Lu (2070-2100)","ACCESS-ESM1-5", mean(ssp126370_access$Mean[661:1020], na.rm = TRUE) * 8.0804643e+12)

rf_soc_2022 = rast("Output/RF_SOC_2022.nc")
rf_soc_1990 = rast("Output/RF_SOC_1990.nc")

rf_mean = (rf_soc_2022 + rf_soc_1990)/2
rf_soc_2022$RF_SOC_2022_2_10 = rf_soc_2022$RF_SOC_2022_2  * 10

ggplot() +
  geom_spatraster(data = rf_soc_2022, aes(fill = RF_SOC_2022)) +
  theme_bw() +
  geom_spatvector(data = usa, fill = NA) +
  theme(legend.position = "bottom") + 
  ylab("Latitude") +
  xlab("Longitude") +
  xlim(-130, -60) +
  ylim(19, 53) +
  scale_fill_gradientn(colors = c("white", "beige", "lightgreen","green4", "darkgreen"))

usa_means[19,] = c("Historical (1984-2014)","RF", (global(rf_mean, sum, na.rm=TRUE))* 1000000)
usa_means$Mean = as.numeric(usa_means$Mean)
usa_means$pg = usa_means$Mean / 1000000000000
  
ggplot(data = usa_means, aes(Model, pg, fill = Model)) + 
  geom_col() + 
  theme_bw() +
  scale_fill_manual(values = c("aquamarine3", "brown3", "black", "grey", "lightskyblue", "olivedrab", "tan3")) +
  ylab("Carbon Mass in Fast Soil Pool (Pg), Mean for CONUS") + 
  facet_wrap(~Scenario) + 
  geom_hline(yintercept = 3.5372108, col = "black", linetype = "dashed")
  

####CHECK UNITS
##########Land Use from https://luh.umd.edu/faq.shtml
ssp370 = rast("multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-AIM-ssp370-2-1-f_gn_2015-2100.nc")
ssp126 = rast("multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-IMAGE-ssp126-2-1-f_gn_2015-2100.nc")

forest = primf + secndf
cropland = c3ann + c3nfx + c3per + c4ann + c4per
grazing = pastr + range
urban
other = primn + secndn

#2015-2100

#land use ssp370 - ssp126  calculate change in area (annual and mean of all years) and correlate with SOC changes

diff = ssp370 - ssp126
gridarea 


=======
setwd("C:/Users/User/Documents/SOC")
library(terra)
library(ggplot2)
library(ncdf4)
library(tidyterra)

#hist
soc_ukesm = rast("hist/cSoilFast_Lmon_UKESM1-0-LL_historical_rensemblei1p1f2_gn_185001-201412_remapbil.nc") #kg/m2 carbon mass
soc_miroc = rast("hist/cSoilFast_Lmon_MIROC-ES2L_historical_rensemblei1p1f2_gn_185001-201412.nc") #kg/m2 carbon mass
soc_cesm2 = rast("hist/cSoilFast_Lmon_CESM2_historical_rensemblei1p1f1_gn_185001-201412_remapbil.nc") #kg/m2 carbon mass
soc_access = rast("hist/cSoilFast_Lmon_ACCESS-ESM1-5_historical_rensemblei1p1f1_gn_185001-201412_remapbil.nc") #kg/m2 carbon mass
soc_noresm = rast("hist/cSoilFast_Lmon_NorESM2-LM_historical_rensemblei1p1f1_gn_185001-201412_remapbil.nc") #kg/m2 carbon mass
soc_ens = rast("hist/cSoilFast_Lmon_ensemble_historical_rensemblei1p1f2_gn_185001-201412_remapbil.nc") #kg/m2 carbon mass

#ssp126
soc_ukesm_126 = rast("ssp126/cSoilFast_Lmon_UKESM1-0-LL_ssp126_rensi1p1f2_gn_201501-210012_remapbil.nc")
soc_miroc_126 = rast("ssp126/cSoilFast_Lmon_MIROC-ES2L_ssp126_rensi1p1f2_gn_201501-210012.nc")
soc_cesm2_126 = rast("ssp126/cSoilFast_Lmon_CESM2_ssp126_rensi1p1f1_gn_201501-210012_remapbil.nc")
soc_access_126 = rast("ssp126/cSoilFast_Lmon_ACCESS-ESM1-5_ssp126_rensi1p1f1_gn_201501-210012_remapbil.nc")
soc_noresm_126 = rast("ssp126/cSoilFast_Lmon_NorESM2-LM_ssp126_r1i1p1f1_gn_201501-210012_remapbil.nc")
soc_ens_126 = rast("ssp126/cSoilFast_Lmon_ensemble_ssp126_rensi1p1f2_gn_201501-210012_remapbil.nc")

#ssp126-370lu
soc_ukesm_126_370 = rast("ssp126-370lu/cSoilFast_Lmon_UKESM1-0-LL_ssp126-ssp370Lu_rensi1p1f2_gn_201501-210012_remapbil.nc")
soc_miroc_126_370 = rast("ssp126-370lu/cSoilFast_Lmon_MIROC-ES2L_ssp126-ssp370Lu_r1i1p1f2_gn_201501-210012.nc")
soc_cesm2_126_370 = rast("ssp126-370lu/cSoilFast_Lmon_CESM2_ssp126-ssp370Lu_rensi1p1f1_gn_201501-210012_remapbil.nc")
soc_access_126_370 = rast("ssp126-370lu/cSoilFast_Lmon_ACCESS-ESM1-5_ssp126-ssp370Lu_rensi1p1f1_gn_201501-210012_remapbil.nc")
soc_noresm_126_370 = rast("ssp126-370lu/cSoilFast_Lmon_NorESM2-LM_ssp126-ssp370Lu_r1i1p1f1_gn_201501-209912_remapbil.nc")
soc_ens_126_370 = rast("ssp126-370lu/cSoilFast_Lmon_ensmean_ssp126-ssp370Lu_rensi1p1f1_gn_201501-210012.nc")

soc_ukesm_1984_2014_mean = mean(soc_ukesm[[1620:1980]])
soc_miroc_1984_2014_mean = mean(soc_miroc[[1620:1980]])
soc_cesm2_1984_2014_mean = mean(soc_cesm2[[1620:1980]])
soc_access_1984_2014_mean = mean(soc_access[[1620:1980]])
soc_noresm_1984_2014_mean = mean(soc_noresm[[1620:1980]])
soc_ens_1984_2014_mean = mean(soc_ens[[1620:1980]])

soc_ukesm_ssp126_2015_2045_mean = mean(soc_ukesm_126[[672:1032]])
soc_miroc_ssp126_2015_2045_mean = mean(soc_miroc_126[[672:1032]])
soc_cesm2_ssp126_2015_2045_mean = mean(soc_cesm2_126[[672:1032]])
soc_access_ssp126_2015_2045_mean = mean(soc_access_126[[672:1032]])
soc_noresm_ssp126_2015_2045_mean = mean(soc_noresm_126[[672:1032]])
soc_ens_ssp126_2015_2045_mean = mean(soc_ens_126[[672:1032]])

soc_ukesm_ssp126_370_2015_2045_mean = mean(soc_ukesm_126_370[[672:1032]])
soc_miroc_ssp126_370_2015_2045_mean = mean(soc_miroc_126_370[[672:1032]])
soc_cesm2_ssp126_370_2015_2045_mean = mean(soc_cesm2_126[[672:1032]])
soc_access_ssp126_370_2015_2045_mean = mean(soc_access_126_370[[672:1032]])
soc_noresm_ssp126_370_2015_2045_mean = mean(soc_noresm_126_370[[672:1032]])
soc_ens_ssp126_370_2015_2045_mean = mean(soc_ens_126_370[[672:1032]])

usa = vect("~/ArcGIS/s_11au16.shp")
usa_pr = project(usa, crs(soc_ens_1984_2014_mean))
soc_ens_1984_2014_mean_r = rotate(soc_ens_1984_2014_mean)
usa_soc_ens = mask(soc_ens_1984_2014_mean_r, usa_pr)

soc_noresm_1984_2014_mean_r = rotate(soc_noresm_1984_2014_mean)
usa_soc_noresm = mask(soc_noresm_1984_2014_mean_r, usa_pr)

soc_ukesm_1984_2014_mean_r = rotate(soc_ukesm_1984_2014_mean)
usa_soc_ukesm = mask(soc_ukesm_1984_2014_mean_r, usa_pr)

soc_miroc_1984_2014_mean_r = rotate(soc_miroc_1984_2014_mean)
usa_soc_miroc = mask(soc_miroc_1984_2014_mean_r, usa_pr)

soc_cesm2_1984_2014_mean_r = rotate(soc_cesm2_1984_2014_mean)
usa_soc_cesm2 = mask(soc_cesm2_1984_2014_mean_r, usa_pr)

soc_access_1984_2014_mean_r = rotate(soc_access_1984_2014_mean)
usa_soc_access = mask(soc_access_1984_2014_mean_r, usa_pr)

###SSP126
soc_noresm_fut_mean_r = rotate(soc_noresm_ssp126_2015_2045_mean)
usa_soc_noresm_ssp126 = mask(soc_noresm_fut_mean_r, usa_pr)

soc_ukesm_fut_mean_r = rotate(soc_ukesm_ssp126_2015_2045_mean)
usa_soc_ukesm_ssp126 = mask(soc_ukesm_fut_mean_r, usa_pr)

soc_miroc_fut_mean_r = rotate(soc_miroc_ssp126_2015_2045_mean)
usa_soc_miroc_ssp126 = mask(soc_miroc_fut_mean_r, usa_pr)

soc_cesm2_fut_mean_r = rotate(soc_cesm2_ssp126_2015_2045_mean)
usa_soc_cesm2_ssp126 = mask(soc_cesm2_fut_mean_r, usa_pr)

soc_access_fut_mean_r = rotate(soc_access_ssp126_2015_2045_mean)
usa_soc_access_ssp126 = mask(soc_access_fut_mean_r, usa_pr)

soc_ens_fut_mean_r = rotate(soc_ens_ssp126_2015_2045_mean)
usa_soc_ens_fut_ssp126 = mask(soc_ens_fut_mean_r, usa_pr)

###SSP126-370LU
soc_noresm_fut_mean_r_370 = rotate(soc_noresm_ssp126_370_2015_2045_mean)
usa_soc_noresm_ssp126_370 = mask(soc_noresm_fut_mean_r_370, usa_pr)

soc_ukesm_fut_mean_r_370 = rotate(soc_ukesm_ssp126_370_2015_2045_mean)
usa_soc_ukesm_ssp126_370 = mask(soc_ukesm_fut_mean_r_370, usa_pr)

soc_miroc_fut_mean_r_370 = rotate(soc_miroc_ssp126_370_2015_2045_mean)
usa_soc_miroc_ssp126_370 = mask(soc_miroc_fut_mean_r_370, usa_pr)

soc_cesm2_fut_mean_r_370 = rotate(soc_cesm2_ssp126_370_2015_2045_mean)
usa_soc_cesm2_ssp126_370 = mask(soc_cesm2_fut_mean_r_370, usa_pr)

soc_access_fut_mean_r_370 = rotate(soc_access_ssp126_370_2015_2045_mean)
usa_soc_access_ssp126_370 = mask(soc_access_fut_mean_r_370, usa_pr)

soc_ens_fut_mean_r_370 = rotate(soc_ens_ssp126_370_2015_2045_mean)
usa_soc_ens_fut_ssp126_370 = mask(soc_ens_fut_mean_r_370, usa_pr)

ggplot() +
  geom_spatraster(data = usa_soc_ens, aes(fill = mean)) +
  theme_bw() +
  geom_spatvector(data = usa_pr, fill = NA) +
  theme(legend.position = "bottom") + 
  ylab("Latitude") +
  xlab("Longitude") +
  xlim(-130, -60) +
  ylim(19, 53) +
  scale_fill_gradientn(colors = c("white", "beige", "lightgreen","green4", "darkgreen"))

hist_ukesm = get_nc_data(nc_open("hist/cSoilFast_Lmon_UKESM1-0-LL_historical_rensemblei1p1f2_gn_185001-201412_remapbil.nc"), 
                        "UKESM1-0-LL", "cSoilFast", 1850:2014)
hist_miroc = get_nc_data(nc_open("hist/cSoilFast_Lmon_MIROC-ES2L_historical_rensemblei1p1f2_gn_185001-201412.nc"), 
                         "MIROC-ES2L", "cSoilFast", 1850:2014)
hist_cesm2 = get_nc_data(nc_open("hist/cSoilFast_Lmon_CESM2_historical_rensemblei1p1f1_gn_185001-201412_remapbil.nc"), 
                         "CESM2", "cSoilFast", 1850:2014)
hist_access = get_nc_data(nc_open("hist/cSoilFast_Lmon_ACCESS-ESM1-5_historical_rensemblei1p1f1_gn_185001-201412_remapbil.nc"), 
                         "ACCESS-ESM1-5", "cSoilFast", 1850:2014)
hist_noresm = get_nc_data(nc_open("hist/cSoilFast_Lmon_NorESM2-LM_historical_rensemblei1p1f1_gn_185001-201412_remapbil.nc"), 
                          "NorESM2-LM", "cSoilFast", 1850:2014)
hist_ens = get_nc_data(nc_open("hist/cSoilFast_Lmon_ensemble_historical_rensemblei1p1f2_gn_185001-201412_remapbil.nc"), 
                          "Ensemble", "cSoilFast", 1850:2014)

usa_means = setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("Scenario", "Model", "Mean"))
usa_means[1,] = c("Historical (1984-2014)","Ensemble", mean(hist_ens$Mean[1609:1980]) * 8.0804643e+12)
usa_means[2,] = c("Historical (1984-2014)","MIROC-ES2L", mean(hist_miroc$Mean[1609:1980]) * 8.0804643e+12)
usa_means[3,] = c("Historical (1984-2014)","UKESM1-0-LL", mean(hist_ukesm$Mean[1609:1980]) * 8.0804643e+12)
usa_means[4,] = c("Historical (1984-2014)","CESM2", mean(hist_cesm2$Mean[1609:1980]) * 8.0804643e+12)
usa_means[5,] = c("Historical (1984-2014)","NorESM2-LM", mean(hist_noresm$Mean[1609:1980]) * 8.0804643e+12)
usa_means[6,] = c("Historical (1984-2014)","ACCESS-ESM1-5", mean(hist_access$Mean[1609:1980]) * 8.0804643e+12)

ssp126_ukesm = get_nc_data(nc_open("ssp126/cSoilFast_Lmon_UKESM1-0-LL_ssp126_rensi1p1f2_gn_201501-210012_remapbil.nc"), 
                         "UKESM1-0-LL", "cSoilFast", 2015:2100)
ssp126_miroc = get_nc_data(nc_open("ssp126/cSoilFast_Lmon_MIROC-ES2L_ssp126_rensi1p1f2_gn_201501-210012.nc"), 
                         "MIROC-ES2L", "cSoilFast", 2015:2100)
ssp126_cesm2 = get_nc_data(nc_open("ssp126/cSoilFast_Lmon_CESM2_ssp126_rensi1p1f1_gn_201501-210012_remapbil.nc"), 
                         "CESM2", "cSoilFast", 2015:2100)
ssp126_access = get_nc_data(nc_open("ssp126/cSoilFast_Lmon_ACCESS-ESM1-5_ssp126_rensi1p1f1_gn_201501-210012_remapbil.nc"), 
                          "ACCESS-ESM1-5", "cSoilFast", 2015:2100)
ssp126_noresm = get_nc_data(nc_open("ssp126/cSoilFast_Lmon_NorESM2-LM_ssp126_r1i1p1f1_gn_201501-210012_remapbil.nc"), 
                          "NorESM2-LM", "cSoilFast", 2015:2100)
ssp126_ens = get_nc_data(nc_open("ssp126/cSoilFast_Lmon_ensemble_ssp126_rensi1p1f2_gn_201501-210012_remapbil.nc"), 
                       "Ensemble", "cSoilFast", 2015:2100)

usa_means[7,] = c("SSP126 (2070-2100)","Ensemble", mean(ssp126_ens$Mean[661:1020], na.rm = TRUE) * 8.0804643e+12)
usa_means[8,] = c("SSP126 (2070-2100)","MIROC-ES2L",  mean(ssp126_miroc$Mean[661:1020], na.rm = TRUE) * 8.0804643e+12)
usa_means[9,] = c("SSP126 (2070-2100)","UKESM1-0-LL",  mean(ssp126_ukesm$Mean[661:1020], na.rm = TRUE) * 8.0804643e+12)
usa_means[10,] = c("SSP126 (2070-2100)","CESM2",  mean(ssp126_cesm2$Mean[661:1020], na.rm = TRUE) * 8.0804643e+12)
usa_means[11,] = c("SSP126 (2070-2100)","NorESM2-LM",  mean(ssp126_noresm$Mean[661:1020], na.rm = TRUE) * 8.0804643e+12)
usa_means[12,] = c("SSP126 (2070-2100)","ACCESS-ESM1-5",  mean(ssp126_access$Mean[661:1020], na.rm = TRUE) * 8.0804643e+12)

ssp126370_ukesm = get_nc_data(nc_open("ssp126-370lu/cSoilFast_Lmon_UKESM1-0-LL_ssp126-ssp370Lu_rensi1p1f2_gn_201501-210012_remapbil.nc"), 
                           "UKESM1-0-LL", "cSoilFast", 2015:2100)
ssp126370_miroc = get_nc_data(nc_open("ssp126-370lu/cSoilFast_Lmon_MIROC-ES2L_ssp126-ssp370Lu_r1i1p1f2_gn_201501-210012.nc"), 
                           "MIROC-ES2L", "cSoilFast", 2015:2100)
ssp126370_cesm2 = get_nc_data(nc_open("ssp126-370lu/cSoilFast_Lmon_CESM2_ssp126-ssp370Lu_rensi1p1f1_gn_201501-210012_remapbil.nc"), 
                           "CESM2", "cSoilFast", 2015:2100)
ssp126370_access = get_nc_data(nc_open("ssp126-370lu/cSoilFast_Lmon_ACCESS-ESM1-5_ssp126-ssp370Lu_rensi1p1f1_gn_201501-210012_remapbil.nc"), 
                            "ACCESS-ESM1-5", "cSoilFast", 2015:2100)
ssp126370_noresm = get_nc_data(nc_open("ssp126-370lu/cSoilFast_Lmon_NorESM2-LM_ssp126-ssp370Lu_r1i1p1f1_gn_201501-209912_remapbil.nc"),
                            "NorESM2-LM", "cSoilFast", 2015:2099)
ssp126370_ens = get_nc_data(nc_open("ssp126-370lu/cSoilFast_Lmon_ensmean_ssp126-ssp370Lu_rensi1p1f1_gn_201501-210012.nc"),
                         "Ensemble", "cSoilFast", 2015:2099)

usa_means[13,] = c("SSP126-370Lu (2070-2100)","Ensemble", mean(ssp126370_ens$Mean[661:1020], na.rm = TRUE) * 8.0804643e+12)
usa_means[14,] = c("SSP126-370Lu (2070-2100)","MIROC-ES2L", mean(ssp126370_miroc$Mean[661:1020], na.rm = TRUE) * 8.0804643e+12)
usa_means[15,] = c("SSP126-370Lu (2070-2100)","UKESM1-0-LL", mean(ssp126370_ukesm$Mean[661:1020], na.rm = TRUE) * 8.0804643e+12)
usa_means[16,] = c("SSP126-370Lu (2070-2100)","CESM2", mean(ssp126370_cesm2$Mean[661:1020], na.rm = TRUE) * 8.0804643e+12)
usa_means[17,] = c("SSP126-370Lu (2070-2100)","NorESM2-LM", mean(ssp126370_noresm$Mean[661:1020], na.rm = TRUE) * 8.0804643e+12)
usa_means[18,] = c("SSP126-370Lu (2070-2100)","ACCESS-ESM1-5", mean(ssp126370_access$Mean[661:1020], na.rm = TRUE) * 8.0804643e+12)

rf_soc_2022 = rast("Output/RF_SOC_2022.nc")
rf_soc_1990 = rast("Output/RF_SOC_1990.nc")

rf_mean = (rf_soc_2022 + rf_soc_1990)/2
rf_soc_2022$RF_SOC_2022_2_10 = rf_soc_2022$RF_SOC_2022_2  * 10

ggplot() +
  geom_spatraster(data = rf_soc_2022, aes(fill = RF_SOC_2022)) +
  theme_bw() +
  geom_spatvector(data = usa, fill = NA) +
  theme(legend.position = "bottom") + 
  ylab("Latitude") +
  xlab("Longitude") +
  xlim(-130, -60) +
  ylim(19, 53) +
  scale_fill_gradientn(colors = c("white", "beige", "lightgreen","green4", "darkgreen"))

usa_means[19,] = c("Historical (1984-2014)","RF", (global(rf_mean, sum, na.rm=TRUE))* 1000000)
usa_means$Mean = as.numeric(usa_means$Mean)
usa_means$pg = usa_means$Mean / 1000000000000
  
ggplot(data = usa_means, aes(Model, pg, fill = Model)) + 
  geom_col() + 
  theme_bw() +
  scale_fill_manual(values = c("aquamarine3", "brown3", "black", "grey", "lightskyblue", "olivedrab", "tan3")) +
  ylab("Carbon Mass in Fast Soil Pool (Pg), Mean for CONUS") + 
  facet_wrap(~Scenario) + 
  geom_hline(yintercept = 3.5372108, col = "black", linetype = "dashed")
  

####CHECK UNITS
##########Land Use from https://luh.umd.edu/faq.shtml
ssp370 = rast("multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-AIM-ssp370-2-1-f_gn_2015-2100.nc")
ssp126 = rast("multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-IMAGE-ssp126-2-1-f_gn_2015-2100.nc")

forest = primf + secndf
cropland = c3ann + c3nfx + c3per + c4ann + c4per
grazing = pastr + range
urban
other = primn + secndn

#2015-2100

#land use ssp370 - ssp126  calculate change in area (annual and mean of all years) and correlate with SOC changes

diff = ssp370 - ssp126
gridarea 


>>>>>>> 095b0e61cdc85d8bfbd63b489b34d6d8f83cd220

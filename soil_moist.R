#Soil Moisture (From ML1.R)

setwd("C:/Users/User/Documents/SOC/Input/soil_moisture/")
soilmoist1984 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-1984-year-fv08.1.nc_annual.nc")
soilmoist1985 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-1985-year-fv08.1.nc_annual.nc")
soilmoist1986 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-1986-year-fv08.1.nc_annual.nc")
soilmoist1987 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-1987-year-fv08.1.nc_annual.nc")
soilmoist1988 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-1988-year-fv08.1.nc_annual.nc")
soilmoist1989 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-1989-year-fv08.1.nc_annual.nc")
soilmoist1990 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-1990-year-fv08.1.nc_annual.nc")
soilmoist1991 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-1991-year-fv08.1.nc_annual.nc")
soilmoist1992 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-1992-year-fv08.1.nc_annual.nc")
soilmoist1993 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-1993-year-fv08.1.nc_annual.nc")
soilmoist1994 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-1994-year-fv08.1.nc_annual.nc")
soilmoist1995 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-1995-year-fv08.1.nc_annual.nc")
soilmoist1996 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-1996-year-fv08.1.nc_annual.nc")
soilmoist1997 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-1997-year-fv08.1.nc_annual.nc")
soilmoist1998 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-1998-year-fv08.1.nc_annual.nc")
soilmoist1999 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-1999-year-fv08.1.nc_annual.nc")
soilmoist2000 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2000-year-fv08.1.nc_annual.nc")
soilmoist2001 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2001-year-fv08.1.nc_annual.nc")
soilmoist2002 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2002-year-fv08.1.nc_annual.nc")
soilmoist2003 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2003-year-fv08.1.nc_annual.nc")
soilmoist2004 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2004-year-fv08.1.nc_annual.nc")
soilmoist2005 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2005-year-fv08.1.nc_annual.nc")
soilmoist2006 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2006-year-fv08.1.nc_annual.nc")
soilmoist2007 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2007-year-fv08.1.nc_annual.nc")
soilmoist2008 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2008-year-fv08.1.nc_annual.nc")
soilmoist2009 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2009-year-fv08.1.nc_annual.nc")
soilmoist2010 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2010-year-fv08.1.nc_annual.nc")
soilmoist2011 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2011-year-fv08.1.nc_annual.nc")
soilmoist2012 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2012-year-fv08.1.nc_annual.nc")
soilmoist2013 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2013-year-fv08.1.nc_annual.nc")
soilmoist2014 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2014-year-fv08.1.nc_annual.nc")
soilmoist2015 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2015-year-fv08.1.nc_annual.nc")
soilmoist2016 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2016-year-fv08.1.nc_annual.nc")
soilmoist2017 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2017-year-fv08.1.nc_annual.nc")
soilmoist2018 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2018-year-fv08.1.nc_annual.nc")
soilmoist2019 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2019-year-fv08.1.nc_annual.nc")
soilmoist2020 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2020-year-fv08.1.nc_annual.nc")
soilmoist2021 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2021-year-fv08.1.nc_annual.nc")
soilmoist2022 = rast("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2022-year-fv08.1.nc_annual.nc")

full_df_id$Lat = full_df_id$Latitude
full_df_id$Lon = full_df_id$Longitude
xy <- full_df_id[c("Longitude", "Latitude")]
df <-full_df_id %>% select(-c("Longitude", "Latitude"))

SPDF <- SpatialPointsDataFrame(coords=xy, data=df)

for (val in 1:nrow(SPDF)){
  dataset = get(paste0("soilmoist", SPDF$Year[val]))
  SPDF$SoilMoisture[val] = terra::extract(rotate(mean(dataset$sm)), coordinates(SPDF[val,])) #method = bilinear)
}




RF_list <- list.files(path="~/Dissertation Work/My Work/FAA/Data/Daymet_tmax", 
                        pattern =".nc", full.names=TRUE)

tmax_list <- list.files(path="~/Dissertation Work/My Work/FAA/Data/Daymet_tmax", 
                        pattern =".tif", full.names=TRUE)

tmax_high_list = list()
for (i in 1:38){
  a = rast(tmax_list)
  a[a < 20] = NA
  tmax_high_list[i] = a
}

RF_list_rast = rast(RF_list)

for (i in 1:33){
  soc = extract(RF_list_rast[[i]], tmax_high_list[i+5])
}
# define raster stack for Nigeria spatial predictions

#### set up #### 
setwd("C:/DATA/Nigeria/EiA")

library(leaflet)
library(geodata)
library(terra)

list.files()

#### vector data #### 
# Nigeria code NGA
adm0 <- gadm(country="NGA", level=0, path=getwd())
adm1 <- gadm(country="NGA", level=1, path=getwd())
adm2 <- gadm(country="NGA", level=2, path=getwd())
#adm3 <- gadm(country="NGA", level=3, path=getwd())

#### CHIRPS rainfall data ####
datadir <- c("C:/DATA/climate/UCSB/CHIRPS_2_0/Africa_dekads")
list.files(datadir, pattern="*.tif$")


i <- 1
for (y in (2001:2020)) {
  print(paste("Year", y))
  mylist <- c("chirps-v2.0.XXXX.04.1.tif", "chirps-v2.0.XXXX.04.2.tif", "chirps-v2.0.XXXX.04.3.tif",
              "chirps-v2.0.XXXX.05.1.tif", "chirps-v2.0.XXXX.05.2.tif", "chirps-v2.0.XXXX.05.3.tif",
              "chirps-v2.0.XXXX.06.1.tif", "chirps-v2.0.XXXX.06.2.tif", "chirps-v2.0.XXXX.06.3.tif",
              "chirps-v2.0.XXXX.07.1.tif", "chirps-v2.0.XXXX.07.2.tif", "chirps-v2.0.XXXX.07.3.tif",
              "chirps-v2.0.XXXX.08.1.tif", "chirps-v2.0.XXXX.08.2.tif", "chirps-v2.0.XXXX.08.3.tif",
              "chirps-v2.0.XXXX.09.1.tif", "chirps-v2.0.XXXX.09.2.tif", "chirps-v2.0.XXXX.09.3.tif")
  mylist <- gsub("XXXX", y, mylist)
  
  tmp.rain.ts <- mask(crop(rast(paste(datadir, mylist, sep="/")), adm0), adm0)
  tmp.rain.ts <- classify(tmp.rain.ts, cbind(-9999,NA))
  summary(tmp.rain.ts[[1]])
  
  # plot(tmp.rain.ts[[1]])
  # plot(adm1, add=TRUE)
  
  tmp.rain.sum <- sum(tmp.rain.ts)
  tmp.rain.avg <- mean(tmp.rain.ts)
  tmp.rain.std <- stdev(tmp.rain.ts)
  tmp.rain.cv  <- (tmp.rain.std/tmp.rain.avg)

  names(tmp.rain.sum)[1] <- paste("rain.sum", y, sep=".")
  names(tmp.rain.avg)[1] <- paste("rain.avg", y, sep=".")
  names(tmp.rain.std)[1] <- paste("rain.std", y, sep=".")
  names(tmp.rain.cv )[1] <- paste("rain.cv" , y, sep=".")
  
  # add to stack of yearly summaries
  if (i==1) {
    rain.sum <- tmp.rain.sum
    rain.avg <- tmp.rain.avg
    rain.std <- tmp.rain.std
    rain.cv  <- tmp.rain.cv 
  } else {
    add(rain.sum) <- tmp.rain.sum
    add(rain.avg) <- tmp.rain.avg
    add(rain.std) <- tmp.rain.std
    add(rain.cv ) <- tmp.rain.cv 
  }
  
i <- i+1
}

rm(tmp.rain.ts)
rm(tmp.rain.sum)
rm(tmp.rain.avg)
rm(tmp.rain.std)
rm(tmp.rain.cv)

# names(rain.sum) <- paste("rain.sum" , names(rain.sum), sep=".")
# names(rain.avg) <- paste("rain.avg" , names(rain.avg), sep=".")
# names(rain.std) <- paste("rain.std" , names(rain.std), sep=".")
# names(rain.cv)  <- paste("rain.cv"  , names(rain.cv), sep=".")

summary(rain.sum)
summary(rain.avg)
summary(rain.std)
summary(rain.cv)

rain <- c(rain.sum, rain.avg, rain.std, rain.cv)
names(rain)

# save to disk
writeRaster(rain, paste(getwd(),"rain","Nigeria_rain_summaries.tif", sep="/"))



#### soils data ####

soil.al <- soil_af_isda("Al", path=getwd())
soil.tc <- soil_af_isda("C.tot", path=getwd())
soil.n  <- soil_af_isda("N.tot", path=getwd())
soil.oc <- soil_af_isda("OC", path=getwd())
soil.p  <- soil_af_isda("P", path=getwd())
soil.k  <- soil_af_isda("K", path=getwd())
soil.ph <- soil_af_isda("ph.h2o", path=getwd())
soil.sa <- soil_af_isda("sand", path=getwd())
soil.si <- soil_af_isda("silt", path=getwd())
soil.cl <- soil_af_isda("clay", path=getwd())

# altsoildir <- c("C:/DATA/Africa/SOILS/ISRIC_250m")
# soil.ea <- rast(paste(altsoildir,"af_EACKCL_T__M_sd1_250m.tif", sep="/"))
# soil.ea <- project(soil.ea, crs(adm0))
# soil.ea <- crop(soil.ea, adm0)
# soil.ea <- mask(crop(soil.ea, adm0), adm0)

soil.30as <- rast(list(soil.al, soil.tc, soil.n, soil.oc, soil.p, soil.k, soil.ph, soil.sa, soil.si, soil.cl))
soil.30as <- mask(crop(soil, adm0), adm0) 
names(soil.30as) <-  c("Al", 
                  "C.tot",
                  "N.tot",
                  "OC", 
                  "P", 
                  "K", 
                  "pH",
                  "sand", 
                  "silt", 
                  "clay")
plot(soil.30as)
# write copy of stack to disk
writeRaster(soil.30as, paste(getwd(), "soil_af_isda", "Nigeria_soil_layers_30as.tif", sep="/"), overwrite=TRUE)

#### resample to make compatible ####
# resample to 30 arc seconds for merging with higher resolution soils data
rain.30as <- resample(rain, soil.30as)
writeRaster(rain.30as, paste(getwd(),"rain","Nigeria_rain_summaries_30as.tif", sep="/"), overwrite=TRUE)

# resample soils to coarser res (0.05 DD), compatible with CHIRPS data 
soil <- resample(soil.30as, rain)
writeRaster(soil, paste(getwd(), "soil_af_isda", "Nigeria_soil_layers.tif", sep="/"), overwrite=TRUE)


#### merge all raster layers ####
stack <- c(soil, rain.30as)
names(stack)

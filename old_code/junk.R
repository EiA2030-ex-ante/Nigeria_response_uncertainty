
mylist <- c("chirps-v2.0.2017.04.1.tif", "chirps-v2.0.2017.04.2.tif", "chirps-v2.0.2017.04.3.tif",
            "chirps-v2.0.2017.05.1.tif", "chirps-v2.0.2017.05.2.tif", "chirps-v2.0.2017.05.3.tif",
            "chirps-v2.0.2017.06.1.tif", "chirps-v2.0.2017.06.2.tif", "chirps-v2.0.2017.06.3.tif",
            "chirps-v2.0.2017.07.1.tif", "chirps-v2.0.2017.07.2.tif", "chirps-v2.0.2017.07.3.tif",
            "chirps-v2.0.2017.08.1.tif", "chirps-v2.0.2017.08.2.tif", "chirps-v2.0.2017.08.3.tif",
            "chirps-v2.0.2017.09.1.tif", "chirps-v2.0.2017.09.2.tif", "chirps-v2.0.2017.09.3.tif")

tmp.rain.ts <- mask(crop(rast(paste(datadir, mylist, sep="/")), adm0), adm0)
tmp.rain.ts <- classify(tmp.rain.ts, cbind(-9999,NA))
summary(tmp.rain.ts[[1]])

plot(tmp.rain.ts[[1]])
plot(adm1, add=TRUE)

tmp.rain.sum <- sum(tmp.rain.ts)
tmp.rain.avg <- mean(tmp.rain.ts)
tmp.rain.std <- stdev(tmp.rain.ts)
tmp.rain.cv <- (tmp.rain.std/tmp.rain.avg)

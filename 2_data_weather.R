
this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	wd <- "G:/.shortcut-targets-by-id/1mfeEftF_LgRcxOT98CBIaBbYN4ZHkBr_/share/maize_variability"
} else if (this == "DESKTOP-JORDAN") {
  wd <- "C:/DATA/Nigeria/EiA"
} else {
  wd <- "."
}

setwd(wd)

rpath <- "data/raw/chirps/"
ipath <- "data/intermediate/chirps/"
dir.create(rpath, FALSE, FALSE)
dir.create(ipath, FALSE, FALSE)
setwd(rpath)


url <- "https://data.chc.ucsb.edu/products/CHIRPS-2.0/africa_dekad/tifs/"
p <- readLines(url)
p <- grep("a href=", p, value=TRUE)
p <- grep("chirps-v2.0", p, value=TRUE)
k <- sapply(strsplit(p, "a href=\""), \(i)i[2]) 
k <- sapply(strsplit(k, "\""), \(i)i[1]) 
urls <- paste0(url, k)

for (u in rev(urls)) {
	fn <- basename(u)
	if (!file.exists(fn)) {
		try(download.file(u, fn, mode="wb"))
	}
}


for (u in rev(urls)) {
	fn <- basename(u)
	ftif <- gsub(".gz$", "", fn)
	if (!file.exists(ftif)) {
		R.utils::gunzip(fn, remove=FALSE)
	}
}


ff <- list.files(pattern="tif$")
for (y in 1981:2023) {
	outf <- paste0("../../intermediate/chirps/chirps_dekades_africa_", y, ".tif") 
	if (!file.exists(outf)) {
		fs <- grep(y, ff, value=TRUE)
		x <- terra::rast(fs)
		terra::NAflag(x) <- -9999
		print(outf); flush.console()
		terra::writeRaster(x, outf) 
	}
}


ff <- list.files("../../intermediate/chirps", pattern="^chirps_dekades_africa_....\\.tif$", full=TRUE)

outf <- gsub("dekades", "dekades_sum", basename(ff))
outsum <- paste0("../../intermediate/chirps/stats/", outf)
outcv <- gsub("_sum", "_cv", outsum)

dir.create(dirname(outsum[1]), FALSE, FALSE)

for (i in 1:length(ff)) {
	if (!file.exists(outcv[i])) {
		print(basename(ff[i])); flush.console()
		r <- terra::rast(ff[i])
		year <- gsub("chirps_dekades_africa_|.tif", "", basename(ff[i]))
		rsum <- round(sum(r))
		terra::writeRaster(rsum, filename=outsum[i], overwrite=TRUE, wopt=list(names=paste0("rain_", year)))
		terra::writeRaster(terra::stdev(r) / terra::mean(r), filename=outcv[i], names=paste0("raincv_", year), overwrite=TRUE)
	}
}




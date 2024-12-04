
this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	wd <- "G:/.shortcut-targets-by-id/1mfeEftF_LgRcxOT98CBIaBbYN4ZHkBr_/share/maize_variability"
} else if (this == "DESKTOP-JORDAN") {
  wd <- "C:/DATA/Nigeria/EiA"
} else {
  wd <- "."
}

setwd(wd)

### admin
adm0 <- geodata::gadm(country="NGA", level=0, path="data/raw")
adm1 <- geodata::gadm(country="NGA", level=1, path="data/raw")
adm2 <- geodata::gadm(country="NGA", level=2, path="data/raw")


### soil
vars <- c("Al", "C.tot", "N.tot", "OC", "P", "K", "ph.h2o", "sand", "silt", "clay")
soil.30s <- geodata::soil_af_isda(vars, path="data/raw")

# aggregate soils to make compatible with CHIRPS data (3 minutes)
fsoil <- file.path("data/intermediate/soil", "soil_af_isda_3m.tif")
if (!file.exists(fsoil)) {
	template <- terra::rast(ext=c(-20, 55, -40, 40), res=0.05) 
	names(soil.30s) <- gsub("ph.h2o", "pH", names(soil.30s))
	names(soil.30s) <- gsub(".0-20cm", "", names(soil.30s))
	names(soil.30s) <- gsub(".tot....", "", names(soil.30s))
	dir.create("data/intermediate/soil")
	rsoil <- terra::resample(soil.30s, template, filename=fsoil)
}

### carob
ds <- paste0("data/raw/", c("carob_survey.csv", "carob_agronomy.csv"))
dir.create(dirname(ds[1]), FALSE, TRUE)
for (d in ds) {
	if (!file.exists(d)) {
		url <- paste0("https://geodata.ucdavis.edu/carob/", gsub(".csv", ".zip", basename(d)))
		fzip <- file.path(wd, "data", "raw", basename(url))
		download.file(url, fzip, mode="wb")
		unzip(fzip, exdir=dirname(fzip))
	}
}

## Population
pop <- geodata::population(year=2020,res=0.5, path="data/raw")
pop <- terra::crop(pop, adm0, snap="near", mask=TRUE, filename="data/intermediate/population_2020_Nigeria.tif")

## Access (travel time to cities)
tt_cities_u3 <- geodata::travel_time(to="city", size=3,up = TRUE, path="data/raw")
tt_cities_u3 <- terra::crop(tt_cities_u3, adm0, mask=TRUE, filename="data/intermediate/travel_time_cities_3_Nigeria.tif")

## Access (travel time to ports)
tt_ports_u2 <- geodata::travel_time(to="port", size=2,up = TRUE, path="data/raw")
tt_ports_u2 <- terra::crop(tt_ports_u2, adm0, mask=TRUE, filename="data/intermediate/travel_time_ports_2_Nigeria.tif")


#terra::plot(tt_cities_u3)
#terra::plot(tt_ports_u2, col = wesanderson::wes_palette("Darjeeling1"))
#stack <- c(pop, tt_cities_u3, tt_ports_u2)
#terra::plot(stack)

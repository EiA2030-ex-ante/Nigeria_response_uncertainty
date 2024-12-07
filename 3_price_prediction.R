
this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	wd <- "G:/.shortcut-targets-by-id/1mfeEftF_LgRcxOT98CBIaBbYN4ZHkBr_/share/maize_variability"
} else if (this == "DESKTOP-JORDAN") {
  wd <- "C:/DATA/Nigeria/EiA"
} else {
  wd <- "."
}

setwd(wd)
dir.create("plots", FALSE, FALSE)

# colors
pal <- colorRampPalette(c(wesanderson::wes_palettes$Zissou1))(30)

# Load the data
adm1 <- geodata::gadm(country="NGA", level=1, path="data/raw")
## Population
pop <- terra::rast("data/intermediate/population_2020_Nigeria.tif")
## accessibility (travel time to city)
acc <- terra::rast("data/intermediate/travel_time_cities_3_Nigeria.tif")
## travel time to port
port <- terra::rast("data/intermediate/travel_time_ports_2_Nigeria.tif")
## stack the rasters
geo_stack <- c(pop, acc, port)

# Generate Latitude and Longitude grid 
# these are not used (and should not be needed; see terra::interpolate) 
latgrd <- terra::init(pop, "y", names="longitude")
longrd <- terra::init(pop, "x", names="latitude")
#geo_stack <- c(geo_stack, latgrd, longrd)
#plot(geo_stack)

## Nitrogen prices
nprice <- read.csv("data/raw/prices/Nigeria_urea_pkg_2024.csv") 
nprice <- terra::vect(nprice, geom=c("Longitude", "Latitude"), crs="+proj=longlat", keepgeom=TRUE)

## Maize prices
mprice <- read.csv("data/raw/prices/wft_maize_wholesale_2023_nga.csv")
mprice <- terra::vect(mprice, geom=c("longitude", "latitude"), crs="+proj=longlat", keepgeom=TRUE)

# extract the values of the raster stack at the location of the  nprices
nprices <- terra::extract(geo_stack, nprice, method="bilinear", bind=TRUE)
# extract the values of the raster stack at the location of the  mprices
mprices <- terra::extract(geo_stack, mprice, method="bilinear", bind=TRUE)


# predict the nitrogen prices
fN <- "data/intermediate/predicted_nitrogen_price_Nigeria.tif"
if (file.exists(fN)) {
	npkg_pr1 <- rast(fN)
} else {
	rf1 <- randomForest::randomForest(N_pkg_USD ~ population_density + travel_time_to_cities_1 + travel_time_to_ports_1 , data=nprices, proximity=TRUE, importance=TRUE, ntree=10, mtry=2, nodesize=5)
	npkg_pr1 <- terra::predict(geo_stack, rf1, filename=fN, overwrite=TRUE, wopt=list(names= "npkg"))
	rf1
	#randomForest::varImpPlot(rf1)
	#round(randomForest::importance(rf1), 4)
	#str(rf1$proximity)
	png("plots/predicted_N-price.png", width=800, height=800, pointsize=24)
	terra::plot(npkg_pr1, col=pal, alpha=.8, main="Predicted N price (USD/kg)", fun=terra::lines(adm1, col="#000643"), cex=1.5, las=1, axes=FALSE)
	dev.off()
}


fM <- "data/intermediate/predicted_maize_price_Nigeria.tif"
if (file.exists(fM)) {
	mai_pkg_pr2	<- rast(fM)
} else {
	# predict the maize prices
	rf2 <- randomForest::randomForest(mai_pkg_usd ~ population_density + travel_time_to_cities_1 + travel_time_to_ports_1, data=mprices, proximity=TRUE, importance=TRUE, ntree=10, mtry=2, nodesize=5)
	mai_pkg_pr2 <- terra::predict(geo_stack, rf2, filename=fM, overwrite=TRUE, wopt=list(names= "mpkg"))
	rf2
	#randomForest::varImpPlot(rf2)
	#round(randomForest::importance(rf2), 4)
	#str(rf2$proximity)
	png("plots/predicted_maize-price.png", width=800, height=800, pointsize=24)
	terra::plot(mai_pkg_pr2, col=pal, alpha=.8, main="Predicted maize price (USD/kg)", fun=terra::lines(adm1, col="#000643"), cex=1.5, las=1, axes=FALSE)
	dev.off()
}



### other plots

#terra::plot(geo_stack$travel_time_to_cities_1, col=pal)
#terra::lines(adm1, col="#000643")

#terra::plot(nprice, add=TRUE, col="blue", pch=8, cex=1.5)
#color_palette <- colorRampPalette(c(wesanderson::wes_palettes$Zissou1))(length(unique(nprice$N_pkg_USD)))
#point_colors <- color_palette[as.numeric(factor(nprice$N_pkg_USD))]

#terra::plet(geo_stack$travel_time_to_cities_1, tiles="CartoDB.Positron", col=pal, alpha=0.7) |> 
#	terra::lines(adm1, lwd=1, col="#000643") |> 
#	terra::points(nprice, cex=5, col="red", popup=TRUE)
	## label requires dev version of terra 
	## terra::points(nprice, cex=5, col = "red", popup=TRUE, label = round(nprice$N_pkg_USD, 3))
	
#leaflet::leaflet() |>
#  leaflet::addProviderTiles("CartoDB.Positron") |>
#  leaflet::addRasterImage(geo_stack$travel_time_to_cities_1, colors = pal, opacity = 0.7) |>
#  leaflet::addPolygons(data = adm1, weight = 1, color = "#000643", fill = FALSE) |>
#  leaflet::addCircleMarkers(data = nprice, lng = ~Longitude, lat = ~Latitude, radius = 5, color = 'red', fillOpacity = 0.8, label = ~round(N_pkg_USD, 3))

#terra::plot(nprice, pch = 19, col = "gold", cex = 1.5, add=TRUE)
#terra::lines(adm1, col="#000643")
#terra::points(mprice, col="white", pch=21, cex=1.5)

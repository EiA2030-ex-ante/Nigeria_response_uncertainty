library(leaflet)
library(geodata)
library(terra)

pal <- colorRampPalette(c(wesanderson::wes_palettes$Zissou1))(10)
# Load the data
## admin
adm0 <- geodata::gadm(country="NGA", level=0, path="data/raw")
adm1 <- geodata::gadm(country="NGA", level=1, path="data/raw")
adm2 <- geodata::gadm(country="NGA", level=2, path="data/raw")

## Population
pop <- rast("data/intermediate/population_2020_Nigeria.tif")

## accessibility (travel time to city)
acc <- rast("data/intermediate/travel_time_cities_3_Nigeria.tif")

## travel time to port
port <- rast("data/intermediate/travel_time_ports_2_Nigeria.tif")

## stack the rasters

geo_stack <- c(pop, acc, port)





## Nitrogen prices

nprices <- read.csv("data/raw/prices/Nigeria_urea_pkg_2024.csv") 
nprices <- vect(nprices, geom=c("Longitude", "Latitude"), crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", keepgeom=TRUE)

terra::plot(geo_stack$travel_time_to_cities_1, col=pal)
terra::plot(adm1, add=TRUE, border="#000643")
terra::plot(nprices, add=TRUE, col="blue", pch=8, cex=1.5)

color_palette <- colorRampPalette(c(wesanderson::wes_palettes$Zissou1))(length(unique(nprices$N_pkg_USD)))
point_colors <- color_palette[as.numeric(factor(nprices$N_pkg_USD))]

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addRasterImage(geo_stack$travel_time_to_cities_1, colors = pal, opacity = 0.7) %>%
  addPolygons(data = adm1, weight = 1, color = "#000643", fill = FALSE) %>%
  addCircleMarkers(data = nprices, lng = ~Longitude, lat = ~Latitude, radius = 5, color = 'red', fillOpacity = 0.8, label = ~round(N_pkg_USD, 3))

terra::plot(adm1, main = "Nitrogen prices (from urea)", border="#000643")
terra::plot(geo_stack$travel_time_to_cities_1, add=TRUE)
terra::plot(nprices, pch = 19, col = "gold", cex = 1.5, add=TRUE)
terra::plot(adm1,  border="#000643", add=TRUE)

## Maize prices

mprices <- read.csv("data/raw/prices/wft_maize_wholesale_2023_nga.csv")
mprices <- vect(mprices, geom=c("longitude", "latitude"), crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", keepgeom=TRUE)

terra::plot(mprices, add=TRUE, col="white", pch=21, cex=1.5)

# Generate Latitude and Longitude grid
latgrd <- longrd <- geo_stack[[1]]
latgrd[] <- yFromCell(latgrd, 1:ncell(latgrd))
longrd[] <- xFromCell(longrd, 1:ncell(longrd))
latgrd <- mask(latgrd,geo_stack[[1]])
longrd <- mask(longrd,geo_stack[[1]])
names(latgrd) <- c("latitude")
names(longrd) <- c("longitude")

plot(c(geo_stack, latgrd, longrd), nc=2)
geo_stack <- c(geo_stack, latgrd, longrd)
plot(geo_stack)

# extract the values of the raster stack at the location of the  nprices
extr_np <- extract(geo_stack, nprices, method="bilinear")
nprices <- cbind(nprices, extr_np)

# extract the values of the raster stack at the location of the  mprices
extr_mp <- extract(geo_stack, mprices, method="bilinear")
mprices <- cbind(mprices, extr_mp)


# predict the nitrogen prices
rf1 <- randomForest::randomForest(N_pkg_USD ~ population_density + travel_time_to_cities_1 + travel_time_to_ports_1 , data=nprices, proximity=TRUE, importance=TRUE, ntree=10, mtry=2, nodesize=5)
rf1
randomForest::varImpPlot(rf1)
round(randomForest::importance(rf1), 4)

str(rf1$proximity)

npkg_pr1 <- predict(geo_stack, rf1)
plot(npkg_pr1, col=scales::alpha(pal,0.8),  main="Predicted N price (USD/kg)")
plot(adm1, add=TRUE, border="#000643")

terra::writeRaster(npkg_pr1, "data/intermediate/predicted_nitrogen_price_Nigeria.tif", overwrite=TRUE)

# predict the maize prices
rf2 <- randomForest::randomForest(mai_pkg_usd ~ population_density + travel_time_to_cities_1 + travel_time_to_ports_1, data=mprices, proximity=TRUE, importance=TRUE, ntree=10, mtry=2, nodesize=5)
rf2
randomForest::varImpPlot(rf2)
round(randomForest::importance(rf2), 4)

str(rf2$proximity)

mai_pkg_pr2 <- predict(geo_stack, rf2)
plot(mai_pkg_pr2, col=scales::alpha(pal,0.8),  main="Predicted maize price (USD/kg)")
plot(adm1, add=TRUE, border="#000643")

terra::writeRaster(mai_pkg_pr2, "data/intermediate/predicted_maize_price_Nigeria.tif", overwrite=TRUE)

library(terra)
library(randomForest)

dir.create("data/results/model/", showWarnings = FALSE, recursive = TRUE)

extrafont::loadfonts()
my_font <- "Frutiger"

# Set up color palette
pal_1 <- colorRampPalette(c(wesanderson::wes_palettes$Zissou1))
pal_2 <- colorRampPalette(c(wesanderson::wes_palettes$AsteroidCity2))
# Read administrative boundaries
adm1 <- geodata::gadm(country = "NGA",
                      level = 1,
                      path = "data/raw")
adm0 <- geodata::gadm(country = "NGA",
                      level = 0,
                      path = "data/raw")

# Define extent for Nigeria
nga_ext <- c(2, 15, 4, 14)

# Load price rasters
npkg <- terra::rast("data/intermediate/predicted_nitrogen_price_Nigeria.tif")
mpkg <- terra::rast("data/intermediate/predicted_maize_price_Nigeria.tif")

# Plot nitrogen price raster
terra::plot(npkg, col = pal_1(7), main = "Predicted Nitrogen price (USD/kg)")
terra::plot(mpkg, col = pal_2(7), main = "Predicted Maize price (USD/kg)")

# Bring in raster layers for prediction
rain_sum <- list.files("data/intermediate/chirps/stats",
                       pattern = "_sum",
                       full.names = TRUE) |> terra::rast()
rain_cv <- list.files("data/intermediate/chirps/stats",
                      pattern = "_cv",
                      full.names = TRUE) |> terra::rast()

soil <- terra::rast("data/intermediate/soil/soil_af_isda_3m.tif")
# Optimal NPK application rates
optimal_NPK_raster <- rast("data/results/model/optNPK.tif")

png("data/results/figures/Optimal_NPK_rasters.png", units = "in", width = 6, height = 5, res = 600)
par(mar = c(0, 0, 0, 0), family = my_font, cex = 0.8, oma = c(0, 0, 0, 0))
terra::plot(optimal_NPK_raster, col = pal_1(3), main= c("Optimal N (kg/ha)","Optimal P (kg/ha)","Optimal K (kg/ha)"))
terra::plot(adm0, border = "#000643", add = TRUE, lwd = 1)
dev.off()

# Define predictor variables
predictors <- c(
  "N_fertilizer",
  "P_fertilizer",
  "K_fertilizer",
  "oc",
  "pH",
  "sand",
  "clay",
  "rain",
  "raincv"
)





#### Predict yields ####

soil <- crop(soil, adm0, mask = TRUE)

# Resample price rasters to match soil raster
npkg <- resample(npkg, soil)
mpkg <- resample(mpkg, soil)
optimal_NPK <- resample(optimal_NPK_raster, soil, method = "bilinear")

# Create predictor raster stack
preds_1 <- c(soil[[c("oc", "pH", "sand", "clay")]], optimal_NPK, npkg, mpkg)
preds_2 <- c(soil[[c("oc", "pH", "sand", "clay")]], npkg, mpkg)
names(preds_1) <- c(
  "oc",
  "pH",
  "sand",
  "clay",
  "N_fertilizer",
  "P_fertilizer",
  "K_fertilizer",
  "npkg",
  "mpkg"
)

names(preds_2) <- c(
  "oc",
  "pH",
  "sand",
  "clay",
  "npkg",
  "mpkg"
)

eld_simpng("data/results/figures/yield_predictors_rasters.png", units = "in", width = 6, height = 5, res = 600)
par(mar = c(0, 0, 0, 0), family = my_font, cex = 0.8, oma = c(0, 0, 0, 5))
terra::plot(preds_1, col = pal_2(10), main= c("Organic Carbon","pH","Sand","Clay","N (kg/ha)","P (kg/ha)","K (kg/ha)","Nitrogen price (USD/kg)","Maize price (USD/kg)"))
terra::plot(adm0, border = "#000643", add = TRUE, lwd = 1)
dev.off()

set.seed(20241113)

# ----------------------------------------------
# Step 1: Define Rainfall Variables and Probabilities
# ----------------------------------------------

# Define the range of years
years <- 2001:2020

# Create lists of rainfall variable names
tlist <- paste0("rain_", years)
clist <- paste0("raincv_", years)

# Define weights: Oldest year has weight 1, most recent year has weight 2
weights <- seq(1, 2, length.out = length(years))

# Normalize weights to obtain probabilities
plist <- weights / sum(weights)


# ----------------------------------------------
# Step 2: Load Base Stack, Optimal NPK, and Model
# ----------------------------------------------
crf <- readRDS("data/results/model/crf.rds")
stack_1 <- preds_1[[c("oc",
                  "pH",
                  "sand",
                  "clay",
                  "N_fertilizer",
                  "P_fertilizer",
                  "K_fertilizer")]]

# ----------------------------------------------
# Step 3: Initialize Prediction Rasters
# ----------------------------------------------

# Initialize empty raster stacks for predictions
pr2_optimal <- rast()
pr3_optimal <- rast()

pr2_constant <- rast()
pr3_constant <- rast()

# ----------------------------------------------
# Step 4: Simple Stochastic Simulation Using `plist`
# ----------------------------------------------
run_simulation <- function(simulation_type, num_simulations, save_path) {
  
  cat("Running Simulation:", simulation_type, "\n")
  
  # Initialize an empty list for storing prediction rasters
  prediction_stack <- rast()
  
  # Select the appropriate stack based on simulation type
  if (simulation_type == "optimal") {
    input_stack <- stack_1
  } else if (simulation_type == "NPK") {
    input_stack <- preds_1[[c("oc", "pH", "sand", "clay")]]
  } else {
    stop("Invalid simulation type. Use 'optimal' or 'NPK'.")
  }
  
  # Loop through the specified number of simulations
  for (i in 1:num_simulations) {
    cat("Simulation Iteration:", i%%100, "\n")
    
    # Randomly select a year's rainfall data
    t_idx <- sample(seq_along(tlist), 1, prob = plist)
    tchoice <- tlist[t_idx]
    cchoice <- clist[t_idx]
    
    # Extract and process rainfall rasters
    rain.sum <- rain_sum[[tchoice]]
    rain.sum <- resample(rain.sum, input_stack)
    rain.sum <- crop(rain.sum, adm0, mask = TRUE)
    names(rain.sum) <- "rain"
    
    rain.cv <- rain_cv[[cchoice]]
    rain.cv <- resample(rain.cv, input_stack)
    rain.cv <- crop(rain.cv, adm0, mask = TRUE)
    names(rain.cv) <- "raincv"
    
    # Combine stack with rainfall data
    newstack <- c(input_stack, rain.sum, rain.cv)
    
    # Replace NA values with -99999
    newstack_filled <- ifel(is.na(newstack), -99999, newstack)
    predictor_df <- as.data.frame(newstack_filled, xy = TRUE, na.rm = FALSE)
    
    # Add fertilizer values for NPK simulation
    if (simulation_type == "NPK") {
      predictor_df$N_fertilizer <- 100
      predictor_df$P_fertilizer <- 50
      predictor_df$K_fertilizer <- 15
    }
    
    # Perform prediction
    predictions <- predict(crf, newdata = predictor_df)
    
    # Replace -99999 back to NA in predictions
    na_cells <- apply(predictor_df == -99999, 1, any)
    predictions[na_cells] <- NA
    
    # Create a raster for predictions
    prediction_raster <- rast(
      nrows = nrow(input_stack),
      ncols = ncol(input_stack),
      extent = ext(input_stack),
      crs = crs(input_stack)
    )
    values(prediction_raster) <- predictions
    
    # Add prediction raster to the stack
    prediction_stack <- c(prediction_stack, prediction_raster)
  }
  
  # Calculate statistics for yield predictions
  avg_raster <- app(prediction_stack, fun = mean, na.rm = TRUE)
  var_raster <- app(prediction_stack, fun = var, na.rm = TRUE)
  sd_raster <- app(prediction_stack, fun = sd, na.rm = TRUE)
  cv_raster <- sd_raster / avg_raster
  names(var_raster) <- "var"
  names(cv_raster) <- "CV"
  
  # Create summary stack
  summary_stack <- c(avg_raster, var_raster, sd_raster, cv_raster)
  
  # Save the results
  writeRaster(prediction_stack, 
              file.path(save_path, paste0("yield_simple_", simulation_type, "_", num_simulations, ".tif")), 
              overwrite = TRUE)
  writeRaster(summary_stack, 
              file.path(save_path, paste0("yield_simple_", simulation_type, "_", num_simulations, "_summary.tif")), 
              overwrite = TRUE)
  
  # Return the summary stack
  return(prediction_stack)
}

# ----------------------------------------------

# Run the simulation for the optimal NPK application rates
pr2_optimal <- run_simulation("optimal", 1000, "data/results/model/")
pr2_constant <- run_simulation("NPK", 1000, "data/results/model/")

# ----------------------------------------------
# Calculate Net returns
# ----------------------------------------------

fertilizer_cost_optimal <- c(optimal_NPK$N_fertilizer * npkg + optimal_NPK$P_fertilizer * npkg + optimal_NPK$K_fertilizer * npkg)
fertilizer_cost_optimal <- resample(fertilizer_cost_optimal, pr2_optimal[[1]], method = "bilinear")
fertilizer_cost_constant <- c(100 * npkg + 50 * npkg + 15 * npkg)
fertilizer_cost_constant <- resample(fertilizer_cost_constant, pr2_optimal[[1]], method = "bilinear")

net_returns_optimal <- rast()
for (i in 1:nlyr(pr2_optimal)) {
  
  yield_raster <- pr2_optimal[[i]]
  netreturns <- yield_raster * mpkg - fertilizer_cost_optimal
  names(netreturns) <- paste0("Net_Returns_", i)
  net_returns_optimal <- c(net_returns_optimal, netreturns)
  
}

writeRaster(net_returns_optimal, "data/results/model/net_returns_optimal_1000.tif", overwrite = TRUE)

net_returns_constant <- rast()
for (i in 1:nlyr(pr2_constant)) {
  
  yield_raster <- pr2_constant[[i]]
  netreturns <- yield_raster * mpkg - fertilizer_cost_constant
  names(netreturns) <- paste0("Net_Returns_", i)
  net_returns_constant <- c(net_returns_constant, netreturns)
  
}

writeRaster(net_returns_constant, "data/results/model/net_returns_constant_1000.tif", overwrite = TRUE)

# ----------------------------------------------
# Net returns
# ----------------------------------------------
# Predict yields with optimal NPK rates


predicted_yield_0N0P0K <- predict(preds_constant, crf, const=data.frame(N_fertilizer=0, P_fertilizer=0, K_fertilizer=0), na.rm = TRUE)

writeRaster(predicted_yield_0N0P0K, "data/results/model/yield_simple_0N0P0K.tif", overwrite = TRUE)

plot(predicted_yield_0N0P0K, main = "Predicted yield (kg/ha) with no NPK", col = pal_2(8))


profit_optimal <- rast()
profit_constant <- rast()
fertilizer_cost_optimal <- c(optimal_NPK$N_fertilizer * npkg + optimal_NPK$P_fertilizer * npkg + optimal_NPK$K_fertilizer * npkg)
fertilizer_cost_optimal <- resample(fertilizer_cost_optimal, pr2_optimal[[1]], method = "bilinear")
fertilizer_cost_constant <- c(100 * npkg + 50 * npkg + 15 * npkg)
fertilizer_cost_constant <- resample(fertilizer_cost_constant, pr2_optimal[[1]], method = "bilinear")

pr2_optimal <- rast("data/results/model/yield_simple_optimal_1000.tif")
pr2_constant <- rast("data/results/model/yield_simple_NPK_1000.tif")
for (i in 1:nlyr(pr2_optimal)) {
  yield_raster <- pr2_optimal[[i]]
  netreturns <- (yield_raster - predicted_yield_0N0P0K) * mpkg - fertilizer_cost_optimal
  names(netreturns) <- paste0("profit_", i)
  profit_optimal <- c(profit_optimal, netreturns)
  
}

for (i in 1:nlyr(pr2_constant)) {
  yield_raster <- pr2_constant[[i]]
  netreturns <- (yield_raster - predicted_yield_0N0P0K) * mpkg - fertilizer_cost_constant
  names(netreturns) <- paste0("profit_", i)
  profit_constant <- c(profit_constant, netreturns)
  
}

profit_optimal[profit_optimal < 0] <- 0
profit_constant[profit_constant < 0] <- 0
plot(profit_optimal[[1]], main = "Profit uncertainty (optimal NPK)", col = pal_1(10))

writeRaster(profit_optimal, "data/results/model/profit_optimal.tif", overwrite = TRUE)
writeRaster(profit_constant, "data/results/model/profit_constant.tif", overwrite = TRUE)

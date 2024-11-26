# Load necessary libraries
library(terra)         # For raster operations
library(GA)            # For genetic algorithm optimization
library(randomForest)  # For the random forest model
library(parallel)      # For parallel processing

# ----------------------------------------------
# Step 1: Load and Prepare Rasters
# ----------------------------------------------

# Create directory for results
dir.create("data/results/model/", FALSE, TRUE)

# Set up color palette
pal <- colorRampPalette(c(wesanderson::wes_palettes$Zissou1))
pal2 <- colorRampPalette(c(wesanderson::wes_palettes$AsteroidCity2))
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
terra::plot(npkg, col = pal, main = "Predicted Nitrogen price (USD/kg)")
terra::plot(adm0,
            border = "white",
            add = TRUE,
            lwd = 1)

# Read maize observations
d <- read.csv("data/intermediate/observations/NGA_Jordan.csv")
s <- vect(d, geom = c("longitude", "latitude"))

# Plot observations
terra::plot(
  s,
  border = "white",
  add = TRUE,
  pch = 15,
  col = "white"
)

# Bring in raster layers for prediction
rain_sum <- list.files("data/intermediate/chirps/stats",
                       pattern = "_sum",
                       full.names = TRUE) |> terra::rast()
rain_cv <- list.files("data/intermediate/chirps/stats",
                      pattern = "_cv",
                      full.names = TRUE) |> terra::rast()
soil <- terra::rast("data/intermediate/soil/soil_af_isda_3m.tif")

# Extract data for observations
s <- s[!is.na(s$pyear), ]
esum <- extract(rain_sum, s, layer = match(s$pyear, gsub("rain_", "", names(rain_sum))))
ecv <- extract(rain_cv, s, layer = match(s$pyear, gsub("raincv_", "", names(rain_cv))))
esoil <- extract(soil, s, ID = FALSE)

s <- cbind(s, data.frame(rain = esum$value, raincv = ecv$value, esoil))

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

# Prepare data for model training
d <- data.frame(s)[, c("yield", predictors)] |> na.omit()
set.seed(20241113)
crf <- randomForest::randomForest(x = d[, -1], y = d[, 1], ntree = 140, importance = T)
plot(crf)

# tune and train random forest model

#crf_src <- randomForestSRC::tune(yield ~ ., data = d, doBest = TRUE)
# crf_src <- crf_src$rf

#trf <- tuneRF(x=d[, -1], y=d[,1])
#trf
#mt <- trf[which.min(trf[,2]),1]
#mt



# export crf model
saveRDS(crf, "data/results/model/crf.rds")

#### Predict yields ####
soil <- crop(soil, ext(adm0))
rain_sum <- crop(rain_sum, ext(adm0))
rain_cv <- crop(rain_cv, ext(adm0))

# Compute mean rainfall rasters
meanrainsum <- mean(rain_sum)
meanraincv <- mean(rain_cv)
names(meanrainsum) <- "rain"
names(meanraincv) <- "raincv"

# Resample price rasters to match soil raster
npkg <- resample(npkg, soil)
mpkg <- resample(mpkg, soil)





soil_2 <- soil[c("oc", "pH", "sand", "clay")]


# Create predictor raster stack
preds <- c(soil[[c("oc", "pH", "sand", "clay")]], meanrainsum, meanraincv, npkg, mpkg)
names(preds) <- c("oc", "pH", "sand", "clay", "rain", "raincv", "npkg", "mpkg")


# plot the location of the data 

png("data/results/figures/observed_yield.png", units = "in", width = 7, height = 7, res = 600)
par(mar = c(0, 15, 0, 0), family = my_font, cex = 0.8, oma = c(0, 0, 0, 0))

plot(meanrainsum, col = scales::alpha(pal(5),0.7), legend=F, breaks = c(-Inf, 500, 1000, 2000, 3000, Inf))
legend("bottom",  cex=1, box.col="white", inset=-0.02, title="Mean annual rainfall", legend=c('0 - 500', '500 - 1000', '1000 - 2000','2000-3000', '>=3000'), fill = scales::alpha(pal(5),0.7), horiz=T, xpd = T)
plot(s, border = "white", col = "#000643", pch = 15, add=T)
legend("bottomright", cex=1, box.col="white", inset=0.02, legend="Observed yield", pch=15, col="#000643", xpd = T)
plot(adm0, border = "#000643", add = TRUE, lwd = 2)

dev.off()
# ----------------------------------------------
# Step 2: Convert Raster Stack to Data Frame
# ----------------------------------------------

# Convert raster stack to data frame
preds_df <- as.data.frame(preds,
                          xy = TRUE,
                          cells = TRUE,
                          na.rm = TRUE)

# Extract cell indices and coordinates
cell_indices <- preds_df$cell
coordinates <- preds_df[, c("x", "y")]

# ----------------------------------------------
# Step 3: Define the Profitability Function
# ----------------------------------------------

profitability <- function(yield, yield0, value, cost) {
  profit <- (yield - yield0) * value - cost
  return(profit)
}

# ----------------------------------------------
# Step 4: Define the Per-Cell Optimization Function
# ----------------------------------------------

optimize_fertilizer <- function(cell_data) {
  # Extract environmental variables and prices for the cell
  
  predictors <- as.data.frame((cell_data))
  
  # Extract prices for the cell
  maize_price <- predictors$mpkg
  fertilizer_price <- predictors$npkg
  N_price <- fertilizer_price
  P_price <- fertilizer_price
  K_price <- fertilizer_price
  
  # Remove the price columns from predictors if they are not used in the model
  predictors <- predictors[, !names(predictors) %in% c("npkg", "mpkg")]
  
  # Ensure all predictor variables are numeric
  predictors[] <- lapply(predictors, function(x) as.numeric(as.character(x)))
  
  # Ensure that all variables expected by the model are present
  model_variables <- crf$xNames
  missing_vars <- setdiff(model_variables, names(predictors))
  if (length(missing_vars) > 0) {
    stop(paste("Missing predictor variables:", paste(missing_vars, collapse = ", ")))
  }
  
  # Baseline yield (yield without fertilizer)
  const_data_zero <- data.frame(
    N_fertilizer = 0,
    P_fertilizer = 0,
    K_fertilizer = 0
  )
  
  baseline_data <- cbind(predictors, const_data_zero)
  yield0 <- predict(crf, newdata = baseline_data)
  
  # Define the fitness function using the profitability function
  fitness_function <- function(NPK) {
    N <- NPK[1]
    P <- NPK[2]
    K <- NPK[3]
    
    # Create data for prediction
    const_data <- data.frame(
      N_fertilizer = N,
      P_fertilizer = P,
      K_fertilizer = K
    )
    prediction_data <- cbind(predictors, const_data)
    
    # Predict yield with fertilizer application
    yield <- predict(crf, newdata = prediction_data)
    
    # Calculate cost of fertilizers
    cost <- (N * N_price) + (P * P_price) + (K * K_price)
    
    # Calculate profit using the profitability function
    profit <- profitability(yield, yield0, maize_price, cost)
    
    # Return negative profit for minimization
    return(-profit)
  }
  
  # Run GA optimization
  ga_result <- GA::ga(
    type = "real-valued",
    fitness = fitness_function,
    lower = c(0, 0, 0),
    upper = c(200, 100, 100),
    popSize = 20,
    maxiter = 50,
    run = 10,
    optim = TRUE,
    monitor = FALSE
  )
  
  # Extract optimal NPK rates and maximum profit
  optimal_NPK <- ga_result@solution
  max_profit <- -ga_result@fitnessValue
  
  return(list(
    N_opt = optimal_NPK[1],
    P_opt = optimal_NPK[2],
    K_opt = optimal_NPK[3],
    max_profit = max_profit
  ))
}

# ----------------------------------------------
# Step 5: Perform Parallel Processing
# ----------------------------------------------

# Predictor columns (including 'npkg' and 'mpkg' for price extraction)
predictor_columns <- setdiff(names(preds_df), c("cell", "x", "y"))

# Create list of cell data
cell_data_list <- split(preds_df[, predictor_columns], seq(nrow(preds_df)))

# Number of cores (adjust as appropriate)
num_cores <- parallel::detectCores() - 1

# Create a cluster
cl <- parallel::makeCluster(num_cores)

# Export necessary variables and functions to the cluster
parallel::clusterExport(
  cl,
  varlist = c("optimize_fertilizer", "crf", "profitability"),
  envir = environment()
)

# Load required packages on cluster nodes
parallel::clusterEvalQ(cl, {
  library(GA)
  library(randomForest)
})

# Apply optimization in parallel with error handling
results_list <- parallel::parLapply(cl, cell_data_list, function(cell_data) {
  tryCatch(
    optimize_fertilizer(cell_data = cell_data),
    error = function(e) {
      # Return NA values and the error message
      list(
        N_opt = NA,
        P_opt = NA,
        K_opt = NA,
        max_profit = NA,
        error = e$message
      )
    }
  )
})

# Stop the cluster
parallel::stopCluster(cl)

# ----------------------------------------------
# Step 6: Compile and Save Results
# ----------------------------------------------

# Combine results into a data frame
results_df <- do.call(rbind, lapply(seq_along(results_list), function(i) {
  res <- results_list[[i]]
  data.frame(
    cell = cell_indices[i],
    x = coordinates$x[i],
    y = coordinates$y[i],
    N_opt = res$N_opt,
    P_opt = res$P_opt,
    K_opt = res$K_opt,
    max_profit = res$max_profit,
    error = ifelse(is.null(res$error), NA, res$error)
  )
}))

# Identify and report errors
error_rows <- results_df[!is.na(results_df$error), ]
if (nrow(error_rows) > 0) {
  cat("Errors occurred in the following cells:\n")
  print(error_rows[, c("cell", "error")])
}

# Remove error rows before creating rasters
results_df_clean <- results_df[is.na(results_df$error), ]

# Create rasters from results
optimal_N_raster <- rast(results_df_clean[, c("x", "y", "N_opt")], crs = crs(preds))
optimal_P_raster <- rast(results_df_clean[, c("x", "y", "P_opt")], crs = crs(preds))
optimal_K_raster <- rast(results_df_clean[, c("x", "y", "K_opt")], crs = crs(preds))
max_profit_raster <- rast(results_df_clean[, c("x", "y", "max_profit")], crs = crs(preds))

# ----------------------------------------------
# Step 7: Save and Visualize the Resultant Rasters
# ----------------------------------------------

# Save rasters to files
writeRaster(optimal_N_raster, "data/results/model/optimal_N.tif", overwrite = TRUE)
writeRaster(optimal_P_raster, "data/results/model/optimal_P.tif", overwrite = TRUE)
writeRaster(optimal_K_raster, "data/results/model/optimal_K.tif", overwrite = TRUE)
writeRaster(max_profit_raster, "data/results/model/max_profit.tif", overwrite = TRUE)

# Optionally, plot the rasters
plot(optimal_N_raster, main = "Optimal Nitrogen Rates")
plot(optimal_P_raster, main = "Optimal Phosphorus Rates")
plot(optimal_K_raster, main = "Optimal Potassium Rates")
plot(max_profit_raster, main = "Maximum Profit")

# Set optimal NPK rates to zero where profit < 0
optimal_N_final <- ifel(max_profit_raster >= 0, optimal_N_raster, 0)
names(optimal_N_final) <- "N_fertilizer"
optimal_P_final <- ifel(max_profit_raster >= 0, optimal_P_raster, 0)
names(optimal_P_final) <- "P_fertilizer"
optimal_K_final <- ifel(max_profit_raster >= 0, optimal_K_raster, 0)
names(optimal_K_final) <- "K_fertilizer"

# Optionally, set negative profits to zero
max_profit_final <- ifel(max_profit_raster >= 0, max_profit_raster, 0)
names(max_profit_final) <- "max_profit"

optimal_NPK_raster <- c(optimal_N_final, optimal_P_final, optimal_K_final)

plot(c(optimal_NPK_raster, max_profit_final),  col=pal)

# Save optimal NPK rates and maximum profit to disk
writeRaster(optimal_NPK_raster, "data/results/model/optNPK.tif", overwrite = TRUE)
writeRaster(max_profit_final, "data/results/model/optNPK_mprofit.tif", overwrite = TRUE)



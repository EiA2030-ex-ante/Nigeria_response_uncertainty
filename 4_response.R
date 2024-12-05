
this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	wd <- "G:/.shortcut-targets-by-id/1mfeEftF_LgRcxOT98CBIaBbYN4ZHkBr_/share/maize_variability"
} else if (this == "DESKTOP-JORDAN") {
  wd <- "C:/DATA/Nigeria/EiA"
} else {
  wd <- "."
}
setwd(wd)


# Load necessary libraries
#library(terra)         # For raster operations
#library(GA)            # For genetic algorithm optimization
#library(randomForest)  # For the random forest model
#library(parallel)      # For parallel processing


# Create directory for results
dir.create("data/results/model/", FALSE, TRUE)

# Read administrative boundaries
adm0 <- geodata::gadm(country = "NGA", level = 0, path = "data/raw")
adm1 <- geodata::gadm(country = "NGA",level = 1, path = "data/raw")

# Define extent for Nigeria
nga_ext <- c(2, 15, 4, 14)

# Load price rasters
npkg <- terra::rast("data/intermediate/predicted_nitrogen_price_Nigeria.tif")
mpkg <- terra::rast("data/intermediate/predicted_maize_price_Nigeria.tif")

# Read maize observations
d <- read.csv("data/intermediate/observations/NGA_Jordan.csv")
s <- terra::vect(d, geom = c("longitude", "latitude"), crs="+proj=longlat")

# Bring in raster layers for prediction
rain_sum <- terra::rast(list.files("data/intermediate/chirps/stats", pattern = "_sum", full.names = TRUE))
rain_cv <- terra::rast(list.files("data/intermediate/chirps/stats", pattern = "_cv", full.names = TRUE))
soil <- terra::rast("data/intermediate/soil/soil_af_isda_3m.tif")

# Extract data for observations
s <- s[!is.na(s$pyear), ]
esum <- terra::extract(rain_sum, s, layer = match(s$pyear, gsub("rain_", "", names(rain_sum))))
ecv <- terra::extract(rain_cv, s, layer = match(s$pyear, gsub("raincv_", "", names(rain_cv))))
esoil <- terra::extract(soil, s, ID = FALSE)

s <- cbind(s, data.frame(rain = esum$value, raincv = ecv$value, esoil))

# Define predictor variables
predictors <- c("N_fertilizer", "P_fertilizer", "K_fertilizer", "oc", "pH", "sand", "clay", "rain", "raincv")

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
soil <- terra::crop(soil, adm1)
rain_sum <- terra::crop(rain_sum, adm1)
rain_cv <- terra::crop(rain_cv, adm1)

# Compute mean rainfall rasters
meanrainsum <- terra::mean(rain_sum, wopt=list(names="rain"))
meanraincv <- terra::mean(rain_cv, wopt=list(names="raincv"))

# Resample price rasters to match soil raster
npkg <- terra::resample(npkg, soil)
mpkg <- terra::resample(mpkg, soil)

soil_2 <- soil[[c("oc", "pH", "sand", "clay")]]
# Create predictor raster stack
preds <- c(soil[[c("oc", "pH", "sand", "clay")]], meanrainsum, meanraincv, npkg, mpkg)

# ----------------------------------------------
# Step 2: Convert Raster Stack to Data Frame
# ----------------------------------------------

# Convert raster stack to data frame
preds_df <- as.data.frame(preds, xy = TRUE, cells = TRUE, na.rm = TRUE)

# Extract cell indices and coordinates
cell_indices <- preds_df$cell
coordinates <- preds_df[, c("x", "y")]

# ----------------------------------------------
# Step 3: Define the Profitability Function
# ----------------------------------------------

profitability <- function(yield, yield0, value, cost) {
  (yield - yield0) * value - cost
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
  # predictors[] <- lapply(predictors, function(x) as.numeric(as.character(x)))
  
  # Ensure that all variables expected by the model are present
  model_variables <- crf$xNames
  missing_vars <- setdiff(model_variables, names(predictors))
  if (length(missing_vars) > 0) {
    stop(paste("Missing predictor variables:", paste(missing_vars, collapse = ", ")))
  }
  
  # Baseline yield (yield without fertilizer)
  const_data_zero <- data.frame(N_fertilizer = 0, P_fertilizer = 0, K_fertilizer = 0)
  
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
    type = "real-valued", fitness = fitness_function,
    lower = c(0, 0, 0), upper = c(200, 100, 100),
    popSize = 20, maxiter = 50, run = 10, optim = TRUE, monitor = FALSE
  )
  
  # Extract optimal NPK rates and maximum profit
  optimal_NPK <- ga_result@solution
  max_profit <- -ga_result@fitnessValue
  
  return(list(N_opt=optimal_NPK[1], P_opt=optimal_NPK[2], K_opt=optimal_NPK[3], max_profit=max_profit))
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
    error = function(e) { # Return NA values and the error message
      list(N_opt = NA, P_opt = NA, K_opt = NA, max_profit = NA, error = e$message)
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
optimal <- rast(results_df_clean[, c("x", "y", "N_opt", "P_opt", "K_opt", "max_profit")], crs = crs(preds))
optimal <- mask(optimal, optimal$max_profit<0, TRUE, 0, filename="data/results/model/optimal.tif", overwrite = TRUE) 
# ----------------------------------------------
# Step 7: Save and Visualize the Resultant Rasters
# ----------------------------------------------

#### plottoing 
# Set up color palette
pal <- colorRampPalette(c(wesanderson::wes_palettes$Zissou1))
pal2 <- colorRampPalette(c(wesanderson::wes_palettes$AsteroidCity2))

plot(optimal, col=pal)


# plot the location of the data 
png("plots/observed_yield.png", units = "in", width = 7, height = 7, res = 600)
terra::plot(meanrainsum, col=pal(5), alpha=0.7, breaks=brks, 
	plg=list(x=5.5, y=3.5,  legend=c('< 500', '500-1000', '1000-1500','1500-2000','2000-3000', '> 3000'), cex=.9, ncol=3, title="mean annual rainfall"))
terra::lines(adm0, col="#000643", lwd = 2)
terra::points(s, col = "#000643", pch = 15)
terra::add_legend("bottomright", cex=.8, bg="white", inset=0.02, legend="field observations", pch=15, col="#000643", xpd = T)

dev.off()


# Plot observations
terra::plot(s, border = "white", add = TRUE, pch = 15, col = "white")

# Plot nitrogen price raster
terra::plot(npkg, col = pal, main = "Predicted Nitrogen price (USD/kg)")
terra::plot(adm0, border = "white", add = TRUE, lwd = 1)

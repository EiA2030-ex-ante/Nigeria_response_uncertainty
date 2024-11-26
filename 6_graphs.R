library(terra)
library(wesanderson)

extrafont::loadfonts(quiet = TRUE)
my_font <- "Frutiger"

# Set up color palette
pal_1 <- colorRampPalette(c(wesanderson::wes_palettes$Zissou1))
pal_2 <- colorRampPalette(c(wesanderson::wes_palettes$AsteroidCity3))

#------------------------------------------------------------
# Load the results for plotting
#------------------------------------------------------------

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

crf <- readRDS("data/results/model/crf.rds")

#-----------------------------------------------------------
# Plot feature importance from RF
#-----------------------------------------------------------
png("data/results/figures/feature_importance.png", units = "in", width = 6, height = 7, res = 600)
varImpPlot(crf, main="Variable Importance", family=my_font, col="#000647", cex=1, pch=15)
dev.off()

#------------------------------------------------------------
# 1-Plotting predicted prices
#------------------------------------------------------------

png("data/results/figures/predicted_price_rasters.png", units = "in", width = 6, height = 5, res = 600)
par(mfrow = c(1, 2), mar = c(0, 0, 0, 0), family = my_font, cex = 0.8, oma = c(0, 0, 0, 0))
terra::plot(npkg, col = pal_1 (7), main = "Predicted Nitrogen price (USD/kg)")
terra::plot(adm0,
            border = "#000643",
            add = TRUE,
            lwd = 1)
terra::plot(mpkg, col = pal_2(7), main = "Predicted Maize price (USD/kg)")
terra::plot(adm0,
            border = "#000643",
            add = TRUE,
            lwd = 1)

dev.off()

#------------------------------------------------------------
# 2-Plotting optimal NPK application rates
#------------------------------------------------------------
png("data/results/figures/Optimal_NPK_rasters.png", units = "in", width = 6, height = 5, res = 600)
par(mar = c(0, 0, 0, 0), family = my_font, cex = 0.8, oma = c(0, 0, 0, 0))
terra::plot(optimal_NPK_raster, col = pal_1(3), main= c("Optimal N (kg/ha)","Optimal P (kg/ha)","Optimal K (kg/ha)"))
terra::plot(adm0, border = "#000643", add = TRUE, lwd = 1)
dev.off()

png("data/results/figures/Optimal_N.png", units = "in", width = 6, height = 6, res = 600)
par(mar = c(0, 0, 0, 0), family = my_font, cex = 1, oma = c(0, 0, 0, 0))
terra::plot(optimal_NPK_raster[[1]], col = scales::alpha(pal_1(4),0.9), main= "Optimal N (kg/ha)", legend=F, breaks = c(0, 50, 100, 150, Inf))
legend("bottom",  cex=1, box.col="white", inset=-0.03, title="", legend=c('0 - 50', '50 - 100','100-150', '>=150'), fill = scales::alpha(pal_1(4),0.9), horiz=T, xpd = T)
plot(adm0, border = "#000643", add = TRUE, lwd = 2)
dev.off()

#------------------------------------------------------------
# 3-Plotting all predictor variables
#------------------------------------------------------------
soil <- crop(soil, adm0, mask = TRUE)

# Resample price rasters to match soil raster
npkg <- resample(npkg, soil)
mpkg <- resample(mpkg, soil)

# Resample optimal NPK raster to match soil raster
optimal_NPK <- resample(optimal_NPK_raster, soil, method = "bilinear")

# Resample rainfall rasters to match soil raster
rain_sum <- crop(rain_sum, adm0, mask = TRUE)
meanrainsum <- mean(rain_sum)
#meanrainsum <- resample(meanrainsum, soil)

rain_cv <- crop(rain_cv, adm0, mask = TRUE)
meanraincv <- mean(rain_cv)
#meanraincv <- resample(meanraincv, soil)
names(meanrainsum) <- "rain"
names(meanraincv) <- "raincv"

# Create predictor raster stack
preds_1 <- c(soil[[c("oc", "pH", "sand", "clay")]], optimal_NPK, meanrainsum, meanraincv)


png("data/results/figures/yield_predictors_rasters.png", units = "in", width = 7, height = 5, res = 600)
par(mar = c(1, 1, 1, 1), family = my_font, cex = 0.8, oma = c(0, 0, 0, 2))
terra::plot(preds_1, col = pal_2(10), main= c("Organic Carbon","pH","Sand","Clay","N (kg/ha)","P (kg/ha)","K (kg/ha)","Mean rainfall (1980-2024)","CV rainfall (1980-2024)"))
terra::plot(adm0, border = "#000643", add = TRUE, lwd = 1)
dev.off()

#------------------------------------------------------------
# 4-Plotting the yield predictions
#------------------------------------------------------------
preds_optimal <- c(soil[[c("oc", "pH", "sand", "clay")]], optimal_NPK, meanrainsum, meanraincv)
predicted_yield_optimal <- terra::predict(preds_optimal, crf, na.rm = TRUE)
terra::plot(predicted_yield_optimal, main = "Predicted yield (kg/ha) under optimal NPK", col = pal_2(7))

preds_constant <- c(soil[[c("oc", "pH", "sand", "clay")]], meanrainsum, meanraincv)
predicted_yield_constant <- predict(preds_constant, crf, const=data.frame(N_fertilizer=100, P_fertilizer=50, K_fertilizer=15), na.rm = TRUE)
terra::plot(predicted_yield_constant, main = "Predicted yield (kg/ha) with constant NPK", col = pal_1(5))

png("data/results/figures/yield_predictions.png", units = "in", width = 7, height = 5, res = 600)
par(mfrow = c(1, 2), mar = c(0, 0, 0, 0), family = my_font, cex = 0.8, oma = c(0, 0, 0, 0))
terra::plot(predicted_yield_optimal, main = "Predicted yield (kg/ha) under optimal NPK", col = pal_2(7))
terra::plot(adm0, border = "#000643", add = TRUE, lwd = 1)
terra::plot(predicted_yield_constant, main = "Predicted yield (kg/ha) with constant NPK", col = pal_2(10))
terra::plot(adm0, border = "#000643", add = TRUE, lwd = 1)
dev.off()


#------------------------------------------------------------
# 5-Plotting the yield uncertainty
#------------------------------------------------------------

pr2_optimal <- rast("data/results/model/yield_simple_optimal_1000_summary.tif")
pr2_constant <- rast("data/results/model/yield_simple_NPK_1000_summary.tif")

pr2_optimal_2 <- pr2_optimal[[c(1, 4)]]
names(pr2_optimal_2) <- c("Mean maize yield (optimal NPK)", "CV maize yield (optimal NPK)")
pr2_constant_2 <- pr2_constant[[c(1, 4)]]
names(pr2_constant_2) <- c("Mean maize yield (constant NPK)", "CV maize yield (constant NPK)")

pr2_results <- c(pr2_constant_2, pr2_optimal_2)

plot(pr2_results, col = pal_2(6))

png("data/results/figures/yield_uncertainty.png", units = "in", width = 7, height = 5, res = 600)
par(mfrow = c(1, 2), mar = c(0, 0, 0, 0), family = my_font, cex = 0.8, oma = c(0, 0, 0, 0))
terra::plot(pr2_results, col = pal_2(6))
terra::plot(adm0, border = "#000643", add = TRUE, lwd = 1)
dev.off()

#------------------------------------------------------------
# 6-Plotting net revenue uncertainty
#------------------------------------------------------------
pr3_optimal <- rast("data/results/model/net_returns_optimal_1000.tif")
pr3_constant <- rast("data/results/model/net_returns_constant_1000.tif")

pr3_optimal_avg <- app(pr3_optimal, mean, na.rm = TRUE)
names(pr3_optimal_avg) <- "Mean net return (optimal NPK)"
pr3_optimal_sd <- app(pr3_optimal, sd, na.rm = TRUE)
names(pr3_optimal_sd) <- "netreturn_sd"
pr3_optimal_cv <- pr3_optimal_sd / pr3_optimal_avg
names(pr3_optimal_cv) <- "CV net return (optimal NPK)"

pr3_constant_avg <- app(pr3_constant, mean, na.rm = TRUE)
names(pr3_constant_avg) <- "Mean net return (constant NPK)"
pr3_constant_sd <- app(pr3_constant, sd, na.rm = TRUE)
names(pr3_constant_sd) <- "netreturn_sd"
pr3_constant_cv <- pr3_constant_sd / pr3_constant_avg
names(pr3_constant_cv) <- "CV net return (constant NPK)"

pr3_results <- c(pr3_constant_avg, pr3_constant_cv, pr3_optimal_avg, pr3_optimal_cv)

plot(pr3_results, col = pal_2(5))

png("data/results/figures/net_revenue_uncertainty.png", units = "in", width = 7, height = 5, res = 600)
par(mar = c(0, 0, 0, 0), family = my_font, cex = 0.8, oma = c(0, 0, 0, 0))
terra::plot(pr3_results, col = pal_2(5))
terra::plot(adm0, border = "#000643", add = TRUE, lwd = 2)
terra::plot(adm0, border = "#000643", add = TRUE, lwd = 2)
dev.off()


#------------------------------------------------------------
# 7-Plotting the intervention areas
#------------------------------------------------------------

# Define class labels
class_labels <- data.frame(
  value = 1:4,
  label = c(
    "Low returns, low variance",
    "High returns, low variance",
    "Low returns, high variance",
    "High returns, high variance"
  )
)
avg_mean_constant <- global(pr3_constant_avg, "mean", na.rm = TRUE)[[1]] 
avg_cv_constant <- global(pr3_constant_cv, "mean", na.rm = TRUE)[[1]]
avg_mean_optimal <- global(pr3_optimal_avg, "mean", na.rm = TRUE)[[1]]
avg_cv_optimal <- global(pr3_optimal_cv, "mean", na.rm = TRUE)[[1]]

pr3_avg_rc_oN <- pr3_optimal_avg > avg_mean_optimal
pr3_cv_rc_oN <- pr3_optimal_cv > avg_cv_optimal

pr3_avg_rc_cN <- pr3_constant_avg > avg_mean_constant
pr3_cv_rc_cN <- pr3_constant_cv > avg_cv_constant

classified_raster_oN <- pr3_avg_rc_oN + 2 * pr3_cv_rc_oN + 1
classified_raster_cN <- pr3_avg_rc_cN + 2 * pr3_cv_rc_cN + 1

levels(classified_raster_oN) <- class_labels
levels(classified_raster_cN) <- class_labels
plot(classified_raster_oN, col = pal_1(4),
     legend = TRUE,
     plg = list(x = "bottomright"))

plot(classified_raster_cN, col = pal_1(4),
     legend = TRUE,
     plg = list(x = "bottomright"))

png("data/results/figures/intervention_areas_oN.png", units = "in", width = 6, height = 5, res = 600)
par(mar = c(0, 0, 0, 0), family = my_font, cex = 1, oma = c(0, 0, 0, 0))
terra::plot(classified_raster_oN, col = pal_1(4),
            legend = TRUE,
            main = "Intervention Areas Based on Expected Returns (Optimal N)",
            plg = list(x = "bottomright"))
terra::plot(adm0, border = "#000643", add = TRUE, lwd = 1.3)
dev.off()

png("data/results/figures/intervention_areas_cN.png", units = "in", width = 6, height = 5, res = 600)
par(mar = c(0, 0, 0, 0), family = my_font, cex = 1, oma = c(0, 0, 0, 0))
terra::plot(classified_raster_cN, col = pal_1(4),
            legend = TRUE,
            main = "Intervention Areas Based on Expected Returns (Constant N)",
            plg = list(x = "bottomright"))
terra::plot(adm0, border = "#000643", add = TRUE, lwd = 1.3)
dev.off()

#------------------------------------------------------------
# 8-Plotting the profit uncertainity
#------------------------------------------------------------


# replace negative values with 0
profit_optimal <- rast("data/results/model/profit_optimal.tif")
profit_constant <- rast("data/results/model/profit_constant.tif")


profit_optimal[profit_optimal < 0] <- 0
profit_constant[profit_constant < 0] <- 0

profit_optimal_avg <- app(profit_optimal, mean, na.rm = TRUE)
names(profit_optimal_avg) <- "Mean profit (optimal NPK)"
profit_optimal_sd <- app(profit_optimal, sd, na.rm = TRUE)
names(profit_optimal_sd) <- "profit_sd"
profit_optimal_cv <- profit_optimal_sd / profit_optimal_avg
names(profit_optimal_cv) <- "CV profit (optimal NPK)"


profit_constant_avg <- app(profit_constant, mean, na.rm = TRUE)
names(profit_constant_avg) <- "Mean profit (constant NPK)"
profit_constant_sd <- app(profit_constant, sd, na.rm = TRUE)
names(profit_constant_sd) <- "profit_sd"
profit_constant_cv <- profit_constant_sd / profit_constant_avg
names(profit_constant_cv) <- "CV profit (constant NPK)"

profit_results <- c(profit_constant_avg, profit_constant_cv, profit_optimal_avg, profit_optimal_cv)

plot(profit_results, col = pal_2(4))

png("data/results/figures/profit_uncertainty_2.png", units = "in", width = 7, height = 5, res = 600)
par(mar = c(0, 0, 0, 0), family = my_font, cex = 0.8, oma = c(0, 0, 0, 0))
terra::plot(profit_results)
terra::plot(adm0, border = "#000643", add = TRUE, lwd = 2)
dev.off()

png("data/results/figures/profit_uncertainty_oN.png", units = "in", width = 7, height = 5, res = 600)
par(mar = c(0, 0, 0, 0), family = my_font, cex = 0.8, oma = c(0, 0, 0, 0))
terra::plot(profit_results[[c(1,2)]], col = pal_1(5))
terra::plot(adm0, border = "#000643", add = TRUE, lwd = 2)
dev.off()

png("data/results/figures/profit_uncertainty_cN.png", units = "in", width = 7, height = 5, res = 600)
par(mar = c(0, 0, 0, 0), family = my_font, cex = 0.8, oma = c(0, 0, 0, 0))
terra::plot(profit_results[[c(3,4)]], col = pal_1(5))
terra::plot(adm0, border = "#000643", add = TRUE, lwd = 2)
dev.off()

avg_mean_constant_pr <- global(profit_constant_avg, "mean", na.rm = TRUE)[[1]]
avg_cv_constant_pr <- global(profit_constant_cv, "mean", na.rm = TRUE)[[1]]
avg_mean_optimal_pr <- global(profit_optimal_avg, "mean", na.rm = TRUE)[[1]]
avg_cv_optimal_pr <- global(profit_optimal_cv, "mean", na.rm = TRUE)[[1]]

profit_constant_rc <- profit_constant_avg > avg_mean_constant_pr
profit_constant_cv_rc <- profit_constant_cv > avg_cv_constant_pr
profit_optimal_rc <- profit_optimal_avg > avg_mean_optimal_pr
profit_optimal_cv_rc <- profit_optimal_cv > avg_cv_optimal_pr

classified_raster_optimal <- profit_optimal_rc + 2 * profit_optimal_cv_rc + 1
classified_raster_constant <- profit_constant_rc + 2 * profit_constant_cv_rc + 1

levels(classified_raster_optimal) <- class_labels
levels(classified_raster_constant) <- class_labels

plot(classified_raster_constant, col = pal_1(4),
     legend = TRUE,
     plg = list(x = "bottomright"))
png("data/results/figures/intervention_areas_profit_constant.png", units = "in", width = 6, height = 5, res = 600)
par(mar = c(0, 0, 0, 0), family = my_font, cex = 1, oma = c(0, 0, 0, 0))
terra::plot(classified_raster_constant, col = pal_1(4),
            legend = TRUE,
            main = "Intervention Areas Based on Expected Returns (Constant NPK)",
            plg = list(x = "bottomright"))
terra::plot(adm0, border = "#000643", add = TRUE, lwd = 1.3)
dev.off()

png("data/results/figures/intervention_areas_profit_optimal.png", units = "in", width = 6, height = 5, res = 600)
par(mar = c(0, 0, 0, 0), family = my_font, cex = 1, oma = c(0, 0, 0, 0))
terra::plot(classified_raster_optimal, col = pal_1(4),
            legend = TRUE,
            main = "Intervention Areas Based on Expected Returns (Optimal NPK)",
            plg = list(x = "bottomright"))
terra::plot(adm0, border = "#000643", add = TRUE, lwd = 1.3)
dev.off()

png("data/results/figures/intervention_areas_profit_both.png", units = "in", width = 6, height = 5, res = 600)
par(mfrow = c(1, 2), mar = c(0, 0, 0, 0), family = my_font, cex = 0.8, oma = c(0, 0, 0, 0))
terra::plot(classified_raster_constant, col = pal_1(4),
            legend = TRUE,
            main = "Intervention Areas Based on Expected Returns (Constant NPK)",
            plg = list(x = "bottomright"))
terra::plot(adm0, border = "#000643", add = TRUE, lwd = 1.3)
terra::plot(classified_raster_optimal, col = pal_1(4),
            legend = TRUE,
            main = "Intervention Areas Based on Expected Returns (Optimal NPK)",
            plg = list(x = "bottomright"))
terra::plot(adm0, border = "#000643", add = TRUE, lwd = 1.3)
dev.off()

# calculate intervention areas by regions 

crop_area <- terra::rast("data/intermediate/spam_harv_area_processed.tif")
maize_area <- crop_area[["MAIZ"]]
maize_area_nigeria <- crop(maize_area, adm0, mask = TRUE)
maize_area_nigeria <- resample(maize_area_nigeria, classified_raster_constant) 
crop_area_region <- cbind(data.frame(adm1[c('COUNTRY', 'NAME_1')]), terra::extract(maize_area_nigeria, adm1, fun=sum, na.rm=T, ID=F))

# calculate the area of each intervention area
maize_intervention_area_1 <- maize_area_nigeria * (classified_raster_constant == 1)
maize_intervention_area_2 <- maize_area_nigeria * (classified_raster_constant == 2)
maize_intervention_area_3 <- maize_area_nigeria * (classified_raster_constant == 3)
maize_intervention_area_4 <- maize_area_nigeria * (classified_raster_constant == 4)

maize_intervention_area <- c(maize_area_nigeria, maize_intervention_area_1, maize_intervention_area_2, maize_intervention_area_3, maize_intervention_area_4)

names(maize_intervention_area) <- c("Maize_area", "Low returns, low variance", "High returns, low variance", "Low returns, high variance", "High returns, high variance")
plot(maize_intervention_area, col = pal_1(4))


maize_intervention_area_df <- cbind(data.frame(adm1[c('COUNTRY', 'NAME_1')]), terra::extract(maize_intervention_area, adm1, fun=sum, na.rm=T, ID=F))

library(tidyverse)
eia_colors <- c("#80a172","#02401B", "#B74F20",  "#d62728", "#FDD262","#D3DDDC","#9467bd","#C7B19C","#FFBB70",
                "#bcbd22" )
df_top10 <- maize_intervention_area_df %>% 
  arrange(desc(Maize_area)) %>% 
  head(10)%>%
  pivot_longer(cols = -c(COUNTRY, NAME_1, Maize_area), names_to = "Intervention_area", values_to = "Area_ha")%>%
  mutate(percent_area = Area_ha / Maize_area * 100)

# stacked bar plot
plt_intervention_area <- ggplot(df_top10, aes(x = reorder(NAME_1,-percent_area), y = percent_area, fill = Intervention_area)) +
  geom_bar(stat = "identity",width = 0.7,
           position = "stack") +
  #coord_flip() +
  geom_text(aes(label = round(percent_area, 1)), position = position_stack(vjust = 0.5), 
            size = 4, family=my_font, color='#000643') +
  theme_minimal() +
  theme(legend.position = "right") +
  labs(title = "",
       x = "Top 10 Maize cultivating Regions",
       y = "Percent of maize area",
       fill = "Intervention area") +
  scale_fill_manual(values = alpha(eia_colors, 0.8)) +
  theme_minimal() +
  theme(
    text = element_text(family = my_font, size = 16),
    #title = element_blank(),
    axis.text.y = element_text(size = 12, family = my_font),
    axis.title = element_text(size = 14, family = my_font),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.major = element_line(
      colour = "#DBDBDB",
      linewidth = 0.5,
      linetype = "dotted"
    ),
    plot.caption = element_text(
      size = 8,
      family = my_font
    ),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = "white"),
    panel.border = element_rect(
      colour = "#DBDBDB",
      fill = NA,
      linewidth = 0.5
    )
  )

plt_intervention_area

ggsave("data/results/figures/top10_maize_intervention_area.png", plt_intervention_area, width = 11, height = 6,units = 'in', dpi = 600)

# Clustering of intervention area
profit_stack <- c(profit_optimal_avg, profit_optimal_cv)
profit_values <- as.data.frame(profit_stack, xy = TRUE, na.rm = TRUE)

profit_scaled <- scale(profit_values[, c(3,4)])

set.seed(251124)
kmeans_results <- kmeans(profit_scaled, centers = 4, nstart = 25)
profit_values$cluster <- as.factor(kmeans_results$cluster)

profit_cluster_raster <- terra::rast(profit_values, type='xyz')

library(cluster)

# Calculate distances (profit_scaled is scaled profit data)
dist_matrix <- dist(profit_scaled)

# Perform hierarchical clustering
hclust_result <- hclust(dist_matrix, method = "ward.D2")

# Cut the tree into clusters (e.g., 4 clusters)
profit_values$cluster_h <- cutree(hclust_result, k = 4)

# Convert back to raster
profit_cluster_raster <- rast(profit_values, type = "xyz")
plot(profit_cluster_raster, col = pal_2(4))

library(fpc)

# Run DBSCAN (adjust eps and MinPts as needed)
dbscan_result <- dbscan(profit_scaled, eps = 3, MinPts = 50)

# Assign clusters
profit_values$cluster_db <- dbscan_result$cluster

# Convert back to raster
profit_cluster_raster <- rast(profit_values, type = "xyz")
plot(profit_cluster_raster, col = pal_2(length(unique(dbscan_result$cluster))),
     main = "DBSCAN Clustering")

rm(list=ls())

library(patchwork)
library(tidyverse)
library(raster)
library(tictoc)
library(spdep)
library(sf)
library(data.table)

library(sp)
library(spatstat)
library(ncf)
library(parallel)
library(mgcv)
library(ranger)

library(randomForest)

##### Spatial autocorrelation plot----
data_sample <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_Full_glad_extent_5m.rds")
data_sample <- data_sample[data_sample$cover>=0.3,]

data_sample <- data_sample[!is.na(data_sample$longitude),]


set.seed(1000)
# data_subsample = data_sample[sample(nrow(data_sample), 40000),]
# make a copy of the random subsampled table
# duplicateTable = data_subsample

# type_gov<- "WDPA_binary"
# covars<-c("MAT_mean", "TAP_mean",  "fireFreq_mean", "Humidity_mean", "PPP_Mean", "accessibity")

#  define the train vector
retainedVars = c("MAT_mean", "TAP_mean",  "fireFreq_mean", "Humidity_mean", "PPP_Mean", "accessibity") 


data_sample <- data_sample[,c('WDPA_binary', retainedVars, "longitude","latitude")]%>%drop_na()

# data_sample = data_sample[sample(nrow(data_sample), 2000),]

# generate the formula for the game model
# formulaString = as.formula(paste('WDPA_binary','~', paste("s(",retainedVars,")", collapse="+",sep=""), sep=""))
formulaString = as.formula(paste('WDPA_binary','~', paste(retainedVars, collapse="+",sep=""), sep=""))

# traint the gam model
# gamMulti = gam(data = data_sample, formula = formulaString) 

# gamMulti = ranger(data = data_sample, formula = formulaString, num.trees = 1000)


gamMulti = glm(data = data_sample, formula = formulaString, family = binomial())


# data_sample$gamResiduals = gamMulti$residuals

data_sample$gamResiduals = gamMulti$predictions-data_sample$WDPA_binary
coordinates(data_sample) = ~longitude+latitude
proj4string(data_sample) = CRS("+init=epsg:4326")

# # calculate the Moran I changes along the spatial distance
tic()
spatialMoranDynamics = spline.correlog(x=coordinates(data_sample)[,1], y=coordinates(data_sample)[,2],z=data_sample$gamResiduals, resamp=10, quiet=TRUE,latlon=T,xmax=500)
toc()

saveRDS(spatialMoranDynamics, file="C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/spatialMoranDynamics_FRAG_2000data_max500km_RF.rds")

# make the plot
# pdf(paste("Plots/Figure_SX_MoranI_along_distance_of_glm_model_residuals_with_all_vars_glm_df_sqrt_n_resample_20_100000.pdf",sep=""),width = 5, height=4)
  plot(spatialMoranDynamics,xlim=c(0,500),ylim=c(-0.5,1),  xlab = "Distance (km)",
       ylab = "Moran's I")
  abline(v=40,col="red", lty=2)
text(100,0.5,"40km")


# dev.off()


##### Spatial thinning----
data_sample <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_Full_glad_extent_5m.rds")
data_sample <- data_sample[data_sample$cover>=0.3,]

data_sample <- data_sample[!is.na(data_sample$longitude),]

# data_sample <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_SubSet_WDPA_T30perc.rds")

# data_sample <- data_sample%>%group_by(WDPA_binary, Mega_Biome)%>%slice_sample(prop=0.02)
tic()
sf_data <- st_as_sf(data_sample, coords = c("longitude", "latitude"), crs = 4326)
meters_crs <- "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs"  # Adjust UTM zone based on your data location
sf_data <- st_transform(sf_data, crs = meters_crs)

sf_data$geometry <- st_buffer(sf_data$geometry, 40000)  # Buffer by 30 km
toc()

tic()
# Create a spatial index to speed up intersection checks
st_crs(sf_data) <- meters_crs  # Confirm CRS is suitable for distance measurement
idx <- st_intersects(sf_data, sparse = TRUE)

# Mark rows to keep
to_keep <- rep(TRUE, nrow(sf_data))


# Efficiently mark overlapping buffers, keeping the first occurrence
for (i in seq_along(idx)) {
  if (to_keep[i]) {
    to_keep[idx[[i]]] <- FALSE
    to_keep[i] <- TRUE  # Ensure the original point is kept
  }
}
toc()
# Subset the data to keep only non-overlapping points
data_thinned <- data_sample[to_keep, ]

saveRDS(data_thinned, file="C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_SubSet_WDPA_T30perc_buffer40km.rds")

table(data_thinned$WDPA_binary)




type_gov<- "WDPA_binary"
covars<-c("MAT_mean", "TAP_mean",  "fireFreq_mean", "Humidity_mean", "PPP_Mean", "accessibity")

data_sample <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_SubSet_WDPA_T30perc.rds")

data_sample <- data_sample%>%group_by(WDPA_binary, Mega_Biome)%>%slice_sample(prop=0.02)

formula_glm<- as.formula(paste0(type_gov, "~", paste0(covars, collapse = "+")) )

print(formula_glm)

model <- glm(formula_glm, data = data_sample, family = binomial())




library(geosphere)

# Calculate the great-circle distance matrix
# distm function expects coordinates in longitude, latitude order
tic()
distance_matrix <- distm(coordinates, fun = distHaversine)
toc()

distance_matrix[distance_matrix==0] <- NA
min(distance_matrix, na.rm=T)
hist(distance_matrix)



residuals <- residuals(model)

# Create a neighbors list and weights
tic()
coordinates <- data_sample[c("longitude", "latitude")]  # Adjust column names as needed


dist <- nbdists(knn2nb(knearneigh(coordinates, longlat = TRUE)), coordinates)

toc()

min(dist)


neighbors <- dnearneigh(coordinates, 0, 100, longlat = TRUE)  # Distance threshold, e.g., 30 km
weights <- nb2listw(neighbors, zero.policy = T)
toc()
# 
# moran_test_results <- moran.test(residuals, weights)
# print(moran_test_results)




# Maximum distance for creating distance bands
max_dist <- 1000
dist_seq <- seq(from = 100, to = max_dist, by = 100)  # Adjust this interval based on your data scale

# Function to calculate Moran's I
calc_moran_i <- function(dist) {
  nb <- dnearneigh(coordinates, 0, dist, longlat = TRUE)
  if (length(nb) == 0) return(NA)  # Avoid errors if no neighbors at this band
  lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
  moran.test(residuals, lw)$estimate['Moran I']
}

# Calculate Moran's I for each distance band
moran_values <- sapply(dist_seq, calc_moran_i)

# Create a data frame for plotting
moran_df <- data.frame(
  Distance = dist_seq,
  MoranI = moran_values
)

ggplot(moran_df, aes(x = Distance, y = MoranI)) +
  geom_line() +  # Use geom_point() if you prefer points over lines
  labs(title = "Moran's I vs. Distance of Model Residuals",
       x = "Distance",
       y = "Moran's I Index") +
  theme_minimal()

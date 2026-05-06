################################################################################
########## EXTRACTING ELEVATION DATA IN SWITZERLAND AND VISUALIZATION ##########
################################################################################

#### Load required packages ####
library(sf)        # modern spatial data handling (simple features)
library(elevatr)   # download elevation data
library(raster)    # raster data manipulation (maps)
library(ggplot2)   # data visualization
library(rnaturalearth)

# Disable s2 geometry engine (can avoid issues in some spatial operations)
sf_use_s2(FALSE)


#### Load Switzerland boundaries ####
# Retrieve country borders from Natural Earth
Switzerland <- ne_countries(
  scale = "medium",
  returnclass = "sf",
  country = "Switzerland"
)


#### Download elevation data ####
# z controls resolution (higher = more detail but slower)
elevation_switzerland <- get_elev_raster(Switzerland, z = 8)

# Quick visualization of the elevation raster
x11()
plot(elevation_switzerland)


#######################################################
#######################################################
##### Salamandre tachetée - Salamandra salamandra #####
#######################################################
#######################################################

# Prepare sampling points #
# Convert coordinates into a spatial object (SpatialPoints format)
spatial_points1 <- SpatialPoints(
  coords = matrix_full_eco1[, c("longitude", "latitude")],
  proj4string = CRS("+proj=longlat +datum=WGS84")
)

# Extract elevation values #
# Extract raster values at each point location
elevation1 <- raster::extract(elevation_switzerland, spatial_points1)

matrix_full_eco_elev1 <- data.frame(
  matrix_full_eco1,
  elevation = elevation1
)

# Visualization: elevation distribution #
p1 <- ggplot(matrix_full_eco_elev1, aes(x = elevation1, fill = Climate_Re)) +
  geom_density(alpha = 0.5, adjust = 3) +  # smoothed density curves
  labs(
    title = "Elevation Distribution by Climate",
    x = "Elevation (m)",
    y = "Density"
  ) +
  theme_minimal()

# Display the plot
x11()
print(p1)


#######################################################
#######################################################
###### Triton alpestre - Ichthyosaura alpestris ######
#######################################################
#######################################################

# Prepare sampling points #
# Convert coordinates into a spatial object (SpatialPoints format)
spatial_points2 <- SpatialPoints(
  coords = matrix_full_eco2[, c("longitude", "latitude")],
  proj4string = CRS("+proj=longlat +datum=WGS84")
)

# Extract elevation values #
# Extract raster values at each point location
elevation2 <- raster::extract(elevation_switzerland, spatial_points2)

matrix_full_eco_elev2 <- data.frame(
  matrix_full_eco2,
  elevation = elevation2
)

# Visualization: elevation distribution #
p2 <- ggplot(matrix_full_eco_elev2, aes(x = elevation2, fill = Climate_Re)) +
  geom_density(alpha = 0.5, adjust = 3) +  # smoothed density curves
  labs(
    title = "Elevation Distribution by Climate",
    x = "Elevation (m)",
    y = "Density"
  ) +
  theme_minimal()

# Display the plot
x11()
print(p2)


#######################################################
#######################################################
## Couleuvre à collier helvétique - Natrix helvetica ##
#######################################################
#######################################################
# Prepare sampling points #
# Convert coordinates into a spatial object (SpatialPoints format)
spatial_points3 <- SpatialPoints(
  coords = matrix_full_eco3[, c("longitude", "latitude")],
  proj4string = CRS("+proj=longlat +datum=WGS84")
)

# Extract elevation values #
# Extract raster values at each point location
elevation3 <- raster::extract(elevation_switzerland, spatial_points3)

matrix_full_eco_elev3 <- data.frame(
  matrix_full_eco3,
  elevation = elevation3
)

# Visualization: elevation distribution #
p3 <- ggplot(matrix_full_eco_elev3, aes(x = elevation3, fill = Climate_Re)) +
  geom_density(alpha = 0.5, adjust = 3) +  # smoothed density curves
  labs(
    title = "Elevation Distribution by Climate",
    x = "Elevation (m)",
    y = "Density"
  ) +
  theme_minimal()

# Display the plot
x11()
print(p3)


#######################################################
#######################################################
############## All species together ##################
#######################################################
#######################################################

# Prepare sampling points #
# Convert coordinates into a spatial object (SpatialPoints format)
spatial_points4 <- SpatialPoints(
  coords = matrix_allsp_eco[, c("longitude", "latitude")],
  proj4string = CRS("+proj=longlat +datum=WGS84")
)

# Extract elevation values #
# Extract raster values at each point location
elevation4 <- raster::extract(elevation_switzerland, spatial_points4)

matrix_allsp_eco_elev <- data.frame(
  matrix_allsp_eco,
  elevation = elevation4
)

# Visualization: elevation distribution #
p4 <- ggplot(matrix_allsp_eco_elev, aes(x = elevation4, fill = Climate_Re)) +
  geom_density(alpha = 0.5, adjust = 3) +  # smoothed density curves
  labs(
    title = "Elevation Distribution by Climate",
    x = "Elevation (m)",
    y = "Density"
  ) +
  theme_minimal()

# Display the plot
x11()
print(p4)

###############################################################################
######### Adding ecosystem data to species occurence coordinates data #########
###############################################################################

##### Needed packages ####
library(raster)          # raster: to read and manipulate raster files
library(sf)              # sf: to handle vector spatial data
library(rnaturalearth)   # rnaturalearth: to download country boundaries
library(ggplot2)         # ggplot2: to create graphs


#### Parameters ####
# Load ecosystem raster #
# Define the path to the GeoTIFF file
file_path <- '/Users/eizorandin/Desktop/Biodiversity Conservation/BDA/myproject/data/WorldEcosystem.tif'

# Read the raster layer
ecosystem_raster <- raster(file_path)

# Display basic information about the raster
print(ecosystem_raster)

# Plot full raster for visual ckeck
x11()
plot(ecosystem_raster, main = "Original Ecosystem Raster")


# Load Switzerland boundaries #
# Download the country boundary as an sf object
Switzerland <- ne_countries(
  scale = "medium",
  returnclass = "sf",
  country = "Switzerland"
)


# Plot the country boundary
x11()
plot(st_geometry(Switzerland), main = "Boundary of Switzerland")


# Crop and mask raster to Switzerland #
# crop() keeps only the rectangular extent around Switzerland
r2 <- crop(ecosystem_raster, extent(Switzerland))
plot(r2)

# mask() keeps only the pixels that fall inside the country boundary
ecosystem_switzerland <- mask(r2, Switzerland)

# Plot the cropped and masked raster
x11()
plot(ecosystem_switzerland, main = "Ecosystem Raster Restricted to Switzerland")


# Load the ecosystem metadata table #
# This metadata table links the numeric raster code to descriptive ecosystem names
metadata_eco <- read.delim("/Users/eizorandin/Desktop/Biodiversity Conservation/BDA/myproject/data/WorldEcosystem.metadata.tsv")

# Inspect the metadata table
head(metadata_eco)
names(metadata_eco)


#######################################################
#######################################################
##### Salamandre tachetée - Salamandra salamandra #####
#######################################################
#######################################################

# Convert species coordinates into spatial points #
spatial_points1 <- SpatialPoints(
  coords = matrix_full_date1[, c("longitude", "latitude")],
  proj4string = CRS("+proj=longlat +datum=WGS84")
)


# Add the occurrence points on top of the ecosystem map
x11()
plot(ecosystem_switzerland, main = "Species Occurrences on Ecosystem Map")
plot(spatial_points1, add = TRUE, pch = 1, cex = 1.2)


# Extract ecosystem values at each occurence point #
# extract() retrieves the raster value at the location of each point
# Each point receives the ecosystem code of the raster cell where it falls
eco_values1 <- raster::extract(ecosystem_switzerland, spatial_points1)

# Check the extracted values
head(eco_values1)

# Add the extracted ecosystem values to the original data frame #
# Create a new data frame by adding the extracted ecosystem values
matrix_full_eco1 <- data.frame(matrix_full_date1, eco_values1)

# Inspect the result
head(matrix_full_eco1)


# Merge the extracted values with metadata #
# Merge the occurrence table with the metadata table
# by.x = "eco_values" means the ecosystem code in our occurrence table
# by.y = "Value" means the corresponding code column in the metadata table
matrix_full_eco1 <- merge(
  matrix_full_eco1,
  metadata_eco,
  by.x = "eco_values1",
  by.y = "Value"
)

# Inspect the enriched table
head(matrix_full_eco1)
nrow(matrix_full_eco1)

# Visualize the number of observations per climate category and species #
# Create a bar plot showing how many observations of each species
# are found in each climate category
x11()
ggplot(matrix_full_eco1, aes(x = Climate_Re, fill = species)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Count of Observations of Each Species by Climate",
    x = "Climate category",
    y = "Number of observations"
  ) +
  theme_minimal()


#######################################################
#######################################################
###### Triton alpestre - Ichthyosaura alpestris ######
#######################################################
#######################################################

# Convert species coordinates into spatial points #
spatial_points2 <- SpatialPoints(
  coords = matrix_full_date2[, c("longitude", "latitude")],
  proj4string = CRS("+proj=longlat +datum=WGS84")
)


# Add the occurrence points on top of the ecosystem map
x11()
plot(ecosystem_switzerland, main = "Species Occurrences on Ecosystem Map")
plot(spatial_points2, add = TRUE, pch = 1, cex = 1.2)


# Extract ecosystem values at each occurence point #
# extract() retrieves the raster value at the location of each point
# Each point receives the ecosystem code of the raster cell where it falls
eco_values2 <- raster::extract(ecosystem_switzerland, spatial_points2)

# Check the extracted values
head(eco_values2)

# Add the extracted ecosystem values to the original data frame #
# Create a new data frame by adding the extracted ecosystem values
matrix_full_eco2 <- data.frame(matrix_full_date2, eco_values2)

# Inspect the result
head(matrix_full_eco2)


# Merge the extracted values with metadata #
# Merge the occurrence table with the metadata table
# by.x = "eco_values" means the ecosystem code in our occurrence table
# by.y = "Value" means the corresponding code column in the metadata table
matrix_full_eco2 <- merge(
  matrix_full_eco2,
  metadata_eco,
  by.x = "eco_values2",
  by.y = "Value"
)

# Inspect the enriched table
head(matrix_full_eco2)
nrow(matrix_full_eco2)

# Visualize the number of observations per climate category and species #
# Create a bar plot showing how many observations of each species
# are found in each climate category
x11()
ggplot(matrix_full_eco2, aes(x = Climate_Re, fill = species)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Count of Observations of Each Species by Climate",
    x = "Climate category",
    y = "Number of observations"
  ) +
  theme_minimal()


#######################################################
#######################################################
## Couleuvre à collier helvétique - Natrix helvetica ##
#######################################################
#######################################################
# Convert species coordinates into spatial points #
spatial_points3 <- SpatialPoints(
  coords = matrix_full_date3[, c("longitude", "latitude")],
  proj4string = CRS("+proj=longlat +datum=WGS84")
)


# Add the occurrence points on top of the ecosystem map
x11()
plot(ecosystem_switzerland, main = "Species Occurrences on Ecosystem Map")
plot(spatial_points3, add = TRUE, pch = 1, cex = 1.2)


# Extract ecosystem values at each occurence point #
# extract() retrieves the raster value at the location of each point
# Each point receives the ecosystem code of the raster cell where it falls
eco_values3 <- raster::extract(ecosystem_switzerland, spatial_points3)

# Check the extracted values
head(eco_values3)

# Add the extracted ecosystem values to the original data frame #
# Create a new data frame by adding the extracted ecosystem values
matrix_full_eco3 <- data.frame(matrix_full_date3, eco_values3)

# Inspect the result
head(matrix_full_eco3)
nrow(matrix_full_eco3)


# Merge the extracted values with metadata #
# Merge the occurrence table with the metadata table
# by.x = "eco_values" means the ecosystem code in our occurrence table
# by.y = "Value" means the corresponding code column in the metadata table
matrix_full_eco3 <- merge(
  matrix_full_eco3,
  metadata_eco,
  by.x = "eco_values3",
  by.y = "Value"
)

# Inspect the enriched table
head(matrix_full_eco3)

# Visualize the number of observations per climate category and species #
# Create a bar plot showing how many observations of each species
# are found in each climate category
x11()
ggplot(matrix_full_eco3, aes(x = Climate_Re, fill = species)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Count of Observations of Each Species by Climate",
    x = "Climate category",
    y = "Number of observations"
  ) +
  theme_minimal()


#######################################################
#######################################################
################# All species matrix #################
#######################################################
#######################################################
# Convert species coordinates into spatial points #
spatial_points4 <- SpatialPoints(
  coords = matrix_allsp[, c("longitude", "latitude")],
  proj4string = CRS("+proj=longlat +datum=WGS84")
)


# Add the occurrence points on top of the ecosystem map
x11()
plot(ecosystem_switzerland, main = "Species Occurrences on Ecosystem Map")
plot(spatial_points4, add = TRUE, pch = 1, cex = 1.2)


# Extract ecosystem values at each occurence point #
# extract() retrieves the raster value at the location of each point
# Each point receives the ecosystem code of the raster cell where it falls
eco_values4 <- raster::extract(ecosystem_switzerland, spatial_points4)

# Check the extracted values
head(eco_values4)

# Add the extracted ecosystem values to the original data frame #
# Create a new data frame by adding the extracted ecosystem values
matrix_allsp_eco <- data.frame(matrix_allsp, eco_values4)

# Inspect the result
head(matrix_allsp_eco)


# Merge the extracted values with metadata #
# Merge the occurrence table with the metadata table
# by.x = "eco_values" means the ecosystem code in our occurrence table
# by.y = "Value" means the corresponding code column in the metadata table
matrix_allsp_eco <- merge(
  matrix_allsp_eco,
  metadata_eco,
  by.x = "eco_values4",
  by.y = "Value"
)

# Inspect the enriched table
head(matrix_allsp_eco)

# Visualize the number of observations per climate category and species #
# Create a bar plot showing how many observations of each species
# are found in each climate category
x11()
ggplot(matrix_allsp_eco, aes(x = Climate_Re, fill = species)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Count of Observations of Each Species by Climate",
    x = "Climate category",
    y = "Number of observations"
  ) +
  theme_minimal()



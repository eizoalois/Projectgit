#############################################################
### Occurence data from GBIF and iNat for desired species ###
#############################################################

#### Needed packages ####
library(rgbif)         # access to GBIF data
library(rnaturalearth) # country maps
library(ggplot2)       # graphics
library(rinat)         # access to iNaturalist data
library(raster)        # spatial extent management
library(dplyr)         # table manipulation
library(sf)            # modern spatial objects

# Disable spherical geometry for simpler spatial operations
sf_use_s2(FALSE)


##### Parameters #####
# Species of interest
mysp1 <- "Salamandra salamandra"
mysp2 <- "Ichthyosaura alpestris"
mysp3 <- "Natrix helvetica"

# Time filtering 
date_start <- as.Date("2020-01-01")
date_end   <- as.Date("2025-12-31")

# Simplified geographic extent for Switzerland
xmin <- 6
xmax <- 11
ymin <- 46
ymax <- 48

# Base map of Switzerland #
# Download the outline of Switzerland
Switzerland <- ne_countries(
  scale = "medium",
  returnclass = "sf",
  country = "Switzerland"
)

# Simple visualization of the map
x11()
ggplot(data = Switzerland) +
  geom_sf(fill = "grey95", color = "black") +
  theme_classic()


#######################################################
#######################################################
##### Salamandre tachetée - Salamandra salamandra #####
#######################################################
#######################################################

# Download GBIF data with coordinates #
gbif_raw1 <- occ_data(
  scientificName = mysp1,
  hasCoordinate = TRUE,
  country = "CH",
  limit = 1000
)

# Extract the main data table
gbif_occ1 <- gbif_raw1$data

# Quick inspection
head(gbif_occ1)
names(gbif_occ1)
nrow(gbif_occ1)

# Map showing GBIF occurrences only
X11()
ggplot(data = Switzerland) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = gbif_occ1,
    aes(x = decimalLongitude, y = decimalLatitude),
    size = 3,
    shape = 21,
    fill = "darkgreen",
    color = "black"
  ) +
  labs(title = "Sala GBIF")
  theme_classic()

# Format GBIF data
data_gbif1 <- data.frame(
  species   = gbif_occ1$species,
  latitude  = gbif_occ1$decimalLatitude,
  longitude = gbif_occ1$decimalLongitude,
  date_obs  = as.Date(gbif_occ1$eventDate),
  source    = "gbif"
)

# Check structure
head(data_gbif1)
str(data_gbif1)
unique(data_gbif1$species)


# Download iNat data #
inat_raw1 <- get_inat_obs(
  taxon_name = mysp1,
  place_id = "switzerland",
  maxresults = 1000
)


# Inspect the structure
head(inat_raw1)
names(inat_raw1)
nrow(inat_raw1)


# Map showing iNaturalist occurrences only
X11()
ggplot(data = Switzerland) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = inat_raw1,
    aes(x = longitude, y = latitude),
    size = 3,
    shape = 21,
    fill = "darkred",
    color = "black"
  ) +
  labs(title = "Sala iNat")
  theme_classic()

# Format iNat data 
data_inat1 <- data.frame(
  species   = inat_raw1$scientific_name,
  latitude  = inat_raw1$latitude,
  longitude = inat_raw1$longitude,
  date_obs  = as.Date(inat_raw1$observed_on),
  source    = "inat"
)

# Check structure
head(data_inat1)
str(data_inat1)
unique(data_inat1$species)


# Merge GBIF and iNat data #
# Stack GBIF and iNat data
matrix_full1 <- bind_rows(data_gbif1, data_inat1)

# Check results
head(matrix_full1)
table(matrix_full1$source, useNA = "ifany")
summary(matrix_full1$date_obs)

#filter by date
matrix_full_date1 <- matrix_full1 %>%
  filter(!is.na(date_obs)) %>%
  filter(date_obs >= date_start & date_obs <= date_end)

# Check results
head(matrix_full_date1)
summary(matrix_full_date1$date_obs)
table(matrix_full_date1$source)
View(matrix_full_date1)

#keep only species (not subspecies)
matrix_full_date1 <- matrix_full_date1 %>%
  mutate(species = sub("^([A-Za-z]+\\s+[A-Za-z]+).*", "\\1", species))
View(matrix_full_date1)

# Map of combined data #
x11()
ggplot(data = Switzerland) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = matrix_full_date1,
    aes(x = longitude, y = latitude, fill = source),
    size = 3,
    shape = 21,
    color = "black",
    alpha = 0.8
  ) +
  labs(title = "Sala GBIF + iNat")
  theme_classic()


#######################################################
#######################################################
###### Triton alpestre - Ichthyosaura alpestris ######
#######################################################
#######################################################

# Download GBIF data with coordinates #
gbif_raw2 <- occ_data(
  scientificName = mysp2,
  hasCoordinate = TRUE,
  country = "CH",
  limit = 1000
)

# Extract the main data table
gbif_occ2 <- gbif_raw2$data

# Quick inspection
head(gbif_occ2)
names(gbif_occ2)
nrow(gbif_occ2)

# Map showing GBIF occurrences only
X11()
ggplot(data = Switzerland) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = gbif_occ2,
    aes(x = decimalLongitude, y = decimalLatitude),
    size = 3,
    shape = 21,
    fill = "darkgreen",
    color = "black"
  ) +
  labs(title = "Triton GBIF")
  theme_classic()

# Format GBIF data
data_gbif2 <- data.frame(
  species   = gbif_occ2$species,
  latitude  = gbif_occ2$decimalLatitude,
  longitude = gbif_occ2$decimalLongitude,
  date_obs  = as.Date(gbif_occ2$eventDate),
  source    = "gbif"
)

# Check structure
head(data_gbif2)
str(data_gbif2)
unique(data_gbif2$species)


# Download iNat data #
inat_raw2 <- get_inat_obs(
  taxon_name = mysp2,
  place_id = "switzerland",
  maxresults = 1000
)

# Inspect the structure
head(inat_raw2)
names(inat_raw2)
nrow(inat_raw2)

# Map showing iNaturalist occurrences only
X11()
ggplot(data = Switzerland) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = inat_raw2,
    aes(x = longitude, y = latitude),
    size = 3,
    shape = 21,
    fill = "darkred",
    color = "black"
  ) +
  labs(title = "Triton iNat")
  theme_classic()

# Format iNat data 
data_inat2 <- data.frame(
  species   = inat_raw2$scientific_name,
  latitude  = inat_raw2$latitude,
  longitude = inat_raw2$longitude,
  date_obs  = as.Date(inat_raw2$observed_on),
  source    = "inat"
)

# Check structure
head(data_inat2)
str(data_inat2)
unique(data_inat2$species)


# Merge GBIF and iNat data #
# Stack GBIF and iNat data
matrix_full2 <- bind_rows(data_gbif2, data_inat2)

# Check results
head(matrix_full2)
table(matrix_full2$source, useNA = "ifany")
summary(matrix_full2$date_obs)

#filter by date
matrix_full_date2 <- matrix_full2 %>%
  filter(!is.na(date_obs)) %>%
  filter(date_obs >= date_start & date_obs <= date_end)

# Check results
head(matrix_full_date2)
summary(matrix_full_date2$date_obs)
table(matrix_full_date2$source)
View(matrix_full_date2)

#keep only species (not subspecies)
matrix_full_date2 <- matrix_full_date2 %>%
  mutate(species = sub("^([A-Za-z]+\\s+[A-Za-z]+).*", "\\1", species))

matrix_full_date2 <- matrix_full_date2 %>%
  mutate(species = ifelse(
    species == "Mesotriton alpestris",
    "Ichthyosaura alpestris",
    species
  ))

View(matrix_full_date2)


# Map of combined data #
x11()
ggplot(data = Switzerland) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = matrix_full_date2,
    aes(x = longitude, y = latitude, fill = source),
    size = 3,
    shape = 21,
    color = "black",
    alpha = 0.8
  ) +
  labs(title = "Triton GBIF + iNat")
  theme_classic()


#######################################################
#######################################################
## Couleuvre à collier helvétique - Natrix helvetica ##
#######################################################
#######################################################

# Download GBIF data with coordinates #
gbif_raw3 <- occ_data(
  scientificName = mysp3,
  hasCoordinate = TRUE,
  country = "CH",
  limit = 1000
)

# Extract the main data table
gbif_occ3 <- gbif_raw3$data

# Quick inspection
head(gbif_occ3)
names(gbif_occ3)
nrow(gbif_occ3)

# Map showing GBIF occurrences only
X11()
ggplot(data = Switzerland) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = gbif_occ3,
    aes(x = decimalLongitude, y = decimalLatitude),
    size = 3,
    shape = 21,
    fill = "darkgreen",
    color = "black"
  ) +
  labs(title = "Couleuvre GBIF")
  theme_classic()

# Format GBIF data
data_gbif3 <- data.frame(
  species   = gbif_occ3$species,
  latitude  = gbif_occ3$decimalLatitude,
  longitude = gbif_occ3$decimalLongitude,
  date_obs  = as.Date(gbif_occ3$eventDate),
  source    = "gbif"
)

# Check structure
head(data_gbif3)
str(data_gbif3)
unique(data_gbif3$species)


# Download iNat data #
inat_raw3 <- get_inat_obs(
  taxon_name = mysp3,
  place_id = "switzerland",
  maxresults = 1000
)

# Inspect the structure
head(inat_raw3)
names(inat_raw3)
nrow(inat_raw3)

# Map showing iNaturalist occurrences only
X11()
ggplot(data = Switzerland) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = inat_raw3,
    aes(x = longitude, y = latitude),
    size = 3,
    shape = 21,
    fill = "darkred",
    color = "black"
  ) +
  labs(title = "Couleuvre iNat")
  theme_classic()

# Format iNat data 
data_inat3 <- data.frame(
  species   = inat_raw3$scientific_name,
  latitude  = inat_raw3$latitude,
  longitude = inat_raw3$longitude,
  date_obs  = as.Date(inat_raw3$observed_on),
  source    = "inat"
)

# Check structure
head(data_inat3)
str(data_inat3)
unique(data_inat3$species)


# Merge GBIF and iNat data #
# Stack GBIF and iNat data
matrix_full3 <- bind_rows(data_gbif3, data_inat3)

# Check results
head(matrix_full3)
table(matrix_full3$source, useNA = "ifany")
summary(matrix_full3$date_obs)

#filter by date
matrix_full_date3 <- matrix_full3 %>%
  filter(!is.na(date_obs)) %>%
  filter(date_obs >= date_start & date_obs <= date_end)

# Check results
head(matrix_full_date3)
summary(matrix_full_date3$date_obs)
table(matrix_full_date3$source)
View(matrix_full_date3)

#keep only species (not subspecies)
matrix_full_date3 <- matrix_full_date3 %>%
  mutate(species = sub("^([A-Za-z]+\\s+[A-Za-z]+).*", "\\1", species))
View(matrix_full_date3)


# Map of combined data #
x11()
ggplot(data = Switzerland) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = matrix_full_date3,
    aes(x = longitude, y = latitude, fill = source),
    size = 3,
    shape = 21,
    color = "black",
    alpha = 0.8
  ) +
  labs(title = "Couleuvre GBIF + iNat")
  theme_classic()


#######################################################
#######################################################
############## All species together ##################
#######################################################
#######################################################

# Create a matrix with the 3 species #
matrix_allsp <- bind_rows(matrix_full_date1, matrix_full_date2, matrix_full_date3)

# Check data
head(matrix_allsp)
nrow(matrix_allsp)
unique(matrix_allsp$species)
View(matrix_allsp)

# ============================================================================ #
# Problem Set 1                                                                #
# Big Data and Machine Learning - 202420                                       #
#                                                                              #
# Script: Location Variables                                                   #
#                                                                              #
# Team 1: - Sergio Sandoval                                                    #
#         - María Fernanda Blanco                                              #
#         - Juan Fernando Barberi                                              #
#         - Juan Gutierrez                                                     #
# ============================================================================ #


# ENVIRONMENT SETUP AND DATA UPLOADING =========================================

# Set working directory
setwd("/Users/setosandoval/Documents/Problem Set 3")

# Clean the environment
rm(list = ls())

# Seed
set.seed(202028276)

# Libraries
library(pacman)
p_load(tidyverse,     
       osmdata,       
       sf,            
       ggplot2,
       leaflet)

# Data 
train <-read.csv("stores/data/raw/train_test/train.csv")
test <-read.csv("stores/data/raw/train_test/test.csv")

# Combined data
combined <- rbind(train, test) 

# Only location variables
combined <- combined %>%
  select(property_id, lat, lon)

# Data as sf object
data_sf <- st_as_sf(combined, coords = c("lon", "lat"), crs = 4326) 



# UPZ ==========================================================================

# Variables: UPZ and Localidad by ID

# UPZ data
UPZ <- st_read("stores/data/raw/external/UPZ") 
UPZ <- st_transform(UPZ, crs = st_crs(data_sf))

# Rename variables
UPZ <- UPZ %>%
  rename(id_UPZ = codigo_upz, 
         id_local = codigo_loca)

# Assign localidad ans UPZ to each property
nearest_UPZ <- st_nearest_feature(data_sf, UPZ) 
data_sf <- cbind(data_sf, UPZ[nearest_UPZ, c("id_UPZ", "id_local")])
data_sf <- data_sf[, !names(data_sf) %in% "geometry.1"]



# BLOCKS (MANZANAS) INFO =======================================================

# Variables: Density, number of properties for living and number of persons per block

# Manzanas data 
manz <- st_read("stores/data/raw/external/M_info") %>%
  select(MPIO_CDPMP, DENSIDAD, TVIVIENDA, TP27_PERSO) %>%  
  filter(MPIO_CDPMP == "11001") %>% # Bogotá
  st_transform(manz_est, crs = st_crs(data_sf))

# Rename variables
manz <- manz %>%
  rename(man_dens = DENSIDAD, 
         man_houses = TVIVIENDA,
         man_per = TP27_PERSO)

# Assign block (manzana) and its estrato to each property
nearest_M <- st_nearest_feature(data_sf, manz) 
data_sf <- cbind(data_sf, manz[nearest_M, c("man_dens", "man_houses", "man_per")]) 
data_sf <- data_sf[, !names(data_sf) %in% "geometry.1"]



# ESTRATO BY BLOCKS (MANZANAS) =================================================

# Variables: Estrato and dummy indicating if info is missing

# Manzanas (estrato) data 
manz_est <- st_read("stores/data/raw/external/M_estratificacion") 
manz_est <- st_transform(manz_est, crs = st_crs(data_sf))
manz_est <- st_make_valid(manz_est)

# Impute blocks (manzanas) with estrato 0 using the nearest non-zero value
manz_est$ESTRATO_corr <- manz_est$ESTRATO
manzanas_zero <- manz_est[manz_est$ESTRATO == 0, ]
manzanas_non_zero <- manz_est[manz_est$ESTRATO != 0, ]
nearest_M_z <- st_nearest_feature(manzanas_zero, manzanas_non_zero)
manz_est$ESTRATO_corr[manz_est$ESTRATO == 0] <- manzanas_non_zero$ESTRATO[nearest_M_z]

# Variable indicating no estrato info
manz_est$info_estrato <- ifelse(manz_est$ESTRATO != 0, 1, 0)

# Rename variables
manz_est <- manz_est %>%
  rename(id_manz = CODIGO_MAN, 
         estrato = ESTRATO_corr)

# Assign block (manzana) and its estrato to each property
nearest_M_est <- st_nearest_feature(data_sf, manz_est) 
data_sf <- cbind(data_sf, manz_est[nearest_M_est, c("id_manz", "estrato", "info_estrato")]) 
data_sf <- data_sf[, !names(data_sf) %in% "geometry.1"]



# PROPERTY VALUATION FROM CATSTRO BY BLOCKS (MANZANAS) =========================

# Variables: Catastro and comervial valuation of properties per block

# Manzanas (property valuation) data
manz_val <- st_read("stores/data/raw/external/M_valuation") 
manz_val <- st_transform(manz_val, crs = st_crs(data_sf))
manz_val <- st_make_valid(manz_val)

# Rename variables
manz_val <- manz_val %>%
  rename(val_com = AVALUO_COM, 
         val_cat = AVALUO_CAT)

# Assign catastro and comercial valuation to each property by block (manzana)
nearest_M_val <- st_nearest_feature(data_sf, manz_val) 
data_sf <- cbind(data_sf, manz_val[nearest_M_val, c("val_cat", "val_com")]) 
data_sf <- data_sf[, !names(data_sf) %in% "geometry.1"]



# AMENITIES OPEN STREET MAP ====================================================

# Variables: Distance to amenities:
#            1. Park
#            2. School
#            3. Highway
#            4. Supermarket
#            5. Hospital
#            6. Public Transport Station (Transmilenio)
#            7. Restaurant
#            8. Cicleway
#            9. Police Station
#            10. University
#            11. Bank
#            12. Mall

# Function to calculate distances to an OSM amenity 
calculate_amenity_distance <- function(data_sf, bbox, amenity_key, amenity_value, variable_name, amenity_type) {
  # Query OSM for the specified amenity
  amenity_data <- opq(bbox = bbox) %>%
    add_osm_feature(key = amenity_key, value = amenity_value) %>%
    osmdata_sf()
  
  # Select amenities based on type
  if (amenity_type == "point") {
    amenity_layer <- amenity_data$osm_points
  } else if (amenity_type == "polygon") {
    amenity_layer <- amenity_data$osm_polygons
  } else if (amenity_type == "line") {
    amenity_layer <- amenity_data$osm_lines
  } else {
    stop("Invalid amenity_type. Use 'point', 'polygon', or 'line'.")
  }
  
  # Ensure CRS alignment
  data_sf <- st_transform(data_sf, crs = st_crs(amenity_layer))
  amenity_layer <- st_transform(amenity_layer, crs = st_crs(data_sf))
  
  # Calculate distances
  distances <- st_distance(data_sf, amenity_layer, by_element = FALSE)
  min_distances <- apply(distances, 1, min)  # Minimum distance for each property
  
  # Add the calculated distances as a new variable
  data_sf <- cbind(data_sf, setNames(data.frame(as.numeric(min_distances)), variable_name))
  
  return(data_sf)
}

# Bounding box for Bogotá
bbox <- st_bbox(UPZ)

# Distance to parks (polygons)
data_sf <- calculate_amenity_distance(data_sf, bbox, "leisure", "park", "dist_park", amenity_type = "polygon")

# Distance to schools (points)
data_sf <- calculate_amenity_distance(data_sf, bbox, "amenity", "school", "dist_school", amenity_type = "point")

# Distance to primary roads (lines)
data_sf <- calculate_amenity_distance(data_sf, bbox, "highway", c("primary", "secondary"), "dist_highway", amenity_type = "line")

# Distance to supermarkets (points)
data_sf <- calculate_amenity_distance(data_sf, bbox, "shop", "supermarket", "dist_market", amenity_type = "point")

# Distance to hospitals (points)
data_sf <- calculate_amenity_distance(data_sf, bbox, "amenity", c("hospital", "clinic"), "dist_hospital", amenity_type = "point")

# Distance to public transport stations (points)
data_sf <- calculate_amenity_distance(data_sf, bbox, "public_transport", "station", "dist_station", amenity_type = "point")

# Distance to restaurants (points)
data_sf <- calculate_amenity_distance(data_sf, bbox, "amenity", "restaurant", "dist_rest", amenity_type = "point")

# Distance to cycleways (lines)
data_sf <- calculate_amenity_distance(data_sf, bbox, "highway", "cycleway", "dist_cycle", amenity_type = "line")

# Distance to police stations (points)
data_sf <- calculate_amenity_distance(data_sf, bbox, "amenity", "police", "dist_police", amenity_type = "point")

# Distance to univerisities (points)
data_sf <- calculate_amenity_distance(data_sf, bbox, "amenity", "university", "dist_uni", amenity_type = "point")

# Distance to banks (points)
data_sf <- calculate_amenity_distance(data_sf, bbox, "amenity", "bank", "dist_bank", amenity_type = "point")

# Distance to malls (points)
data_sf <- calculate_amenity_distance(data_sf, bbox, "shop", "mall", "dist_mall", amenity_type = "point")



# SAVE DATA ====================================================================

# Not sf object
data_final <- st_drop_geometry(data_sf)

# Export data
write.csv(data_final, "stores/data/raw/train_test/data_location.csv", row.names = F)

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


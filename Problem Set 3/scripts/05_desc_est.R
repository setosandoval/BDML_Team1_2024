# ============================================================================ #
# Problem Set 1                                                                #
# Big Data and Machine Learning - 202420                                       #
#                                                                              #
# Script: Descriptive Statistics                                               #
#                                                                              #
# Team 1: - Sergio Sandoval                                                    #
#         - María Fernanda Blanco                                              #
#         - Juan Fernando Barberi                                              #
#         - Juan Gutierrez                                                     #
# ============================================================================ #


# ENVIRONMENT SETUP AND DATA UPLOADING =========================================

# Set working directory
setwd("/Users/sergiosandovalcamargo/Desktop/Problem Set 3")

# Clean the environment
rm(list = ls())

# Seed
set.seed(202028276)

# Libraries
library(pacman)
p_load(tidyverse,
       sf,
       ggplot2,
       cowplot,
       tibble,
       osmdata,
       stargazer)  

# Data
data <-read.csv("stores/data/work/data_med.csv")

# Split into real train and test data
train <- data[data$is_test == 0, ] 
test <- data[data$is_test == 1, ] 

# Sf object
data_sf <- st_as_sf(data, coords = c("lon", "lat"), crs = 4326)

# UPZ 
UPZ <- st_read("stores/data/raw/external/UPZ") 
UPZ <- st_transform(UPZ, crs = st_crs(data_sf))

# No chapinero
UPZ_no_ch <- UPZ %>%
  filter(!codigo_upz %in% c("88", "89", "90", "97", "99"))

# Assign localidad UPZ to each property
nearest_UPZ <- st_nearest_feature(data_sf, UPZ_no_ch) 
data_sf <- cbind(data_sf, UPZ_no_ch[nearest_UPZ, c("codigo_upz")])
data_sf <- data_sf[, !names(data_sf) %in% "geometry.1"]

# Price in millions
data$price_millions <- data$price / 1e6



# TABLES =======================================================================

# 1) Descriptive Stattistics

# Rename variables with better English names
data_renamed <- data %>%
  rename(
    Precio_millions = price_millions,
    Bedrooms = bedrooms,
    Rooms = rooms,
    Bathrooms = bathrooms,
    Floors = floors,
    Parkings = parkings,
    Area = area,
    Estrato = estrato,
    Density_per_Block = man_dens,
    Houses_per_Block = man_houses,
    Persons_per_Block = man_per,
    Cadastre_Value = val_cat,
    Commercial_Value = val_com
  )

# Select relevant variables for the table
data_selected <- data_renamed %>%
  select(
    Precio_millions,
    Bedrooms,
    Rooms,
    Bathrooms,
    Floors,
    Parkings,
    Area,
    Estrato,
    Density_per_Block,
    Houses_per_Block,
    Persons_per_Block,
    Cadastre_Value,
    Commercial_Value
  )

# Create a descriptive statistics table and export to LaTeX
stargazer(
  data_selected,
  type = "latex",
  title = "Descriptive Statistics of Selected Variables",
  summary = TRUE,               
  median = TRUE,                
  digits = 2,                   
  out = "views/table1.tex" 
)





# FIGURES ======================================================================

# 1) Price: Histogram (train data) and Time Series by Month

# Histogram

# Transform the variable price to millions of COP
train$price_millions <- train$price / 1e6


# Distribution of Property Prices
graph_1 <- ggplot(train, aes(x = price_millions)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "blue", alpha = 0.7) +
  labs(
    title = "Distribution of Property Prices",
    x = "Price (Millions COP)",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 25))

# Time Series

# Create a summary dataset with median and mean prices for each year_month
price_summary <- data %>%
  group_by(year_month) %>%
  summarize(
    median_price = median(price, na.rm = TRUE)/1e6,
    mean_price = mean(price, na.rm = TRUE)/1e6
  )

# Convert year_month to a date format for better plotting
price_summary <- price_summary %>%
  mutate(year_month = as.Date(paste0(year_month, "_01"), format = "%Y_%m_%d"))

# Time Series
graph_2 <- ggplot(price_summary, aes(x = year_month)) +
  geom_line(aes(y = median_price, color = "Median"), size = 0.8) +
  geom_line(aes(y = mean_price, color = "Mean"), size = 0.8) +
  labs(
    title = "Median and Mean Prices Over Time",
    x = "Year-Month",
    y = "Price (Millions COP)",
    color = ""
  ) +
  scale_color_manual(values = c("Median" = "blue", "Mean" = "darkred")) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") +  
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 25),
    legend.position = "right",
    axis.text.x = element_text(angle = 60, hjust = 1))

# Combined figure
figure_1 <- plot_grid(graph_1, graph_2, ncol = 2, align = "h")
figure_1

# Save
ggsave("views/figure1.pdf", figure_1, width = 20, height = 6, dpi = 300)



# 2) Maps: Median Property Prices and Count of Amenities by UPZ 

# Median Property Prices

# Median
upz_medians <- data_sf %>%
  group_by(codigo_upz) %>%
  summarise(median_price = median(price, na.rm = TRUE)/1e6)
upz_medians <-st_drop_geometry(upz_medians)

# Joint
UPZ_with_prices <- UPZ %>%
  left_join(upz_medians, by = "codigo_upz")

# Median Property Prices by UPZ
map_1 <- ggplot() +
  geom_sf(data = UPZ_with_prices, aes(fill = median_price), color = "black", size = 0.2) +
  scale_fill_gradient(low = "white", high = "blue", name = "Median Price\n(Millions COP)") +
  labs(title = "Median Property Prices") +
  theme_void(base_size = 20) +
  theme(plot.title = element_text(hjust = 2, face = "bold", size = 25),
        legend.position = "right")

# Amenities

# Bounding Box
bogota_bbox <- st_bbox(UPZ) 
bogota_boundary <- opq(bbox = bogota_bbox) %>%
  add_osm_feature(key = "boundary", value = "administrative") %>%
  add_osm_feature(key = "admin_level", value = "10") %>%
  osmdata_sf()

bogota_boundary2 <- bogota_boundary$osm_polygons %>%
  st_transform(crs = st_crs(UPZ))

bogota_boundary2 <- st_join(bogota_boundary2$osm_polygons, UPZ)


# 1. Download Parks
parks <- opq(bbox = bogota_bbox) %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf() %>%
  .$osm_polygons %>%
  st_centroid() %>%
  st_transform(crs = st_crs(UPZ)) %>%
  mutate(type = "Park") %>%
  select(geometry, type)

# 2. Download Hospitals
hospitals <- opq(bbox = bogota_bbox) %>%
  add_osm_feature(key = "amenity", value = "hospital") %>%
  osmdata_sf() %>%
  .$osm_points %>%
  st_transform(crs = st_crs(UPZ)) %>%
  mutate(type = "Hospital") %>%
  select(geometry, type)

# 3. Download Schools
schools <- opq(bbox = bogota_bbox) %>%
  add_osm_feature(key = "amenity", value = "school") %>%
  osmdata_sf() %>%
  .$osm_points %>%
  st_transform(crs = st_crs(UPZ)) %>%
  mutate(type = "School") %>%
  select(geometry, type)

# 4. Download Markets
markets <- opq(bbox = bogota_bbox) %>%
  add_osm_feature(key = "shop", value = "supermarket") %>%
  osmdata_sf() %>%
  .$osm_points %>%
  st_transform(crs = st_crs(UPZ)) %>%
  mutate(type = "Supermarket") %>%
  select(geometry, type)

# 5. Download Restaurants
restaurants <- opq(bbox = bogota_bbox) %>%
  add_osm_feature(key = "amenity", value = "restaurant") %>%
  osmdata_sf() %>%
  .$osm_points %>%
  st_transform(crs = st_crs(UPZ)) %>%
  mutate(type = "Restaurant") %>%
  select(geometry, type)

# 6. Download Police Stations
police <- opq(bbox = bogota_bbox) %>%
  add_osm_feature(key = "amenity", value = "police") %>%
  osmdata_sf() %>%
  .$osm_points %>%
  st_transform(crs = st_crs(UPZ)) %>%
  mutate(type = "Police Station") %>%
  select(geometry, type)

# 7. Download Universities
universities <- opq(bbox = bogota_bbox) %>%
  add_osm_feature(key = "amenity", value = "university") %>%
  osmdata_sf() %>%
  .$osm_points %>%
  st_transform(crs = st_crs(UPZ)) %>%
  mutate(type = "University") %>%
  select(geometry, type)

# 8. Download Banks
banks <- opq(bbox = bogota_bbox) %>%
  add_osm_feature(key = "amenity", value = "bank") %>%
  osmdata_sf() %>%
  .$osm_points %>%
  st_transform(crs = st_crs(UPZ)) %>%
  mutate(type = "Bank") %>%
  select(geometry, type)

# 9. Download Malls
malls <- opq(bbox = bogota_bbox) %>%
  add_osm_feature(key = "shop", value = "mall") %>%
  osmdata_sf() %>%
  .$osm_points %>%
  st_transform(crs = st_crs(UPZ)) %>%
  mutate(type = "Mall") %>%
  select(geometry, type)

# 10. Download Cycleways
cycleways <- opq(bbox = bogota_bbox) %>%
  add_osm_feature(key = "highway", value = "cycleway") %>%
  osmdata_sf() %>%
  .$osm_lines %>%
  st_transform(crs = st_crs(UPZ)) %>%
  st_centroid() %>% # Convert lines to points
  mutate(type = "Cycleway") %>%
  select(geometry, type)

# 11. Download Bus/Train Stations
stations <- opq(bbox = bogota_bbox) %>%
  add_osm_feature(key = "public_transport", value = "station") %>%
  osmdata_sf() %>%
  .$osm_points %>%
  st_transform(crs = st_crs(UPZ)) %>%
  mutate(type = "Station") %>%
  select(geometry, type)

# 12. Download Highways (Primary and Secondary)
highways <- opq(bbox = bogota_bbox) %>%
  add_osm_feature(key = "highway", value = c("primary", "secondary")) %>%
  osmdata_sf() %>%
  .$osm_lines %>%
  st_transform(crs = st_crs(UPZ)) %>%
  st_centroid() %>% # Convert lines to points
  mutate(type = "Highway") %>%
  select(geometry, type)

# Combine all amenities
amenities_combined <- bind_rows(
  parks, hospitals, schools, markets, restaurants, police, universities,
  banks, malls, cycleways, stations, highways
)

# Heatmap
map_2 <- ggplot() +
  stat_density2d(
    data = amenities,
    aes(
      x = st_coordinates(geometry)[, 1], 
      y = st_coordinates(geometry)[, 2], 
      fill = ..level..
    ),
    geom = "polygon",
    alpha = 0.5
  ) +
  scale_fill_gradient(
    low = "white", 
    high = "blue", 
    name = "Density"
  ) +
  geom_sf(data = UPZ, fill = NA, color = "black", size = 0.1) + 
  labs(title = "Heatmap of Amenities") +
  theme_void(base_size = 20) +
  theme(
    plot.title = element_text(
      hjust = 1,         
      face = "bold", 
      size = 25,
      margin = margin(t = 15, b = 10)  
    ),
    legend.position = "right"
  )

# Combined figure
figure_2 <- plot_grid(map_1, map_2, ncol = 2, align = "h")

# Save
ggsave("views/figure2.pdf", figure_2, width = 12, height = 8, dpi = 300)



# 3) Boxplot Estrato Price

# Create a box plot
graph_3 <- ggplot(data, aes(x = as.factor(estrato), y = price_millions)) +
  geom_boxplot(fill = "lightblue", color = "blue", outlier.color = "darkred", outlier.shape = 16, outlier.size = 2) +
  labs(x = "Estrato",
    y = "Price (Millions COP)"
  ) +
  theme_minimal(base_size = 20) +
  theme(axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

# Save
ggsave("views/figure3.pdf", graph_3, width = 12, height = 8, dpi = 300)



# 4) Boxplot Ditance to Amenties

# Convert distance variables to long format with renamed amenities
distances_long <- data %>%
  select(starts_with("dist")) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Amenity",
    values_to = "Distance"
  ) %>%
  mutate(Amenity = recode(
    Amenity,
    dist_bank = "Distance to Bank",
    dist_cycle = "Distance to Cycle Lane",
    dist_highway = "Distance to Highway",
    dist_hospital = "Distance to Hospital",
    dist_mall = "Distance to Mall",
    dist_market = "Distance to Market",
    dist_park = "Distance to Park",
    dist_police = "Distance to Police Station",
    dist_rest = "Distance to Restaurant",
    dist_school = "Distance to School",
    dist_station = "Distance to Bus/Train Station",
    dist_uni = "Distance to University"
  ))

# Distribution of Distances to Amenities
graph_4 <- ggplot(distances_long, aes(x = Amenity, y = Distance)) +
  geom_boxplot(fill = "lightblue", color = "blue", outlier.color = "darkred") +
  labs( x = "Amenity",
    y = "Distance (meters)",) +
  theme_minimal(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save
ggsave("views/figure4.pdf", graph_4, width = 12, height = 8, dpi = 300)



# 5) Scatter plot Area 

# Area
graph_5 <- ggplot(data, aes(x = area, y = price_millions)) +
  geom_point(color = "darkred", alpha = 0.3) +
  geom_smooth(method = "lm", color = "blue", linetype = "dashed", size = 1, se = TRUE) + # Linear trend line
  labs(
    title = "Area",
    x = "Area (m²)",
    y = "Price (Millions COP)"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

graph_6 <- ggplot(data %>% filter(area < 500), aes(x = area, y = log(price_millions))) +
  geom_point(color = "darkred", alpha = 0.3) +
  geom_smooth(method = "lm", color = "blue", linetype = "dashed", size = 1, se = TRUE) + # Linear trend line
  labs(
    title = "Filtered for Area < 500 m²",
    x = "Area (m²)",
    y = "Log Price (Millions COP)"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

# Combined figure
figure_5 <- plot_grid(graph_5, graph_6, ncol = 2, align = "h")

# Save
ggsave("views/figure5.pdf", figure_5, width = 16, height = 4, dpi = 300)

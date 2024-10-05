# ========================================================================== #
# Problem Set 1                                                              #
# Big Data and Machine Learning - 202402                                     #
#                                                                            #
# Script: Scrape                                                             #
#                                                                            #
# Team 1: - Sergio Sandoval                                                  #
#         - María Fernanda Blanco                                            #
#         - Juan Fernando Barberi                                            #
#         - Juan Gutierrez                                                   #
# ========================================================================== #

# Set working directory
setwd("/Users/setosandoval/Desktop/BDML/Problem Sets/PS1/BDML_Team1_PS1")

# Clean environment
rm(list = ls())

# Load necessary libraries
library(pacman)
p_load(tidyverse,    # For tidy-data operations
       rvest)       # For web scraping

# Create an empty tibble to store all the tables
combined_data <- tibble()

# Loop through the 10 chunks of data
for (i in 1:10) {
  
  # Construct the URL for each page
  my_url <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", i, ".html")
  
  # Read the HTML content from the page
  my_html <- read_html(my_url)
  
  # Extract the table from the HTML content
  table <- my_html %>% 
    html_table(fill = TRUE)
  
  # Combine the table with the existing data
  data <- table[[1]]
  combined_data <- bind_rows(combined_data, data)
  
  # Print progress to console
  print(paste("Chunk", i, "loaded"))
}

# Save the combined dataframe as an RDS file
saveRDS(combined_data, "stores/data_PS1.rds")

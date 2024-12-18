# Problem Set 3 - Big Data and Machine Learning

This repository contains the solution for a project in the Big Data and Machine Learning course (2024-20). The project was developed by Team 1, composed of:
- Sergio Sandoval
- María Fernanda Blanco
- Juan Fernando Barberi
- Juan Eduardo Gutiérrez

## Repository Structure

The repository is organized as follows:

### `document`
Contains the final report in PDF format and any documentation or references related to the dataset and methodology used.

### `scripts`
This folder contains all the R scripts used in the project, including the main scripts for data preparation, modeling, and analysis:
1. `01_data_text.R`: Processes and cleans text-based variables, including feature extraction using natural language processing techniques.
2. `02_data_location.R`: Processes spatial data, including the calculation of distances to amenities and aggregation of location-based features.
3. `03_data_clean.R`: Combines and cleans the datasets from previous scripts to prepare the final dataset for analysis and modeling.
4. `04_models.R`: Implements machine learning models, including data partitioning, model training, hyperparameter tuning, and evaluation.
5. `05_desc_est.R`: Generates descriptive statistics and visualizations to summarize the dataset.

### `stores`
This folder contains data and intermediate outputs organized into subfolders:
- `raw`: Contains the original datasets and external data sources:
  - **Original data**: Raw CSV files for the project. Due to large file sizes, these need to be placed manually.
  - **External data**: Includes shapefiles and other external data sources used for spatial analysis, as well as a `stopwords.txt` file for text processing. These datasets are too large to store here but can be downloaded from open data portals as specified in the project documentation.
- `work`: Contains processed and intermediate datasets generated by the scripts.
- `submissions`: Stores the final model predictions in CSV format.

### `views`
This folder contains figures and visualizations generated during the analysis.

## Instructions to Run the Project

To reproduce the results and run the project, follow these steps:

1. **Prepare the data**:
   - Download the original raw datasets from the project source and place them in `stores/raw`.  
   - Download the required external datasets (e.g., shapefiles, external features) from open data portals as specified in the project documentation and place them in the same folder.

2. **Run the scripts in order**:
   - `01_data_text.R`: Processes text-based variables such as property descriptions and titles.
   - `02_data_location.R`: Extracts spatial features such as distances to amenities using OpenStreetMap data.
   - `03_data_clean.R`: Combines, cleans, and imputes missing data to generate a ready-to-use dataset.
   - `04_models.R`: Trains machine learning models and saves predictions.
   - `05_desc_est.R`: Generates descriptive statistics and visualizations.

3. **Notes**:
   - Ensure that R and Python has access to all required packages. Install missing packages if necessary.
   - Due to the size of the datasets, ensure sufficient memory and computational resources.
   - Verify that all file paths are correct to avoid errors during execution.

4. **Output**:
   - The cleaned datasets will be saved in `stores/work`.
   - Figures and visualizations will be saved in `views`.
   - Model predictions will be saved in `stores/submissions`.

---

This structure ensures clarity and reproducibility, allowing efficient navigation and execution of the project components.

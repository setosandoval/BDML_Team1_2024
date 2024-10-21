# Problem Set 2 - Big Data and Machine Learning

This folder contains the solution for Problem Set 2 in the Big Data and Machine Learning course (2024-20). The project was developed by Team 1, composed of:
- Sergio Sandoval
- María Fernanda Blanco
- Juan Fernando Barberi
- Juan Eduardo Gutiérrez

## Repository Structure

The repository is organized into the following folders:

- `document`: Contains the final report in PDF format and documentation related to the dataset.
- `scripts`: Contains all the R code used in the project. Inside, there is a folder named `solo` which includes the individual scripts and submissions from each team member. There are also three key scripts:
    1. `1_data.R`: Code for cleaning the raw data.
    2. `2_predictions.R`: Code for building prediction models.
    3. `3_desc_stat.R`: Code for generating descriptive statistics.
- `stores`: This folder contains two subfolders:
    - `data`: Stores the datasets for the project, divided into:
        - `raw`: Where the raw datasets should be placed manually.
        - `work`: Where the cleaned datasets are stored.
    - `submissions`: Contains the model predictions.
- `views`: Includes all the generated figures and visualizations from the analysis.

## Instructions to Run the Project

To run the project and reproduce the results, follow these steps:

1. **Download the data manually**: Due to the large file sizes, the raw datasets must be manually placed in the `stores/data/raw` folder. The datasets can be downloaded from the following link:  
   [Kaggle Competition Data](https://www.kaggle.com/competitions/uniandes-bdml-2024-20-ps-2/data)

2. **Run the scripts**: After placing the data in the correct location, the project can be executed by running the following scripts in order:
    1. `1_data.R`: Cleans and processes the raw data.
    2. `3_desc_stat.R`: Generates descriptive statistics.
    3. `2_predictions.R`: Runs the predictive models.

Ensure that the data is properly placed before running the scripts to avoid any errors.

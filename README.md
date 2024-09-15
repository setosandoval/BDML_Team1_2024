# Problem Set 1 Repository for Big Data and Machine Learning for Applied Economics

This repository is designed as a template for problem sets in Big Data and Machine Learning for Applied Economics. The repository follows an organized structure, which facilitates both document writing and code development.

## Repository Structure

The repository contains four main folders, each serving a specific purpose:

- `document/`: This folder contains the final output document of the problem set in `pdf` format, which includes the full solution. The document is written in LaTeX, with figures and tables ideally pulled directly from the `views` folder. 
  
- `scripts/`: This folder contains all the scripts used in the problem set. It includes:
  - `scrape_PS1.R`: The script for scraping the dataset from the web.
  - `code_PS1.R`: The main script that runs all the analyses and models described in the problem set.
  
- `stores/`: This folder stores all the datasets used for the problem set. 
  - `data_PS1.rds`: The main dataset obtained through web scraping and used for the analysis.
  
- `views/`: This folder contains all the figures and tables generated from the analysis. These elements are linked directly to the LaTeX document to ensure the figures are up-to-date with the analysis.

## How to Run the Project

1. Ensure you have all the necessary libraries installed. You can check the required libraries in the `code_PS1.R` script.
   
2. Run the `scrape_PS1.R` script to obtain the dataset. The script scrapes data from the provided source and stores it as `data_PS1.rds` in the `stores/` folder.

3. Once the data is obtained, run the `code_PS1.R` script to execute the various models and analyses discussed in the problem set. The script generates figures and tables, which are automatically saved in the `views/` folder.

4. Compile the LaTeX document in the `document/` folder to generate the final report. Ensure all figures and tables are correctly referenced and included in the document.

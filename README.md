# LGE Granularity in Radiology

Welcome to the `LGE_granularity_Radiology` GitHub repository. This repository hosts a collection of documents and scripts associated with the radiology submission RAD-24-086 + RAD-24-0806.R1 (Major revisions)

## Repository Contents

Below is a brief overview of the contents provided in this repository:

- `code_book.pdf`: This document explains all the variables used in our analysis. It serves as a comprehensive guide for understanding the dataset's structure and the significance of each variable.

- `code_book_tabular.xlsx`: Offers a tabular view of all variables, facilitating easier import and modification of the dataset. This format is designed to complement the `data_management.R` script by allowing for a streamlined data handling process.

- `data_management.R`: A script dedicated to the preprocessing and cleaning of the dataset. Key features include:
  - Recoding and categorization of various clinical and demographic variables.
  - Adjustments for cardiovascular risks, clinical assessments, and medical history.
  - Categorization of CMR findings, specifically LGE indicators and extents.
  - Recoding and categorization of numerous variables related to patients' clinical and diagnostic data.
  - Special focus on CMR_LGE related variables for ischemic and midwall findings.

- `main_script.Rmd`: An R Markdown document containing all the statistical analyses, including the creation of tables and graphics. This script is the core of our analysis workflow, providing detailed insights and visualizations of the LGE granularity study. Of note, it has been adapted in mai 2025 following major revisions by radiology. 

## Note on Data

Please note that no actual data are provided within this repository. The provided scripts and documentation are intended to outline and detail the analysis conducted as part of the RAD-24-086 submission. The focus is purely on showcasing the analytical methodologies and findings derived from the study.

## How to Use

1. **Familiarize Yourself with the Code Book**: Start by reviewing the `code_book.pdf` to understand the variables and their definitions within the dataset.

2. **Explore the Tabular Variable Guide**: Use the `code_book_tabular.xlsx` for an easy-to-navigate view of the dataset structure and to facilitate any required data manipulations.

3. **Data Management**: Run the `data_management.R` script to perform the necessary data cleaning and preprocessing steps before analysis.

4. **Conduct Analysis**: Execute the `main_script.Rmd` for a comprehensive statistical analysis and to generate relevant tables and figures.

## Contact

For any inquiries or further assistance, please open an issue in this GitHub repository, and we will get back to you promptly.

Thank you for visiting our repository

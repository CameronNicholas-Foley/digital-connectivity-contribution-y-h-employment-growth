# Contribution of Digital Connectivity Data in Local Authority Employment Growth Modelling.
Assessing the Predictive Contribution of Yorkshire and The Humber's Digital Connectivity Data (% SFBB Availability; % of Premises Unable to Receive 10Mbit/s) in Employment Growth Modelling.

Overview
-
Territorial-level employment growth is a highly complex outcome shaped by industry, geography, and human capital - each influenced by contemporary and historical dynamics. As digital connectivity increasingly affects these relationships, investigating its predictive value in employment growth predictive modelling is timely. Using Yorkshire and The Humber, a region with a strong manufacturing history, this project employs a combination of Principal Component Analysis (PCA), bootstrapped Random Forest regression, and Ordinary Least Squares (OLS) linear regression to analyse macroeconomic and structural indicators that separate true economic signals from regional noise.

Data Sources
-
- Department for Education Attainment by Local Authority Data (2015-2023)
- Ofcom Connected Nations (2015-2023)
- ONS Business Register and Employment Survey (BRES) (2015-2023)
- ONS Business Demography (2015-2023)

Research Questions
-
- RQ1: To what extent are digital and structural factors associated with employment growth in Yorkshire and the Humber?

- RQ2: How much predictive value does digital connectivity data add in employment growth models alongside broader regional indicators?

Machine Learning Pipeline
-
1. **Spatial Data Partitioning:** 70/30 Train/Test Split performed on the `local_authority` level to prevent geographical data leakage.
2. **Industrial Dimensionality Reduction:** PCA utilised for industry shares within Local Authorities to handle multicollinearity.
3. **Manually Bootstrapped Feature Importance:** Random Forest model bootstrapped over 500 iterations to evaluate feature importance stability and identify consistent predictive signal among the features.
4. **Parametric Validation:** OLS Linear Regression used post-Random Forest out-of-sample evaluation to validate the statistical significance of primary digital indicators.

Findings
-
- **The COVID-19 Shock:** Exploratory data analysis revealed local authorities have experienced temporal change across  structural characteristics, particularly after the COVID-19 outbreak.
- **Model Complexity vs Sample Size:** The small sample size (train N = 72, test N = 40) led to the Random Forest models heavily overfitting to localised noise.
-   **The Digital Hypothesis on Parametric and Non-Parametric Testing:** Both Random Forest feature importance and linear OLS p-values confirmed the selected digital connectivity variables (% SFBB Availability; % of Premises Unable to Receive 10Mbit/s) were not significant predictors of employment growth % change.
-   **The Actual Structural Drivers:** Both models did however successfully identify Level 3 Educational Attainment (%) and PC3 (a specific local industry composition) as significant driver of employment growth % change. These two variables achieved >98% positive importance across 500 bootstraps and were the only variables to achieve $p < 0.05$ significance in the linear model.

Code
-
- All data inputs are stored in the `raw_data/` directory (trimmed data sets to reduce file size are commented in `3_data_processing.R`).
- All R scripts are in the `scripts/` directory and organised to reflect the following workflow:
  - Library loading.
  -  Data ETL (with function and global object storage in seperate `2_functions_global_objects.R` script).
  -  Finding from exploratory data analysis.
  -  Feature engineering and Selection.
  -  Final models with evaluation metrics and summaries.
- Tables, figures, and trained model outputs are saved automatically in the `outputs/` directory once the code is executed.

Instructions to Run Code
-
  1. Clone the repository:
  ```bash
https://github.com/CameronNicholas-Foley/digital-connectivity-contribution-y-h-employment-growth.git
```
  2. Open RStudio, set the cloned repository as the repository URL **(File -> New Project -> Version Control -> Git)**.
  3. Run `1_libraries.R` and install the packages if required.
  4. Execute the remaining scripts in the numerical order as labelled.
  5. Outputs will be saved in the `outputs/` directory.

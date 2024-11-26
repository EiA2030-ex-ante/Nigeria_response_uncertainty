
# Nigeria Response Uncertainty Framework

This repository contains the **Integrated Spatial Modeling Framework** for analyzing fertilizer investment returns in smallholder maize production in Nigeria. The framework is designed to support strategic investment decisions by integrating spatial, agronomic, and economic data to estimate returns on fertilizer application while incorporating production uncertainties.

---

## Background

Sustainably increasing the low rates of inorganic fertilizer use by smallholder farmers is a critical policy objective in Sub-Saharan Africa. Farmers face challenges such as:
- Overly generalized fertilizer recommendations.
- Recommended application rates that are unprofitable.
- Risk associated with rainfed farming systems.

This spatially explicit modeling framework uses empirical yield response data distributed across time and space to:
1. Estimate agronomic and economic returns to fertilizer investments.
2. Incorporate stochastic (uncertain) production outcomes to assess the riskiness of returns.

The framework focuses on maize farming in Nigeria and provides actionable insights for policy intervention and investment programming to improve agricultural productivity.

---

## Repository Structure

### 1. `data/`
Contains data necessary for the analysis, including:
- **`raw/`**: Raw, unprocessed data.
- **`intermediate/`**: Intermediate datasets generated during data preprocessing.
- **`results/`**: Final results and outputs of the analysis.

### 2. `old_code/`
Deprecated or legacy scripts that may provide historical context or alternative methodologies.

### 3. Analysis Scripts
The main scripts are organized to handle data cleaning, modeling, and visualization:

- **`1_data.R`**: Prepares and cleans the primary datasets for analysis.
- **`2_data_weather.R`**: Processes weather-related data for integration into the analysis.
- **`3_price_prediction_bisrat.R`**: Develops models for predicting fertilizer and maize prices.
- **`4_response_bisrat.R`**: Analyzes yield response to fertilizer use across spatial and temporal dimensions.
- **`5_main_analysis_2.R`**: Conducts the main econometric and spatial analysis to estimate fertilizer returns and model uncertainties.
- **`6_graphs_bisrat.R`**: Generates visualizations and graphs to illustrate key findings.

### 4. Additional Files
- **`.gitignore`**: Specifies files and folders to ignore in Git version control.
- **`.here`**: Facilitates relative file path management in R.
- **`LICENSE`**: License file for the repository.
- **`README.md`**: Provides an overview of the repository and usage instructions (this file).

---

## Key Features

- **Spatial Modeling**: Integrates spatially explicit yield response data to identify profitable fertilizer application rates.
- **Risk Assessment**: Incorporates stochastic production outcomes to evaluate the risk of fertilizer investments in rainfed systems.
- **Economic Analysis**: Provides insights into agronomic and economic returns, aiding policymakers and investors in designing targeted interventions.

---


## License
This project is licensed under the Creative Commons Attribution 4.0 International License. See the LICENSE file for details.

## Acknowledgments
    - This work was made possible through the OneCGIAR Initiative on Excellence in Agronomy (INV-005431), as well as through the Guiding Acid Soil Management Investments in Africa (GAIA) project (Grant no: INV-029117), supported by the Bill & Melinda Gates Foundation (BMGF). 
    - We would like to thank all funders supporting research through contributions to the CGIAR Trust Fund: https://www.cgiar.org/funders/.
    - This project uses data from the LSMS-ISA Ethiopia and various rainfall datasets. Special thanks to the data providers and contributors.
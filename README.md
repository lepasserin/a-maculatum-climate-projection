# *A.maculatum* Climate Impact Analysis

## Project Overview

This report describes the methods and results of a data-scientific pipeline forecasting survivorship probabilities of a northern population of yellow spotted salamanders under alternate climate scenarios. The writing and figures presented here form part of a larger research paper on the environmental determinants of *A. maculatum* survival (Kaufman et al., 20XX). All code and data used to complete this sub-analysis are viewable at https://github.com/lepasserin/a-maculatum-climate-impact.

## Repository Structure

- `data`: all datasets (raw and cleaned) used in this report.
- `document`: QMD file containing the full writeup of the analysis and data visualizations. Also contains project references and a PDF of the writeup.
- `scripts`: cleaning and analytical R scripts used to carry out the analysis.

## Data Acquisition Declaration

All raw data used in this project was either provided directly by the BLISS (Bat Lake Inventory of Spotted Salamanders) research team, or sourced from [this page](https://climate-scenarios.canada.ca/?page=CanDCS6-data) at Environment and Climate Change Canada, using the following parameters:
- **Dataset**: CanDCS-M6
- **Variable**: Mean temp / Min temp / Total precip
- **Model**: Ensemble
- **Ensemble**: Ensemble mean
- **Scenario**: Historical + SSP1-26 / SSP2-45 / SSP5-85
- **Coordinates (NE)**: (`45.5475`, `-78.2464`)
- **Coordinates (SW)**: (`45.5191`, `-78.2869`)
- **Time of Year**: March / April / May
- **Start Year**: 2000
- **End Year**: 2100
- **Temporal Resolution**: Daily output

## Acknowledgements

This project was completed by Benedict Cummins-Mburu, with the help of Dylan Kaufman (MSc) and under the supervision of Dr. Njal Rollinson.

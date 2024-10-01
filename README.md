# RenewableEnergy-ClimateNexus

Code and data for an analysis of the renewable energy-climate nexus across four US states. The results from the analysis are currently under review. 

Three categories of data were collected: renewable energy generation data, climate data, and population data. The renewable energy generation data (folder: `EIAdata`) includes energy generation data for hydroelectric, solar, and wind power obtained from the US Energy Information Administration. The climate data (folder: `NARRdata`) were obtained from the North American Regional Reanalysis.The population data (folder: `CensusData`) were obtained from the US Census Bureau. All data were collected between 2023 and 2024.

The code was developed in R version 4.1.2 and last ran on 01 October 2024. The code is contained in the file `fourstates.R` and the associated Rdata files can be found in the `rdatafiles` folder. In order to run the code, the following R packages are required, with the version we used in parentheses: 

*  bartMachine (v1.3.2)
*  cowplot(v1.1.1)
*  dplyr (v1.0.10)
*  e1071 (v1.7-12)
*  earth (v5.3.1)
*  gam (v1.22)
*  ggplot2 (v3.4.0)
*  glmnet (v4.1-4)
*  lubridate (v1.9.0)
*  pdp (v0.8.1)
*  randomForest (v4.7-1.1)
*  readxl (v1.4.1)
*  reshape2 (v1.4.4)
*  rpart (v4.1.19)
*  scales (v1.2.1)
*  stringr (v1.4.1)
*  VSURF (v1.1.0)

To run the code, users need to update the path to the folder downloaded or cloned from this repository. This change can be made on line 43 of `fourstates.R`. Once this path is changed, the data directories will be assigned automatically after running lines 46-53 in `fourstates.R`. The code can then be run sequentially or users can choose to run different sections, provided users load the rdata files in the `rdatafiles` folder prior to running each section. 

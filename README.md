# RenewableEnergy-ClimateNexus

Code and data for an analysis of the renewable energy-climate nexus across four US states. The results from the analysis are currently under review. 

Three categories of data were collected: renewable energy generation data, climate data, and population data. The renewable energy generation data (folder: `EIAdata`) includes energy generation data for hydroelectric, solar, and wind power obtained from the US Energy Information Administration, as well as capacity data. The climate data (folders: `NARRdata` and `GCMdata`) were obtained from the North American Regional Reanalysis and CMIP6 generalized circulation models (GCMs), respectively.The population data (folder: `CensusData`) were obtained from the US Census Bureau. All data were collected between 2023 and 2024.

The code was developed in R version 4.4.2 and last ran on 19 November 2024. The code is contained in the file `fourstates.R` and the associated Rdata files can be found in the `rdatafiles` folder. In order to run the code, the following R packages are required, with the version we used in parentheses: 

*  bartMachine (v1.3.4.1)
*  cowplot (v1.1.3)
*  dplyr (v1.1.4)
*  e1071 (v1.7-16)
*  earth (v5.3.4)
*  gam (v1.22-5)
*  ggplot2 (v3.5.1)
*  glmnet (v4.1-8)
*  lubridate (v1.9.3)
*  pdp (v0.8.2)
*  randomForest (v4.7-1.2)
*  readxl (v1.4.3)
*  reshape2 (v1.4.4)
*  rpart (v4.1.23)
*  scales (v1.3.0)
*  stringr (v1.5.1)
*  VSURF (v1.2.0)

To run the code, users need to update the path to the folder downloaded or cloned from this repository. This change can be made on line 43 of `fourstates.R`. Once this path is changed, the data directories will be assigned automatically after running lines 47-59 in `fourstates.R`. The code can then be run sequentially or users can choose to run different sections, provided users load the rdata files in the `rdatafiles` folder prior to running each section. 

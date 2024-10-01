Climate data were obtained from the North American Regional Reanalysis (NARR). 

Variables included precipitation (mm), average air temperature (K), relative humidity (%), and wind speed (m/s).
Wind speed is broken down into u and v directions, to calculate wind speed, use the following equation: sqrt(u^2 + v^2).

Data were obtained for each day from January 2000 through December 2022 and aggregated to form minimum, average, and maximum monthly values within the `fourstates.R` code.

Data were collected for each contiguous US state and Washington DC. For state ID codes, see `CONUS_States_info.xslx` in this folder.

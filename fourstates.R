# Project: Renewable Energy Generation - Four States
# Author: Renee Obringer
# Last Ran: 19 November 2024

# ORGANIZATION: 
# This code is organized into sections, the start of each is denoted by multiple #
# The sections can be run independently by loading the rdata files at the beginning of each section
# Each section is described below
#
# LOAD DATA: load data
# DATA PRE-PROCESSING: pre-processing to integrate generation and weather data
# MODEL RUNS: conduct variable selection & run all models for all sources across the four states
# INTERPRETATION: analyze model performance
# ENSEMBLE MODEL PERFORMANCE: evaluate the performance of an ensemble of MARS, SVM, RF, and BART predictions
# PROJECTIONS: use the individual models and ensemble to make projections
# PERCENT CHANGE ANALYSIS: use the projections to calculate the change in renewable energy under climate change
# SENSITIVITY ANALYSES: investigate how the sensitive the model results are to various choices made in the analysis
# FIGURES AND TABLES: code for plotting figures and creating tables included in manuscript 

rm(list=ls())
options(scipen = 999)
options(java.parameters = "-Xmx4g")

# libraries
library(stringr)       # for reading in data 
library(readxl)        # for reading in data
library(reshape2)      # for data cleaning
library(lubridate)     # for data cleaning
library(dplyr)         # for data cleaning
library(VSURF)         # for variable selection
library(glmnet)        # algorithm library - GLM
library(gam)           # algorithm library - GAM
library(earth)         # algorithm library - MARS
library(rpart)         # algorithm library - CART
library(randomForest)  # algorithm library - Random Forest
library(bartMachine)   # algorithm library - BART
library(e1071)         # algorithm library - SVM
library(ggplot2)       # for plotting
library(scales)        # for plotting
library(cowplot)       # for plotting
library(pdp)           # for partial dependence plots

# set file path
# NOTE: set this path to the folder on your personal machine which contains the cloned repository
# for example: path <- '/Users/Obringer/Downloads/RenewableEnergy-ClimateNexus'

path <- '   '

# set directories
datadir1 <- paste(path,'/EIAdata', sep = '')                        # directory for energy generation data
datadir2 <- paste(path, '/NARRdata', sep = '')                      # directory for observed weather data
datadir3 <- paste(path, '/CensusData', sep = '')                    # directory for population data 
datadir4 <- paste(path, '/GCMdata', sep = '')                       # directory for future climate data
datadir5 <- paste(path, '/EIAdata/powerplantinfo_EIA860', sep = '') # directory for power plant capacity data
rdatadir <- paste(path, '/rdatafiles', sep = '')                    # directory for rdata files generated in the code below

# OPTIONAL: create an output directory
outputdir <- paste(path, '/outputdir', sep = '')                           # directory for any non-rdata output files (e.g., csv, pdf, etc.)
dir.create(outputdir)

################ LOAD DATA ###########################

# NARR data
setwd(datadir2)

narrdata <- list()
narrdatanames <- c()
filenames <- list.files(pattern="*.txt", full.names=F)
for (i in 1:length(filenames)) {
  narrdata[[i]] <- read.table(paste(datadir2, '/', filenames[i], sep = ""), header=T, fill = TRUE, stringsAsFactors = F)
  narrdatanames[i] <- str_remove(filenames[i],'.txt')
  names(narrdata[[i]])[c(1,2)] <- c('Year', 'Month')
}

# CMIP6 data
setwd(datadir4)

cmip6data <- list()
cmip6datanames <- c()
filenames <- list.files(pattern="*.txt", full.names=F)
for (i in 1:length(filenames)) {
  cmip6data[[i]] <- read.table(paste(datadir4, '/', filenames[i], sep = ""), header=T, fill = TRUE, stringsAsFactors = F)
  cmip6datanames[i] <- str_remove(filenames[i],'.txt')
  names(cmip6data[[i]])[c(1:3)] <- c('Year', 'Month', 'Day')
}

# Generation Data
setwd(datadir1)

gendata <- c()
for (i in 1:16) {
  sheetdata <- read_excel(paste(datadir1, "/monthlygeneration.xlsx", sep = ""), sheet = i)
  names(sheetdata) <- c('Year', 'Month', 'State', 'TypeOfProducer', 'EnergySource','Generation_MWh')
  gendata <- rbind(gendata, sheetdata)
}

# Population Data
setwd(datadir3)

pop2000_10 <- read.csv('st-est00int-01.csv')
pop2010_20 <- read.csv('nst-est2020.csv')
pop2020_23 <- read.csv('NST-EST2023-POP.csv')

setwd(rdatadir)
save(narrdata, cmip6data, gendata, pop2000_10, pop2010_20, pop2020_23, file = "rawdata.RData")


################ DATA PRE-PROCESSING #################

setwd(rdatadir)
load('rawdata.RData')

# rename energy source values of interest
gendata$EnergySource[gendata$EnergySource == 'Hydroelectric Conventional'] <- 'Hydropower'
gendata$EnergySource[gendata$EnergySource == 'Solar Thermal and Photovoltaic'] <- 'Solar'

# subset generation data by state
CAdata <- gendata[which(gendata$State == 'CA' & gendata$TypeOfProducer == 'Total Electric Power Industry' & (gendata$EnergySource == 'Hydropower' | gendata$EnergySource == 'Solar' | gendata$EnergySource == 'Wind')),]
NYdata <- gendata[which(gendata$State == 'NY' & gendata$TypeOfProducer == 'Total Electric Power Industry' & (gendata$EnergySource == 'Hydropower' | gendata$EnergySource == 'Solar' | gendata$EnergySource == 'Wind')),]
FLdata <- gendata[which(gendata$State == 'FL' & gendata$TypeOfProducer == 'Total Electric Power Industry' & (gendata$EnergySource == 'Hydropower' | gendata$EnergySource == 'Solar' | gendata$EnergySource == 'Wind')),]
GAdata <- gendata[which(gendata$State == 'GA' & gendata$TypeOfProducer == 'Total Electric Power Industry' & (gendata$EnergySource == 'Hydropower' | gendata$EnergySource == 'Solar' | gendata$EnergySource == 'Wind')),]

CAdata <- dcast(CAdata, Year + Month + State + TypeOfProducer ~ EnergySource)
NYdata <- dcast(NYdata, Year + Month + State + TypeOfProducer ~ EnergySource)
FLdata <- dcast(FLdata, Year + Month + State + TypeOfProducer ~ EnergySource)
GAdata <- dcast(GAdata, Year + Month + State + TypeOfProducer ~ EnergySource)

# remove unnecessary variables
CAdata <- CAdata[,-c(3,4)]; NYdata <- NYdata[,-c(3,4)]; FLdata <- FLdata[,-c(3,4)]; GAdata <- GAdata[,-c(3,4)]

# get state-wide population
CApop <- unlist(c(pop2000_10[which(pop2000_10$Geographic.Area == 'California'),3:12], pop2010_20[which(pop2010_20$Geographic.Area == 'California'),3:12], pop2020_23[which(pop2020_23$Geographic.Area == 'California'),3:4]))
NYpop <- unlist(c(pop2000_10[which(pop2000_10$Geographic.Area == 'New York'),3:12], pop2010_20[which(pop2010_20$Geographic.Area == 'New York'),3:12], pop2020_23[which(pop2020_23$Geographic.Area == 'New York'),3:4]))
FLpop <- unlist(c(pop2000_10[which(pop2000_10$Geographic.Area == 'Florida'),3:12], pop2010_20[which(pop2010_20$Geographic.Area == 'Florida'),3:12], pop2020_23[which(pop2020_23$Geographic.Area == 'Florida'),3:4]))
GApop <- unlist(c(pop2000_10[which(pop2000_10$Geographic.Area == 'Georgia'),3:12], pop2010_20[which(pop2010_20$Geographic.Area == 'Georgia'),3:12], pop2020_23[which(pop2020_23$Geographic.Area == 'Georgia'),3:4]))

# convert to dataframe
CApop <- data.frame('Year' = c(2001:2022), 'Population' = CApop); NYpop <- data.frame('Year' = c(2001:2022), 'Population' = NYpop)
FLpop <- data.frame('Year' = c(2001:2022), 'Population' = FLpop); GApop <- data.frame('Year' = c(2001:2022), 'Population' = GApop)

# convert to monthly values
CApop <- CApop[rep(seq_len(nrow(CApop)), each = 12),]; NYpop <- NYpop[rep(seq_len(nrow(NYpop)), each = 12),]
FLpop <- FLpop[rep(seq_len(nrow(FLpop)), each = 12),]; GApop <- GApop[rep(seq_len(nrow(GApop)), each = 12),]

# normalize generation values by population
CAdata$Hydropower <- CAdata$Hydropower/CApop$Population; CAdata$Solar <- CAdata$Solar/CApop$Population; CAdata$Wind <- CAdata$Wind/CApop$Population
NYdata$Hydropower <- NYdata$Hydropower/NYpop$Population; NYdata$Solar <- NYdata$Solar/NYpop$Population; NYdata$Wind <- NYdata$Wind/NYpop$Population
FLdata$Hydropower <- FLdata$Hydropower/FLpop$Population; FLdata$Solar <- FLdata$Solar/FLpop$Population
GAdata$Hydropower <- GAdata$Hydropower/GApop$Population; GAdata$Solar <- GAdata$Solar/GApop$Population

# store as a list of data frames
stateGENdata <- list(CAdata, NYdata, FLdata, GAdata)

# detrend generation data (Sailor and Munoz 1997) 
for (i in 1:4) {
  state <- stateGENdata[[i]]

  fullmean <- colMeans(state[,3:length(state)],na.rm=T) # get mean over entire period
  yearlymeans <- aggregate(x = state[,3:length(state)], by = list(state[,1]), function(x) mean(x, na.rm=TRUE)) # get yearly means
  yearlymeans <- yearlymeans[,2:length(yearlymeans)]

  nyear <- floor(nrow(state)/12)
  nvar <- length(yearlymeans)

  # get adjustment term for each year and each variable
  fadj <- matrix(numeric(0), nrow = nyear, ncol = nvar)
  for (y in 1:nyear) {
    for (v in 1:nvar) {
      fadj[y,v] <- yearlymeans[y,v]/fullmean[v]
    }
  }

  # convert from yearly to monthly matrix
  monthlyfadj <- fadj[rep(1:nrow(fadj), c(rep(12,nyear))), ]

  # remove trend
  for (j in 1:nvar) {
    state[,j+2] <- state[,j+2]/monthlyfadj[,j]
  }
  
  # replace data
  stateGENdata[[i]] <- state
}

# subset NARR data by state
CAweather <- cbind(narrdata[[1]][,c(1,2,3,which(names(narrdata[[1]]) == 'ID_06'))], narrdata[[2]][,which(names(narrdata[[2]]) == 'ID_06')], 
                   narrdata[[3]][,which(names(narrdata[[3]]) == 'ID_06')], narrdata[[4]][,which(names(narrdata[[4]]) == 'ID_06')], 
                   narrdata[[5]][,which(names(narrdata[[5]]) == 'ID_06')]); names(CAweather)[4:8] <- narrdatanames[1:5]

NYweather <- cbind(narrdata[[1]][,c(1,2,3,which(names(narrdata[[1]]) == 'ID_36'))], narrdata[[2]][,which(names(narrdata[[2]]) == 'ID_36')], 
                   narrdata[[3]][,which(names(narrdata[[3]]) == 'ID_36')], narrdata[[4]][,which(names(narrdata[[4]]) == 'ID_36')], 
                   narrdata[[5]][,which(names(narrdata[[5]]) == 'ID_36')]); names(NYweather)[4:8] <- narrdatanames[1:5]

FLweather <- cbind(narrdata[[1]][,c(1,2,3,which(names(narrdata[[1]]) == 'ID_12'))], narrdata[[2]][,which(names(narrdata[[2]]) == 'ID_12')], 
                   narrdata[[3]][,which(names(narrdata[[3]]) == 'ID_12')], narrdata[[4]][,which(names(narrdata[[4]]) == 'ID_12')], 
                   narrdata[[5]][,which(names(narrdata[[5]]) == 'ID_12')]); names(FLweather)[4:8] <- narrdatanames[1:5]

GAweather <- cbind(narrdata[[1]][,c(1,2,3,which(names(narrdata[[1]]) == 'ID_13'))], narrdata[[2]][,which(names(narrdata[[2]]) == 'ID_13')], 
                   narrdata[[3]][,which(names(narrdata[[3]]) == 'ID_13')], narrdata[[4]][,which(names(narrdata[[4]]) == 'ID_13')], 
                   narrdata[[5]][,which(names(narrdata[[5]]) == 'ID_13')]); names(GAweather)[4:8] <- narrdatanames[1:5]

stateNARRdata <- list(CAweather, NYweather, FLweather, GAweather)

# aggregate to monthly values
monthlyNARRdata <- list()

for (i in 1:4) {
  state <- stateNARRdata[[i]]
  
  # calculate wind speed
  state$windspeed <- sqrt(state$daily_uwind10m_2000_2022^2 + state$daily_vwind10m_2000_2022^2)
  
  # convert temperature units
  state$daily_tavg_2000_2022 <- state$daily_tavg_2000_2022 - 273.15
  
  # aggregate to monthly values
  monthlyvalues <- state %>% group_by(Year, Month) %>% 
    summarize(min_rh = min(daily_rh_2000_2022), max_rh = max(daily_rh_2000_2022), avg_rh = mean(daily_rh_2000_2022), 
              min_temp = min(daily_tavg_2000_2022), max_temp = max(daily_tavg_2000_2022), avg_temp = mean(daily_tavg_2000_2022), 
              min_ws = min(windspeed), max_ws = max(windspeed), avg_ws = mean(windspeed), total_precip = sum(daily_precip_2000_2022), 
              min_precip = min(daily_precip_2000_2022), max_precip = max(daily_precip_2000_2022), avg_precip = mean(daily_precip_2000_2022))
  
  # store
  monthlyNARRdata[[i]] <- monthlyvalues
}

allstatesrad <- list(CA = narrdata[[6]][which(narrdata[[6]]$Year >= 2000 & narrdata[[6]]$Year <= 2022),c(1,2,which(names(narrdata[[6]]) == 'ID_06'))], 
                     NY = narrdata[[6]][which(narrdata[[6]]$Year >= 2000 & narrdata[[6]]$Year <= 2022),c(1,2,which(names(narrdata[[6]]) == 'ID_36'))],
                     FL = narrdata[[6]][which(narrdata[[6]]$Year >= 2000 & narrdata[[6]]$Year <= 2022),c(1,2,which(names(narrdata[[6]]) == 'ID_12'))], 
                     GA = narrdata[[6]][which(narrdata[[6]]$Year >= 2000 & narrdata[[6]]$Year <= 2022),c(1,2,which(names(narrdata[[6]]) == 'ID_13'))])

updatedNARR <- list()
statenames <- c('CA', 'NY', 'FL', 'GA')

# loop through each state
for (i in 1:4) {
  names(allstatesrad[[i]])[3] <- 'avg_rad'
  updatedNARR[[statenames[i]]] <- merge(monthlyNARRdata[[i]], allstatesrad[[i]])
}

# combine generation with NARR data
alldata <- list()

for (i in 1:4) {
  alldata[[i]] <- merge(updatedNARR[[i]], stateGENdata[[i]], by = c('Year','Month'))
}

# subset CMIP6 data by state

# labels for tracking data
gcmnames <- c('gfdl-esm4', 'ipsl-cm6a-lr', 'mpi-esm1-2-hr', 'mri-esm2-0', 'ukesm1-0-ll')
scenaarionames <- c('ssp126', 'ssp585')
stateids <- c('ID_06', 'ID_36', 'ID_12', 'ID_13')
statenames <- c('CA', 'NY', 'FL', 'GA')

# initialize data
monthlyCMIP6data <- list()

# loop through states
for (s in 1:4) {
  # initialize data
  allgcmdata <- list()
  
  for (i in 1:5) {
    # extract GCM data
    gcmdata <- cmip6data[grepl(gcmnames[i], cmip6datanames, fixed = T)]
    gcmdatanames <- cmip6datanames[grepl(gcmnames[i], cmip6datanames, fixed = T)]
    
    # initialize data
    allscenariodata <- list()
    for (j in 1:2) {
      # extract scenario data
      scenariodata <- gcmdata[grepl(scenaarionames[j], gcmdatanames, fixed = T)]
      scenariodatanames <- gcmdatanames[grepl(scenaarionames[j], gcmdatanames, fixed = T)]
      

      # store climate data in each scenario
      allscenariodata[[scenaarionames[j]]] <- cbind(scenariodata[[1]][,c(1,2,3,which(names(scenariodata[[1]]) == stateids[s]))], scenariodata[[2]][,which(names(scenariodata[[2]]) == stateids[s])], 
                                              scenariodata[[3]][,which(names(scenariodata[[3]]) == stateids[s])], scenariodata[[4]][,which(names(scenariodata[[4]]) == stateids[s])],
                                              scenariodata[[5]][,which(names(scenariodata[[5]]) == stateids[s])])
      names(allscenariodata[[j]])[4:8] <- c('humidity','precip','radiation','windspeed','temp')
      
      # convert temperature units
      allscenariodata[[j]]$temp <- allscenariodata[[j]]$temp - 273.15
      
      # aggregate to monthly values
      allscenariodata[[j]] <- allscenariodata[[j]] %>% group_by(Year, Month) %>% 
        summarize(min_rh = min(humidity), max_rh = max(humidity), avg_rh = mean(humidity), 
                  min_temp = min(temp), max_temp = max(temp), avg_temp = mean(temp), 
                  min_ws = min(windspeed), max_ws = max(windspeed), avg_ws = mean(windspeed), total_precip = sum(precip), 
                  min_precip = min(precip), max_precip = max(precip), avg_precip = mean(precip), 
                  avg_rad = mean(radiation)) 
      
        
    }
    # store data in each model
    allgcmdata[[gcmnames[i]]] <- allscenariodata
  }
  
  # create GCM ensemble for each scenario
  for (k in 1:2) {
    allgcmdata[['ensemble']][[scenaarionames[k]]] <- bind_rows(allgcmdata[[1]][[k]], allgcmdata[[2]][[k]], allgcmdata[[3]][[k]], allgcmdata[[4]][[k]], allgcmdata[[5]][[k]]) %>%
                                                     group_by(Year, Month) %>%
                                                     summarize(min_rh = mean(min_rh), max_rh = mean(max_rh), avg_rh = mean(avg_rh), 
                                                               min_temp = mean(min_temp), max_temp = mean(max_temp), avg_temp = mean(avg_temp), 
                                                               min_ws = mean(min_ws), max_ws = mean(max_ws), avg_ws = mean(avg_ws), 
                                                               total_precip = mean(total_precip), min_precip = mean(min_precip), max_precip = mean(max_precip), avg_precip = mean(avg_precip),
                                                               avg_rad = mean(avg_rad))
      
  }
  
  # store data for each state
  monthlyCMIP6data[[statenames[s]]] <- allgcmdata
}

setwd(rdatadir)
save(alldata, stateGENdata, updatedNARR, monthlyCMIP6data, file = "cleaneddata.RData")

################ MODEL RUNS ##########################

setwd(rdatadir)
load('cleaneddata.RData')

# labels for seasons [summer, winter]
seasonindices <- list(c(4:9), c(10:12,1:3))
seasonnames <- c('summer', 'winter')

# initialize variables
allrsq <- list(); allrmse <- list(); allnrmse <- list(); allmodels <- list(); allyhat <- list(); allselecteddata <- list(); allvarimpvalues <- list(); allsvmpd <- list()

# loop through each state
for (s in 1:4) {
  state <- alldata[[s]]
  
  # initialize variables
  seasonrsq <- list(); seasonrmse <- list(); seasonnrmse <- list(); seasonmodels <- list(); seasonyhat <- list(); seasonselecteddata <- list(); seasonvarimpvalues <- list(); seasonsvmpd <- list()
  
  # loop through each season
  for (t in 1:2) {
    # extract data for each season
    season <- state[state$Month %in% seasonindices[[t]],]
    
    # initialize variables
    sourcersq <- list(); sourcermse <- list(); sourcenrmse <- list(); sourcemodels <- list(); sourceyhat <- list(); selecteddata <- list(); varimpvalues <- list(); svmpd <- list()
    
    # loop through each source
    for (g in 1:3) {
      source <- season[,c(g+16, 3:16)]
      
      # remove NA values
      source <- na.omit(source)
      
      # implement VSURF for variable selection
      vsurf_obj <- VSURF(x = source[,2:length(source)], y = source[,1])
      
      # store variables
      updatedsource <- source[,c(1, 1 + vsurf_obj$varselect.interp)]
      
      # set up cross validation with selected variables
      n <- nrow(updatedsource)
      set.seed(11)
      updatedsource <- updatedsource[sample(n),]
      
      # initialize yhat and test data lists
      glmyhat_all <- c(); marsyhat_all <- c(); cartyhat_all <- c(); rfyhat_all <- c()
      bartyhat_all <- c(); svmyhat_all <- c(); testdata_all <- c(); meanonlyyhat_all <- c()
      
      # run models (LOOCV due to small sample size)
      for (j in 1:n) {
        # split into training and test dataset
        testData <- updatedsource[j,]
        trainData <- updatedsource[-j,]
        
        # create formula
        myformula <- paste(names(trainData)[1], "~", paste(names(trainData[2:length(trainData)]), collapse="+"))
        myformula <- as.formula(myformula)
        meanonlyformula <- as.formula(paste(names(trainData)[1], "~ 1"))
        
        # run models
        glmmodel <- glm(formula = myformula, data = trainData); marsmodel <- earth(formula = myformula, data = trainData)
        cartmodel <- rpart(formula = myformula, data = trainData); rfmodel <- randomForest(formula = myformula, data = trainData)
        svmmodel <- svm(formula = myformula, data = trainData)
        meanonly <- lm(meanonlyformula, data = trainData) 
        
        # fit models
        glmyhat <- predict(glmmodel, newdata = testData); marsyhat <- predict(marsmodel, newdata = testData)
        cartyhat <- predict(cartmodel, newdata = testData); rfyhat <- predict(rfmodel, newdata = testData)
        svmyhat <- predict(svmmodel, newdata = testData)
        meanonlyyhat <- predict(meanonly, newdata = testData)
        
        # special case for BART
        if (length(trainData == 2)) {
          # if there is only 1 selected predictor
          bartmodel <- bartMachine(X = data.frame(avg_rad = trainData[,2]), y = trainData[,1], serialize = TRUE)
          bartyhat <- predict(bartmodel, new_data = data.frame(avg_rad = testData[,2]))
        } else {
          bartmodel <- bartMachine(X = trainData[,2:length(trainData)], y = trainData[,1], serialize = TRUE)
          bartyhat <- predict(bartmodel, new_data = testData[,2:length(testData)])
        }
        
        # store yhat 
        glmyhat_all <- append(glmyhat_all, glmyhat); marsyhat_all <- append(marsyhat_all, marsyhat)
        cartyhat_all <- append(cartyhat_all, cartyhat); rfyhat_all <- append(rfyhat_all, rfyhat)
        bartyhat_all <- append(bartyhat_all, bartyhat); svmyhat_all <- append(svmyhat_all, svmyhat)
        meanonlyyhat_all <- append(meanonlyyhat_all, meanonlyyhat)
        
        # store test data 
        testdata_all <- append(testdata_all, testData[,1])
      }
      
      # store source-specific data
      varname <- names(updatedsource)[1]
      
      sourcersq[[varname]] <- data.frame("glm" = cor(glmyhat_all, testdata_all)^2, "mars" = cor(marsyhat_all, testdata_all)^2, "cart" = cor(cartyhat_all, testdata_all)^2, "rf" = cor(rfyhat_all, testdata_all)^2, 
                                         "bart" = cor(bartyhat_all, testdata_all)^2, "svm" = cor(svmyhat_all, testdata_all)^2)
      sourcermse[[varname]] <- data.frame("meanonly" = sqrt(sum((meanonlyyhat_all-testdata_all)^2)/length(testdata_all)), "glm" = sqrt(sum((glmyhat_all-testdata_all)^2)/length(testdata_all)), 
                                          "mars" = sqrt(sum((marsyhat_all-testdata_all)^2)/length(testdata_all)), "cart" = sqrt(sum((cartyhat_all-testdata_all)^2)/length(testdata_all)), 
                                          "rf" = sqrt(sum((rfyhat_all-testdata_all)^2)/length(testdata_all)), "bart" = sqrt(sum((bartyhat_all-testdata_all)^2)/length(testdata_all)),
                                          "svm" = sqrt(sum((svmyhat_all-testdata_all)^2)/length(testdata_all)))
      sourcenrmse[[varname]] <- data.frame("meanonly" = sqrt(sum((meanonlyyhat_all-testdata_all)^2)/length(testdata_all))/(range(testdata_all)[2]-range(testdata_all)[1]), 
                                           "glm" = sqrt(sum((glmyhat_all-testdata_all)^2)/length(testdata_all))/(range(testdata_all)[2]-range(testdata_all)[1]), 
                                           "mars" = sqrt(sum((marsyhat_all-testdata_all)^2)/length(testdata_all))/(range(testdata_all)[2]-range(testdata_all)[1]), 
                                           "cart" = sqrt(sum((cartyhat_all-testdata_all)^2)/length(testdata_all))/(range(testdata_all)[2]-range(testdata_all)[1]), 
                                           "rf" = sqrt(sum((rfyhat_all-testdata_all)^2)/length(testdata_all))/(range(testdata_all)[2]-range(testdata_all)[1]),
                                           "bart" = sqrt(sum((bartyhat_all-testdata_all)^2)/length(testdata_all))/(range(testdata_all)[2]-range(testdata_all)[1]),
                                           "svm" = sqrt(sum((svmyhat_all-testdata_all)^2)/length(testdata_all))/(range(testdata_all)[2]-range(testdata_all)[1]))
      sourcemodels[[varname]] <- list(glmmodel, marsmodel, cartmodel, rfmodel, bartmodel, svmmodel, meanonly)
      sourceyhat[[varname]] <- data.frame("glm" = glmyhat_all, "mars" = marsyhat_all, "cart" = cartyhat_all, "rf" = rfyhat_all, "bart" = bartyhat_all, "svm" = svmyhat_all, "meanonly" = meanonlyyhat_all, "testData" = testdata_all)
      selecteddata[[varname]] <- updatedsource
      varimpvalues[[varname]] <- data.frame("name" = names(source)[1 + vsurf_obj$imp.mean.dec.ind], "value" = vsurf_obj$imp.mean.dec)
      
      pdvalues <- list()
      for (p in 2:length(updatedsource)) {
        pdvalues[[names(updatedsource)[p]]] <- partial(svmmodel, pred.var = names(updatedsource)[p])
      }
      
      svmpd[[varname]] <- pdvalues
      
      # account for some state without all three sources
      if (length(state) == 18 & g == 2) {
        break
      }
    }
    # store season-specific data
    seasonrsq[[seasonnames[t]]] <- sourcersq; seasonrmse[[seasonnames[t]]] <- sourcermse; seasonnrmse[[seasonnames[t]]] <- sourcenrmse; seasonmodels[[seasonnames[t]]] <- sourcemodels
    seasonyhat[[seasonnames[t]]] <- sourceyhat; seasonselecteddata[[seasonnames[t]]] <- selecteddata; seasonvarimpvalues[[seasonnames[t]]] <- varimpvalues; seasonsvmpd[[seasonnames[t]]] <- svmpd
  }
  
  # store state-specific data
  statenames <- c('CA', 'NY', 'FL', 'GA')
  
  allrsq[[statenames[s]]] <- seasonrsq
  allrmse[[statenames[s]]] <- seasonrmse
  allnrmse[[statenames[s]]] <- seasonnrmse
  allmodels[[statenames[s]]] <- seasonmodels
  allyhat[[statenames[s]]] <- seasonyhat
  allselecteddata[[statenames[s]]] <- seasonselecteddata
  allvarimpvalues[[statenames[s]]] <- seasonvarimpvalues
  allsvmpd[[statenames[s]]] <- seasonsvmpd
}

setwd(rdatadir)
save(alldata, allselecteddata, allvarimpvalues, allsvmpd, allrsq, allrmse, allnrmse, allmodels, allyhat, file = "modelrunresults.RData")


################ INTERPRETATION ######################
setwd(rdatadir)
load('modelrunresults.RData')

# Measures of Error

# California 
allnrmse[['CA']][['summer']][['Hydropower']]
allnrmse[['CA']][['summer']][['Solar']]
allnrmse[['CA']][['summer']][['Wind']] 

allnrmse[['CA']][['winter']][['Hydropower']] 
allnrmse[['CA']][['winter']][['Solar']] 
allnrmse[['CA']][['winter']][['Wind']] 

allrsq[['CA']][['summer']][['Hydropower']]
allrsq[['CA']][['summer']][['Solar']]
allrsq[['CA']][['summer']][['Wind']]

allrsq[['CA']][['winter']][['Hydropower']]
allrsq[['CA']][['winter']][['Solar']] 
allrsq[['CA']][['winter']][['Wind']]

# New York 
allnrmse[['NY']][['summer']][['Hydropower']]
allnrmse[['NY']][['summer']][['Solar']]
allnrmse[['NY']][['summer']][['Wind']] 

allnrmse[['NY']][['winter']][['Hydropower']]
allnrmse[['NY']][['winter']][['Solar']]
allnrmse[['NY']][['winter']][['Wind']]

allrsq[['NY']][['summer']][['Hydropower']]
allrsq[['NY']][['summer']][['Solar']]
allrsq[['NY']][['summer']][['Wind']]

allrsq[['NY']][['winter']][['Hydropower']]
allrsq[['NY']][['winter']][['Solar']]
allrsq[['NY']][['winter']][['Wind']]

# Florida
allnrmse[['FL']][['summer']][['Hydropower']]
allnrmse[['FL']][['summer']][['Solar']]

allnrmse[['FL']][['winter']][['Hydropower']]
allnrmse[['FL']][['winter']][['Solar']]

allrsq[['FL']][['summer']][['Hydropower']]
allrsq[['FL']][['summer']][['Solar']]

allrsq[['FL']][['winter']][['Hydropower']]
allrsq[['FL']][['winter']][['Solar']]

# Georgia
allnrmse[['GA']][['summer']][['Hydropower']]
allnrmse[['GA']][['summer']][['Solar']]

allnrmse[['GA']][['winter']][['Hydropower']] 
allnrmse[['GA']][['winter']][['Solar']]

allrsq[['GA']][['summer']][['Hydropower']] 
allrsq[['GA']][['summer']][['Solar']]

allrsq[['GA']][['winter']][['Hydropower']]
allrsq[['GA']][['winter']][['Solar']]

################ ENSEMBLE MODEL PERFORMANCE ######################

setwd(rdatadir)
load('modelrunresults.RData')

allensemblenrmse <- list(); allensemblemean <- list()

# loop through each state
for (i in 1:length(allyhat)) {
  state <- allyhat[[i]]
  
  seasonensemblemean <- list(); seasonensemblenrmse <- list()
  
  # loop through each season
  for (t in 1:2) {
    season <- state[[t]]
    
    ensemblenrmse <- c(); ensemblemean <- list()
    
    # loop through each source
    for (j in 1:length(season)) {
      source <- season[[j]]
      
      # unweighted mean
      ensembledata <- data.frame('rf' = source$rf, 'bart' = source$bart, 'svm' = source$svm)
      ensemblemean[[j]] <- rowMeans(ensembledata)
      
      # weighted mean
      # ensembledata <- data.frame('glm' = source$glm, 'bart' = source$bart, 'svm' = source$svm)
      #weights <- data.frame(models = c('glm', 'bart', 'svm'), weights = c(0.38, 0.15, 0.46))
      #ensemblemean[[j]] <- ensembledata %>%  rowwise() %>% mutate(wt_avg = weighted.mean(c(glm, bart, svm), weights$weights), .keep = "none")
      
      # calculate RMSE
      ensemblenrmse[j] <- sqrt(sum((ensemblemean[[j]]-source$testData)^2)/nrow(source))/(range(source$testData)[2]-range(source$testData)[1])
    }
    
    seasonensemblemean[[t]] <- ensemblemean
    seasonensemblenrmse[[t]] <- ensemblenrmse
  }

  allensemblenrmse[[i]] <- seasonensemblenrmse
  allensemblemean[[i]] <- seasonensemblemean
}

setwd(rdatadir)
save(allensemblenrmse, allensemblemean, file = "ensembleresults.RData")

################ PROJECTIONS ######################

setwd(rdatadir)
load('cleaneddata.RData')      # includes GCM data
load('modelrunresults.RData')  # includes ML models

# labels
stateids <- c('CA', 'NY', 'FL', 'GA')
gcms <- c('gfdl-esm4', 'ipsl-cm6a-lr', 'mpi-esm1-2-hr', 'mri-esm2-0', 'ukesm1-0-ll', 'ensemble')
scenarios <- c('ssp126', 'ssp585')
seasonindices <- list(c(4:9), c(10:12,1:3)) # summer, winter
seasonnames <- c('summer', 'winter')
mlmodels <- c('glm', 'mars', 'cart', 'rf', 'bart', 'svm')

# initialize variables
projections <- list()

# loop through each state
for (s in 1:4) {
  
  # initialize variables
  gcmproj <- list()
  
  # loop through each climate model
  for (m in 1:6) {
    
    # initialize variables
    scenarioproj <- list()
    
    # loop through scenarios
    for (c in 1:2) {
      
      # get data from CMIP6 dataframe for each [[state]][[gcm]][[scenario]]
      futuredata <- monthlyCMIP6data[[s]][[m]][[c]]
      
      # initialize variables
      seasonproj <- list()
      
      # loop through each season in the ML model dataset
      for (t in 1:2) {
        
        # get season data
        seasondata <- futuredata[which(futuredata$Month %in% seasonindices[[t]]),]
        
        # initialize variables
        sourceproj <- list()
        
        # loop through each source
        for (g in 1:3) {
          
          # initialize variables
          n <- nrow(seasondata)
          mlproj <- data.frame(year = seasondata$Year, month = seasondata$Month, glm = double(n), mars = double(n), cart = double(n), rf = double(n), bart = double(n), svm = double(n), ensemble = double(n))
          
          # loop through each ML model except mean-only 
          for (i in 1:6) {
            
            # extract model
            model <- allmodels[[s]][[t]][[g]][[i]]
            
            # make projections 
            if (i == 5) {
              # different style of predict for BART 
              mlproj[,i+2] <- predict(model, seasondata[, which(names(seasondata) %in% model$training_data_features)])
            } else {
              mlproj[,i+2] <- predict(model, seasondata)
            }
            
          }
          
          # create ML model ensemble
          ensembledata <- data.frame('rf' = mlproj$rf, 'bart' = mlproj$bart, 'svm' = mlproj$svm)
          mlproj$ensemble <- rowMeans(ensembledata)
          
          # store projections by source
          sourceproj[[names(allmodels[[s]][[t]])[g]]] <- mlproj
          
          # account for some state without all three sources
          if (length(alldata[[s]]) == 18 & g == 2) {
            break
          }
        }
        
        # store projections by season
        seasonproj[[seasonnames[t]]] <- sourceproj
      }
      
      # store projections by scenario
      scenarioproj[[scenarios[c]]] <- seasonproj
    }
    
    # store projections by GCM
    gcmproj[[gcms[m]]] <- scenarioproj
  }
  
  # store projections by state
  projections[[stateids[s]]] <- gcmproj
}

setwd(rdatadir)
save(projections, file = "projectionsresults.RData")

################ PERCENT CHANGE ANALYSIS ######################

setwd(rdatadir)
load('projectionsresults.RData')

# labels
stateids <- c('CA', 'NY', 'FL', 'GA')
gcms <- c('gfdl-esm4', 'ipsl-cm6a-lr', 'mpi-esm1-2-hr', 'mri-esm2-0', 'ukesm1-0-ll', 'ensemble')
scenarios <- c('ssp126', 'ssp585')
seasonindices <- list(c(4:9), c(10:12,1:3)) # summer, winter
seasonnames <- c('summer', 'winter')

# initialize variables
percentchange <- list(); mwhchange <- list(); currdata <- list()

# loop through each state
for (s in 1:length(projections)) {
  
  # initialize variables
  gcmpc <- list(); gcmmwh <- list(); gcmcd <- list()
  
  # loop through each gcm 
  for (m in 1:length(projections[[s]])) {
    
    # initialize variables
    scenariopc <- list(); scenariomwh <- list(); scenariocd <- list()
    
    # loop through each scenario
    for (c in 1:length(projections[[s]][[m]])) {
      
      # initialize variables
      seasonpc <- list(); seasonmwh <- list(); seasoncd <- list()
      
      #loop through each season 
      for (t in 1:length(projections[[s]][[m]][[c]])) {
        
        # initialize variables
        sourcepc <- list(); sourcemwh <- list(); sourcecd <- list()
        
        # loop through each source
        for (g in 1:length(projections[[s]][[m]][[c]][[t]])) {
          
          # extract data
          sourcedata <- projections[[s]][[m]][[c]][[t]][[g]]
          
          # get current data (2001-2020)
          currentdata <- sourcedata[which(sourcedata$year >=2001 & sourcedata$year <= 2020),]
          
          # get future data (2041-2060)
          futuredata <- sourcedata[which(sourcedata$year >=2041 & sourcedata$year <= 2060),]
          
          # percent change (pos == % increase) [future-current/current]
          mlpc <- ((colMeans(futuredata[,3:9]) - colMeans(currentdata[,3:9]))/colMeans(currentdata[,3:9]))*100
          mlmwh <- (colMeans(futuredata[,3:9]) - colMeans(currentdata[,3:9]))
          mlcd <- colMeans(currentdata[,3:9])
          
          # store data for each source
          sourcepc[[names(projections[[s]][[m]][[c]][[t]])[g]]] <- mlpc
          sourcemwh[[names(projections[[s]][[m]][[c]][[t]])[g]]] <- mlmwh
          sourcecd[[names(projections[[s]][[m]][[c]][[t]])[g]]] <- mlcd
        }
        
        # store data for each season
        seasonpc[[seasonnames[t]]] <- sourcepc
        seasonmwh[[seasonnames[t]]] <- sourcemwh
        seasoncd[[seasonnames[t]]] <- sourcecd
      }
      
      # store data for each scenario
      scenariopc[[scenarios[c]]] <- seasonpc
      scenariomwh[[scenarios[c]]] <- seasonmwh
      scenariocd[[scenarios[c]]] <- seasoncd
    }
    
    # store data for each GCM
    gcmpc[[gcms[m]]] <- scenariopc
    gcmmwh[[gcms[m]]] <- scenariomwh
    gcmcd[[gcms[m]]] <- scenariocd
  }
  
  # store data for each state
  percentchange[[stateids[s]]] <- gcmpc
  mwhchange[[stateids[s]]] <- gcmmwh
  currdata[[stateids[s]]] <- gcmcd
}

setwd(rdatadir)
save(percentchange, mwhchange, currdata, file = "percentchange.RData")

################ SENSITIVITY ANALYSES ######################

setwd(rdatadir)
load('rawdata.RData')
load('cleaneddata.RData')

# Sensitivity to De-Trending/Normalization Process

# OPTIONS: None; Sailor & Munoz De-Trending; Capacity Normalization

# set up data w/o any transformations

# rename energy source values of interest
gendata$EnergySource[gendata$EnergySource == 'Hydroelectric Conventional'] <- 'Hydropower'
gendata$EnergySource[gendata$EnergySource == 'Solar Thermal and Photovoltaic'] <- 'Solar'

# subset generation data by state
CAdata <- gendata[which(gendata$State == 'CA' & gendata$TypeOfProducer == 'Total Electric Power Industry' & (gendata$EnergySource == 'Hydropower' | gendata$EnergySource == 'Solar' | gendata$EnergySource == 'Wind')),]
NYdata <- gendata[which(gendata$State == 'NY' & gendata$TypeOfProducer == 'Total Electric Power Industry' & (gendata$EnergySource == 'Hydropower' | gendata$EnergySource == 'Solar' | gendata$EnergySource == 'Wind')),]
FLdata <- gendata[which(gendata$State == 'FL' & gendata$TypeOfProducer == 'Total Electric Power Industry' & (gendata$EnergySource == 'Hydropower' | gendata$EnergySource == 'Solar' | gendata$EnergySource == 'Wind')),]
GAdata <- gendata[which(gendata$State == 'GA' & gendata$TypeOfProducer == 'Total Electric Power Industry' & (gendata$EnergySource == 'Hydropower' | gendata$EnergySource == 'Solar' | gendata$EnergySource == 'Wind')),]

CAdata <- dcast(CAdata, Year + Month + State + TypeOfProducer ~ EnergySource)
NYdata <- dcast(NYdata, Year + Month + State + TypeOfProducer ~ EnergySource)
FLdata <- dcast(FLdata, Year + Month + State + TypeOfProducer ~ EnergySource)
GAdata <- dcast(GAdata, Year + Month + State + TypeOfProducer ~ EnergySource)

# remove unnecessary variables
CAdata <- CAdata[,-c(3,4)]; NYdata <- NYdata[,-c(3,4)]; FLdata <- FLdata[,-c(3,4)]; GAdata <- GAdata[,-c(3,4)]

# store as one dataframe
fsgeneration <- list(CAdata, NYdata, FLdata, GAdata)

# combine generation with NARR data
notransformation <- list()

for (i in 1:4) {
  notransformation[[i]] <- merge(updatedNARR[[i]], fsgeneration[[i]], by = c('Year','Month'))
}

# set up data w/ Sailor & Munoz de-trending
sm_detrending <- alldata

# set up data w/ capacity normalization (https://www.eia.gov/todayinenergy/detail.php?id=42995)

setwd(datadir5)

filenames <- list.files(pattern="*.xlsx", full.names=F)
cols_of_interest <- c('Plant Code', 'Nameplate Capacity (MW)', 'State', 'Energy Source 1', 'Status')

capdata <- c()

for (i in 4:length(filenames)) {
  annualdata <- read_excel(paste(datadir5, '/',filenames[i], sep = ''))
  annualdata <- annualdata[,which(names(annualdata) %in% cols_of_interest)]
  annualdata <- annualdata[which(annualdata$Status == 'OP'),]
  annualdata <- annualdata[which(annualdata$`Energy Source 1` == 'WAT' | annualdata$`Energy Source 1` == 'SUN' | annualdata$`Energy Source 1` == 'WND'),]
  annualdata <- annualdata %>% group_by(`Plant Code`, State, `Energy Source 1`) %>% 
      summarize(cap_MW = sum(`Nameplate Capacity (MW)`))
  annualdata <- annualdata[which(annualdata$State == 'CA' | annualdata$State == 'NY' | annualdata$State == 'FL' | annualdata$State == 'GA'),]
  annualdata$year <- rep(as.numeric(gsub(".xlsx", "", filenames[i])), nrow(annualdata))
  capdata <- rbind(capdata, annualdata)
}

leap_years = c(2000, 2004, 2008, 2012, 2016, 2020, 2024)

capdata <- capdata %>% group_by(State, `Energy Source 1`, year) %>%
  summarize(cap_MW = sum(cap_MW)) %>%
  mutate(maxgen_MWh = cap_MW * case_when(year %in% leap_years ~ 8784,
                                         TRUE ~ 8760),
         source = case_when(`Energy Source 1` == 'SUN' ~ 'Solar',
                            `Energy Source 1` == 'WAT' ~ 'Hydropower',
                            `Energy Source 1` == 'WND' ~ 'Wind'))

# create normalized data
states <- c('CA', 'NY', 'FL', 'GA')
months <- expand.grid(year = unique(capdata$year), month = 1:12)
capdata <- left_join(capdata, months, by = "year")
capdata <- capdata %>% dcast(State + year + month ~ source, value.var = 'maxgen_MWh')
names(capdata)[c(2:3)] <- c('Year', 'Month')

normalizeddata <- list()
for (i in 1:4) {
  gendata <- fsgeneration[[i]]
  stcapdata <- capdata[which(capdata$State == states[i]),]
  combineddata <- merge(gendata, stcapdata, by = c('Year', 'Month'))
  
  if ('Wind' %in% names(gendata)) {
    stnormdata <- data.frame(Year = combineddata$Year, Month = combineddata$Month,
                                              Hydropower = combineddata$Hydropower.x/combineddata$Hydropower.y,
                                              Solar = combineddata$Solar.x/combineddata$Solar.y,
                                              Wind = combineddata$Wind.x/combineddata$Wind.y)
  } else {
    stnormdata <- data.frame(Year = combineddata$Year, Month = combineddata$Month,
                                              Hydropower = combineddata$Hydropower.x/combineddata$Hydropower.y,
                                              Solar = combineddata$Solar.x/combineddata$Solar.y)
  }
  
  normalizeddata[[states[i]]] <- merge(updatedNARR[[i]], stnormdata, by = c('Year','Month'))
}

# compare datasets

# labels for seasons [summer, winter]
seasonindices <- list(c(4:9), c(10:12,1:3))
seasonnames <- c('summer', 'winter')

# initialize variables
datasets <- list(notransformation, sm_detrending, normalizeddata)
datasetsnrmse <- list(); datasetsrsq <- list()
datasetnames <- c('NoTransformation', 'smDetrending', 'CapNormalization')

for (d in 1:length(datasets)) {
  data <- datasets[[d]]
  
  # initialize variables
  allrsq <- list(); allnrmse <- list()
  
  # loop through each state
  for (s in 1:4) {
    state <- data[[s]]
    
    # initialize variables
    seasonrsq <- list(); seasonnrmse <- list()
    
    # loop through each season
    for (t in 1:2) {
      # extract data for each season
      season <- state[state$Month %in% seasonindices[[t]],]
      
      # initialize variables
      sourcersq <- list(); sourcenrmse <- list()
      
      # loop through each source
      for (g in 1:3) {
        source <- season[,c(g+16, 3:16)]
        
        # remove NA values
        source <- na.omit(source)
        
        # set up cross validation with selected variables
        n <- nrow(source)
        set.seed(11)
        updatedsource <- source[sample(n),]
        
        # initialize yhat and test data lists
        glmyhat_all <- c(); marsyhat_all <- c(); cartyhat_all <- c(); rfyhat_all <- c()
        bartyhat_all <- c(); svmyhat_all <- c(); testdata_all <- c(); meanonlyyhat_all <- c()
        
        # run models (LOOCV due to small sample size)
        for (j in 1:n) {
          # split into training and test dataset
          testData <- updatedsource[j,]
          trainData <- updatedsource[-j,]
          
          # create formula
          myformula <- paste(names(trainData)[1], "~", paste(names(trainData[2:length(trainData)]), collapse="+"))
          myformula <- as.formula(myformula)
          meanonlyformula <- as.formula(paste(names(trainData)[1], "~ 1"))
          
          # run models
          glmmodel <- glm(formula = myformula, data = trainData); marsmodel <- earth(formula = myformula, data = trainData)
          cartmodel <- rpart(formula = myformula, data = trainData); rfmodel <- randomForest(formula = myformula, data = trainData)
          bartmodel <- bartMachine(X = trainData[,2:length(trainData)], y = trainData[,1], serialize = TRUE)
          svmmodel <- svm(formula = myformula, data = trainData)
          meanonly <- lm(meanonlyformula, data = trainData) 
          
          # fit models
          glmyhat <- predict(glmmodel, newdata = testData); marsyhat <- predict(marsmodel, newdata = testData)
          cartyhat <- predict(cartmodel, newdata = testData); rfyhat <- predict(rfmodel, newdata = testData)
          bartyhat <- predict(bartmodel, new_data = testData[,2:length(testData)])
          svmyhat <- predict(svmmodel, newdata = testData)
          meanonlyyhat <- predict(meanonly, newdata = testData)
          
          # store yhat 
          glmyhat_all <- append(glmyhat_all, glmyhat); marsyhat_all <- append(marsyhat_all, marsyhat)
          cartyhat_all <- append(cartyhat_all, cartyhat); rfyhat_all <- append(rfyhat_all, rfyhat)
          bartyhat_all <- append(bartyhat_all, bartyhat); svmyhat_all <- append(svmyhat_all, svmyhat)
          meanonlyyhat_all <- append(meanonlyyhat_all, meanonlyyhat)
          
          # store test data 
          testdata_all <- append(testdata_all, testData[,1])
        }
        
        # store source-specific data
        varname <- names(updatedsource)[1]
        
        sourcersq[[varname]] <- data.frame("glm" = cor(glmyhat_all, testdata_all)^2, "mars" = cor(marsyhat_all, testdata_all)^2, "cart" = cor(cartyhat_all, testdata_all)^2, "rf" = cor(rfyhat_all, testdata_all)^2, 
                                           "bart" = cor(bartyhat_all, testdata_all)^2, "svm" = cor(svmyhat_all, testdata_all)^2)
        sourcenrmse[[varname]] <- data.frame("meanonly" = sqrt(sum((meanonlyyhat_all-testdata_all)^2)/length(testdata_all))/(range(testdata_all)[2]-range(testdata_all)[1]), 
                                             "glm" = sqrt(sum((glmyhat_all-testdata_all)^2)/length(testdata_all))/(range(testdata_all)[2]-range(testdata_all)[1]), 
                                             "mars" = sqrt(sum((marsyhat_all-testdata_all)^2)/length(testdata_all))/(range(testdata_all)[2]-range(testdata_all)[1]), 
                                             "cart" = sqrt(sum((cartyhat_all-testdata_all)^2)/length(testdata_all))/(range(testdata_all)[2]-range(testdata_all)[1]), 
                                             "rf" = sqrt(sum((rfyhat_all-testdata_all)^2)/length(testdata_all))/(range(testdata_all)[2]-range(testdata_all)[1]),
                                             "bart" = sqrt(sum((bartyhat_all-testdata_all)^2)/length(testdata_all))/(range(testdata_all)[2]-range(testdata_all)[1]),
                                             "svm" = sqrt(sum((svmyhat_all-testdata_all)^2)/length(testdata_all))/(range(testdata_all)[2]-range(testdata_all)[1]))
       
        # account for some state without all three sources
        if (length(state) == 18 & g == 2) {
          break
        }
      }
      # store season-specific data
      seasonrsq[[seasonnames[t]]] <- sourcersq; seasonnrmse[[seasonnames[t]]] <- sourcenrmse
    }
    
    # store state-specific data
    statenames <- c('CA', 'NY', 'FL', 'GA')
    
    allrsq[[statenames[s]]] <- seasonrsq
    allnrmse[[statenames[s]]] <- seasonnrmse
  }
  # store dataset-specific data
  datasetsrsq[[datasetnames[d]]] <- allrsq
  datasetsnrmse[[datasetnames[d]]] <- allnrmse
}

setwd(rdatadir)
save(datasetsrsq, datasetsnrmse, file = "normalizationcomparison.RData")

# Sensitivity to variables selected through VSURF

# OPTIONS: Threshold-Based (VSURF Round 1); Interpretation (VSURF Round 2); Prediction (VSURF Round 3) x 10 repetitions w/ different random seeds

# initialize variables
statenames <- c('CA', 'NY', 'FL', 'GA')
seasonindices <- list(c(4:9), c(10:12,1:3))
seasonnames <- c('summer', 'winter')
allnrmse <- list(); allvars <- list()

# loop through each state
for (s in 1:4) {
  state <- alldata[[s]]
  
  # initialize variables
  seasonnrmse <- list(); seasonvars <- list()
  
  # loop through each season
  for (t in 1:2) {
    # extract data for each season
    season <- state[state$Month %in% seasonindices[[t]],]
    
    # initialize variables
    sourcenrmse <- list(); selectedvars <- list()
    
    # loop through each source
    for (g in 1:3) {
      source <- season[,c(g+16, 3:16)]
      
      # remove NA values
      source <- na.omit(source)
      
      # initialize variables
      vsurfvars <- list()
      roundnrmse <- data.frame('Threshold' = c(rep(NA,10)), 'Interpretation' = c(rep(NA,10)), 'Prediction' = c(rep(NA,10)))
      
      # loop through each repetition
      for (r in 1:10) {
        # set up random seed generator
        x <- sample(0:10000, size = 1)
        set.seed(x)
        
        # implement VSURF for variable selection
        vsurf_obj <- VSURF(x = source[,2:length(source)], y = source[,1])
        
        # store variables
        if (is.null(vsurf_obj$varselect.pred) == FALSE) {
          updatedsource <- list(source[,c(1, 1 + vsurf_obj$varselect.thres)], source[,c(1, 1 + vsurf_obj$varselect.interp)], source[,c(1, 1 + vsurf_obj$varselect.pred)])
        } else {
          updatedsource <- list(source[,c(1, 1 + vsurf_obj$varselect.thres)], source[,c(1, 1 + vsurf_obj$varselect.interp)])
        }
        
        # initialize variables
        cvnrmse <- c()
        
        # loop through each VSURF round
        for (v in 1:length(updatedsource)) {
          
          rounddata <- updatedsource[[v]]
          
          # set up cross validation with selected variables
          n <- nrow(rounddata)
          rounddata <- rounddata[sample(n),]
          
          # initialize variables
          rfyhat_all <- c(); testdata_all <- c()
          
          # run models (LOOCV due to small sample size)
          for (j in 1:n) {
            # split into training and test dataset
            testData <- rounddata[j,]
            trainData <- rounddata[-j,]
            
            # create formula
            myformula <- paste(names(trainData)[1], "~", paste(names(trainData[2:length(trainData)]), collapse="+"))
            myformula <- as.formula(myformula)
            
            # run model
            rfmodel <- randomForest(formula = myformula, data = trainData)
            
            # fit model
            rfyhat <- predict(rfmodel, newdata = testData)
            
            # store yhat 
            rfyhat_all <- append(rfyhat_all, rfyhat)
            
            # store test data 
            testdata_all <- append(testdata_all, testData[,1])
          }
          
          # store repetition-specific data
          cvnrmse <- append(cvnrmse, sqrt(sum((rfyhat_all-testdata_all)^2)/length(testdata_all))/(range(testdata_all)[2]-range(testdata_all)[1]))
        }
        
        # store VSURF round-specific data
        if (length(cvnrmse) == 3) {
          roundnrmse[r,] <- cvnrmse
          vsurfvars[[r]] <- list(names(source[,1 + vsurf_obj$varselect.thres]), names(source[,1 + vsurf_obj$varselect.interp]), names(source[,1 + vsurf_obj$varselect.pred]))
        } else {
          roundnrmse[r,] <- c(cvnrmse, NA)
          vsurfvars[[r]] <- list(names(source[,1 + vsurf_obj$varselect.thres]), names(source[,1 + vsurf_obj$varselect.interp]))
        }
        
      }
      # store source-specific data
      varname <- names(rounddata)[1]
      
      selectedvars[[varname]] <- vsurfvars
      sourcenrmse[[varname]] <- roundnrmse
      
      # account for some state without all three sources
      if (length(state) == 18 & g == 2) {
        break
      }
    }
    # store season-specific data
    seasonnrmse[[seasonnames[t]]] <- sourcenrmse
    seasonvars[[seasonnames[t]]] <- selectedvars
  }
  
  # store state-specific data
  allnrmse[[statenames[s]]] <- seasonnrmse
  allvars[[statenames[s]]] <- seasonvars
}

setwd(rdatadir)
save(allnrmse, allvars, file = "vsurfexperiment.RData")

# Sensitivity to Ensemble Choices

# OPTIONS: regular & weighted ensemble of GLM, RF, BART, & SVM, + other combinations of those

setwd(rdatadir)
load('modelrunresults.RData')

allensemblenrmse <- list()
combinations <- c('glm-rf-bart-svm', 'glm-bart-svm', 'glm-rf-svm', 'glm-rf-bart', 'rf-bart-svm', 'rf-svm')
statenames <- c('CA', 'NY', 'FL', 'GA')
seasonnames <- c('summer', 'winter')

# loop through each state
for (i in 1:length(allyhat)) {
  state <- allyhat[[i]]
  
  seasonensemblemean <- list(); seasonensemblenrmse <- list()
  
  # loop through each season
  for (t in 1:2) {
    season <- state[[t]]
    
    sourcenrmse <- list()
    
    # loop through each source
    for (j in 1:length(season)) {
      source <- season[[j]]
      
      ensembledata <- list(data.frame('glm' = source$mars, 'rf' = source$rf, 'bart' = source$bart, 'svm' = source$svm),
                           data.frame('glm' = source$mars, 'bart' = source$bart, 'svm' = source$svm),
                           data.frame('glm' = source$mars, 'rf' = source$rf, 'svm' = source$svm),
                           data.frame('glm' = source$mars, 'rf' = source$rf, 'bart' = source$bart),
                           data.frame('rf' = source$rf, 'bart' = source$bart, 'svm' = source$svm),
                           data.frame('rf' = source$rf, 'svm' = source$svm))
      
      weights <- list(data.frame(models = c('glm', 'rf', 'bart', 'svm'), weights = c(0.28, 0.26, 0.11, 0.34)),
                      data.frame(models = c('glm', 'bart', 'svm'), weights = c(0.38, 0.15, 0.46)),
                      data.frame(models = c('glm', 'rf', 'svm'), weights = c(0.32, 0.30, 0.38)),
                      data.frame(models = c('glm', 'rf', 'bart'), weights = c(0.15, 0.40, 0.17)),
                      data.frame(models = c('rf', 'bart', 'svm'), weights = c(0.37, 0.16, 0.47)),
                      data.frame(models = c('rf', 'svm'), weights = c(0.44, 0.56)))
      
      models <- list(c(as.name('glm'), as.name('rf'), as.name('bart'), as.name('svm')), c(as.name('glm'), as.name('bart'), as.name('svm')), 
                     c(as.name('glm'), as.name('rf'), as.name('svm')), c(as.name('glm'), as.name('rf'), as.name('bart')), 
                     c(as.name('rf'), as.name('bart'), as.name('svm')), c(as.name('rf'), as.name('svm')))
      
      ensemblenrmse <- list()
      
      # loop through each combination
      for (c in 1:6) {
        # calculate unweighted ensemble
        unweighted <- rowMeans(ensembledata[[c]])
        
        # calculated weighted ensemble
        weighted <- apply(ensembledata[[c]], 1, function(x) weighted.mean(x, weights[[c]]$weights))
        
        # calculate RMSE
        ensemblenrmse[['unweighted']][[combinations[c]]] <- sqrt(sum((unweighted-source$testData)^2)/nrow(source))/(range(source$testData)[2]-range(source$testData)[1])
        ensemblenrmse[['weighted']][[combinations[c]]] <- sqrt(sum((weighted-source$testData)^2)/nrow(source))/(range(source$testData)[2]-range(source$testData)[1])
      }
      
      # store source-specific data
      varname <- names(season)[j]
      sourcenrmse[[varname]] <- ensemblenrmse
    }
    
    # store season-specific data
    seasonensemblenrmse[[seasonnames[t]]] <- sourcenrmse
  }
  
  # store state-specific data
  allensemblenrmse[[statenames[i]]] <- seasonensemblenrmse
}

setwd(rdatadir)
save(allensemblenrmse, file = "ensembleexperiments.RData")

# Sensitivity to Excluding/Including Net Radiation (W/m^2)

# OPTIONS: Exclusion; Inclusion

# read in data
radiationdata <- read.table(paste(datadir2, '/monthly_downwardRadation_1979_2023.txt', sep = ""), header=T, fill = TRUE, stringsAsFactors = F)

names(radiationdata)[c(1,2)] <- c('Year', 'Month')

allstatesrad <- list(CA = radiationdata[which(radiationdata$Year >= 2000 & radiationdata$Year <= 2022),c(1,2,which(names(radiationdata) == 'ID_06'))], 
                     NY = radiationdata[which(radiationdata$Year >= 2000 & radiationdata$Year <= 2022),c(1,2,which(names(radiationdata) == 'ID_36'))],
                     FL = radiationdata[which(radiationdata$Year >= 2000 & radiationdata$Year <= 2022),c(1,2,which(names(radiationdata) == 'ID_12'))], 
                     GA = radiationdata[which(radiationdata$Year >= 2000 & radiationdata$Year <= 2022),c(1,2,which(names(radiationdata) == 'ID_13'))])

# add radiation to monthly NARR data

updatedNARR <- list()
statenames <- c('CA', 'NY', 'FL', 'GA')

# loop through each state
for (i in 1:4) {
  names(allstatesrad[[i]])[3] <- 'avg_rad'
  updatedNARR[[statenames[i]]] <- merge(monthlyNARRdata[[i]], allstatesrad[[i]])
}

# combine generation with NARR data
alldata <- list()

for (i in 1:4) {
  alldata[[i]] <- merge(updatedNARR[[i]], stateGENdata[[i]], by = c('Year','Month'))
}

for (i in 1:4) {
  plot(alldata[[i]]$avg_temp, alldata[[i]]$avg_rad)
}

# labels for seasons [summer, winter]
seasonindices <- list(c(4:9), c(10:12,1:3))
seasonnames <- c('summer', 'winter')

# initialize variables
allradnrmse <- list(); allradselecteddata <- list()

# loop through each state
for (s in 1:4) {
  state <- alldata[[s]]
  
  # initialize variables
  seasonnrmse <- list(); seasonselecteddata <- list()
  
  # loop through each season
  for (t in 1:2) {
    # extract data for each season
    season <- state[state$Month %in% seasonindices[[t]],]
    
    # initialize variables
    sourcenrmse <- list(); selecteddata <- list()
    
    # loop through each source
    for (g in 1:3) {
      source <- season[,c(g+16, 3:16)]
      
      # remove NA values
      source <- na.omit(source)
      
      # implement VSURF for variable selection
      vsurf_obj <- VSURF(x = source[,2:length(source)], y = source[,1])
      
      # store variables
      updatedsource <- source[,c(1, 1 + vsurf_obj$varselect.interp)]
      
      # set up cross validation with selected variables
      n <- nrow(updatedsource)
      set.seed(11)
      updatedsource <- updatedsource[sample(n),]
      
      # initialize yhat and test data lists
      glmyhat_all <- c(); marsyhat_all <- c(); cartyhat_all <- c(); rfyhat_all <- c()
      bartyhat_all <- c(); svmyhat_all <- c(); testdata_all <- c(); meanonlyyhat_all <- c()
      
      # run models (LOOCV due to small sample size)
      for (j in 1:n) {
        # split into training and test dataset
        testData <- updatedsource[j,]
        trainData <- updatedsource[-j,]
        
        # create formula
        myformula <- paste(names(trainData)[1], "~", paste(names(trainData[2:length(trainData)]), collapse="+"))
        myformula <- as.formula(myformula)
        meanonlyformula <- as.formula(paste(names(trainData)[1], "~ 1"))
        
        # run models
        glmmodel <- glm(formula = myformula, data = trainData); marsmodel <- earth(formula = myformula, data = trainData)
        cartmodel <- rpart(formula = myformula, data = trainData); rfmodel <- randomForest(formula = myformula, data = trainData)
        bartmodel <- bartMachine(X = data.frame(avg_rad = trainData[,2:length(trainData)]), y = trainData[,1], serialize = TRUE)
        svmmodel <- svm(formula = myformula, data = trainData)
        meanonly <- lm(meanonlyformula, data = trainData) 
        
        # fit models
        glmyhat <- predict(glmmodel, newdata = testData); marsyhat <- predict(marsmodel, newdata = testData)
        cartyhat <- predict(cartmodel, newdata = testData); rfyhat <- predict(rfmodel, newdata = testData)
        bartyhat <- predict(bartmodel, new_data = data.frame(avg_rad = testData[,2:length(testData)]))
        svmyhat <- predict(svmmodel, newdata = testData)
        meanonlyyhat <- predict(meanonly, newdata = testData)
        
        # store yhat 
        glmyhat_all <- append(glmyhat_all, glmyhat); marsyhat_all <- append(marsyhat_all, marsyhat)
        cartyhat_all <- append(cartyhat_all, cartyhat); rfyhat_all <- append(rfyhat_all, rfyhat)
        bartyhat_all <- append(bartyhat_all, bartyhat); svmyhat_all <- append(svmyhat_all, svmyhat)
        meanonlyyhat_all <- append(meanonlyyhat_all, meanonlyyhat)
        
        # store test data 
        testdata_all <- append(testdata_all, testData[,1])
      }
      
      # store source-specific data
      varname <- names(updatedsource)[1]
      
      sourcenrmse[[varname]] <- data.frame("meanonly" = sqrt(sum((meanonlyyhat_all-testdata_all)^2)/length(testdata_all))/(range(testdata_all)[2]-range(testdata_all)[1]), 
                                           "glm" = sqrt(sum((glmyhat_all-testdata_all)^2)/length(testdata_all))/(range(testdata_all)[2]-range(testdata_all)[1]), 
                                           "mars" = sqrt(sum((marsyhat_all-testdata_all)^2)/length(testdata_all))/(range(testdata_all)[2]-range(testdata_all)[1]), 
                                           "cart" = sqrt(sum((cartyhat_all-testdata_all)^2)/length(testdata_all))/(range(testdata_all)[2]-range(testdata_all)[1]), 
                                           "rf" = sqrt(sum((rfyhat_all-testdata_all)^2)/length(testdata_all))/(range(testdata_all)[2]-range(testdata_all)[1]),
                                           "bart" = sqrt(sum((bartyhat_all-testdata_all)^2)/length(testdata_all))/(range(testdata_all)[2]-range(testdata_all)[1]),
                                           "svm" = sqrt(sum((svmyhat_all-testdata_all)^2)/length(testdata_all))/(range(testdata_all)[2]-range(testdata_all)[1]))
      selecteddata[[varname]] <- updatedsource
      
      # account for some state without all three sources
      if (length(state) == 18 & g == 2) {
        break
      }
    }
    # store season-specific data
    seasonnrmse[[seasonnames[t]]] <- sourcenrmse
    seasonselecteddata[[seasonnames[t]]] <- selecteddata
  }
  
  # store state-specific data
  statenames <- c('CA', 'NY', 'FL', 'GA')
  
  allradnrmse[[statenames[s]]] <- seasonnrmse
  allradselecteddata[[statenames[s]]] <- seasonselecteddata
}

setwd(rdatadir)
save(allradnrmse, allradselecteddata, file = "radiationexperiment.RData")

################ FIGURES & TABLES ####################

setwd(rdatadir)
load('modelrunresults.RData')
load('ensembleresults.RData')
load('percentchange.RData')

# FIGURE: Model Performance (NRMSE)

nrmse <- c(unlist(allnrmse$CA$summer$Hydropower), unlist(allnrmse$CA$summer$Solar), unlist(allnrmse$CA$summer$Wind),
           unlist(allnrmse$NY$summer$Hydropower), unlist(allnrmse$NY$summer$Solar), unlist(allnrmse$NY$summer$Wind),
           unlist(allnrmse$FL$summer$Hydropower), unlist(allnrmse$FL$summer$Solar),
           unlist(allnrmse$GA$summer$Hydropower), unlist(allnrmse$GA$summer$Solar),
           unlist(allnrmse$CA$winter$Hydropower), unlist(allnrmse$CA$winter$Solar), unlist(allnrmse$CA$winter$Wind),
           unlist(allnrmse$NY$winter$Hydropower), unlist(allnrmse$NY$winter$Solar), unlist(allnrmse$NY$winter$Wind),
           unlist(allnrmse$FL$winter$Hydropower), unlist(allnrmse$FL$winter$Solar),
           unlist(allnrmse$GA$winter$Hydropower), unlist(allnrmse$GA$winter$Solar))

models <- rep(c('Mean-Only', 'GLM', 'MARS', 'CART', 'RF', 'BART', 'SVM'), 20)
states <- c(rep('CA', 7*3), rep('NY', 7*3), rep('FL', 7*2), rep('GA', 7*2), rep('CA', 7*3), rep('NY', 7*3), rep('FL', 7*2), rep('GA', 7*2))
sources <- c(rep('Hydropower',7), rep('Solar', 7), rep('Wind', 7), rep('Hydropower', 7), rep('Solar', 7), rep('Wind', 7), rep('Hydropower', 7), rep('Solar', 7), rep('Hydropower', 7), rep('Solar', 7),
             rep('Hydropower',7), rep('Solar', 7), rep('Wind', 7), rep('Hydropower', 7), rep('Solar', 7), rep('Wind', 7), rep('Hydropower', 7), rep('Solar', 7), rep('Hydropower', 7), rep('Solar', 7))
seasons <- c(rep('Summer', 70), rep('Winter', 70))

plotdata <- data.frame(nrmse, models, states, sources, seasons)

p1 <- ggplot(plotdata[which(plotdata$seasons == 'Summer'),]) + 
  geom_bar(aes(x = states, y = nrmse, fill = models), stat = 'identity', position = 'dodge') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Model', values = c('#7fc97f','#beaed4','#fdc086','#386cb0','#ffff99','#bf5b17', '#f0027f')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('NRMSE') +
  ggtitle('Summer')

p2 <- ggplot(plotdata[which(plotdata$seasons == 'Winter'),]) + 
  geom_bar(aes(x = states, y = nrmse, fill = models), stat = 'identity', position = 'dodge') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Model', values = c('#7fc97f','#beaed4','#fdc086','#386cb0','#ffff99','#bf5b17', '#f0027f')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('NRMSE') +
  ggtitle('Winter')

setwd(outputdir)
pdf('modelperformance_nrmse.pdf', width = 11, height = 10)
plot_grid(p1, p2, nrow = 2)
dev.off()

# FIGURE: Model Performance (R^2)

rsq <- c(unlist(allrsq$CA$summer$Hydropower), unlist(allrsq$CA$summer$Solar), unlist(allrsq$CA$summer$Wind),
         unlist(allrsq$NY$summer$Hydropower), unlist(allrsq$NY$summer$Solar), unlist(allrsq$NY$summer$Wind),
         unlist(allrsq$FL$summer$Hydropower), unlist(allrsq$FL$summer$Solar), 
         unlist(allrsq$GA$summer$Hydropower), unlist(allrsq$GA$summer$Solar), 
         unlist(allrsq$CA$winter$Hydropower), unlist(allrsq$CA$winter$Solar), unlist(allrsq$CA$winter$Wind),
         unlist(allrsq$NY$winter$Hydropower), unlist(allrsq$NY$winter$Solar), unlist(allrsq$NY$winter$Wind),
         unlist(allrsq$FL$winter$Hydropower), unlist(allrsq$FL$winter$Solar),
         unlist(allrsq$GA$winter$Hydropower), unlist(allrsq$GA$winter$Solar))

models <- rep(c('GLM', 'MARS', 'CART', 'RF', 'BART', 'SVM'), 20)
states <- c(rep('CA', 6*3), rep('NY', 6*3), rep('FL', 6*2), rep('GA', 6*2), rep('CA', 6*3), rep('NY', 6*3), rep('FL', 6*2), rep('GA', 6*2))
sources <- c(rep('Hydropower',6), rep('Solar', 6), rep('Wind', 6), rep('Hydropower', 6), rep('Solar', 6), rep('Wind', 6), rep('Hydropower', 6), rep('Solar', 6), rep('Hydropower', 6), rep('Solar', 6),
             rep('Hydropower',6), rep('Solar', 6), rep('Wind', 6), rep('Hydropower', 6), rep('Solar', 6), rep('Wind', 6), rep('Hydropower', 6), rep('Solar', 6), rep('Hydropower', 6), rep('Solar', 6))
seasons <- c(rep('Summer', 60), rep('Winter', 60))

plotdata <- data.frame(rsq, models, states, sources, seasons)

p1 <- ggplot(plotdata[which(plotdata$seasons == 'Summer'),]) + 
  geom_bar(aes(x = states, y = rsq, fill = models), stat = 'identity', position = 'dodge') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Model', values = c('#7fc97f','#beaed4','#fdc086','#386cb0','#bf5b17', '#f0027f')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('NRMSE') +
  ggtitle('Summer')

p2 <- ggplot(plotdata[which(plotdata$seasons == 'Winter'),]) + 
  geom_bar(aes(x = states, y = rsq, fill = models), stat = 'identity', position = 'dodge') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Model', values = c('#7fc97f','#beaed4','#fdc086','#386cb0','#bf5b17', '#f0027f')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('NRMSE') +
  ggtitle('Winter')

setwd(outputdir)
pdf('modelperformance_rsq.pdf', width = 11, height = 10)
plot_grid(p1, p2, nrow = 2)
dev.off()

# FIGURE: Variable Selection Results (option 1)

variables <- rep(c('RH - Min', 'RH - Max', 'RH - Mean', 'Temp - Min', 'Temp - Max', 'Temp - Mean', 'WS - Min',
                  'WS - Max', 'WS - Mean', 'Precip - Total', 'Precip - Min', 'Precip - Max', 'Precip - Mean', 
                  'Rad - Mean'), 20)

selectedvariables <- c(NA, NA, NA, NA, 1, 1, NA, NA, NA, NA, NA, NA, NA, 1,    # CA Hydro (Summer)
                       NA, NA, NA, NA, NA, 1, NA, NA, NA, NA, NA, NA, NA, 1,   # CA Solar (Summer)
                       1, NA, NA, 1, 1, 1, NA, NA, 1, NA, NA, 1, NA, 1,        # CA Wind (Summer)
                       NA, NA, 1, 1, NA, 1, NA, NA, NA, NA, NA, NA, NA, 1,     # NY Hydro (Summer)
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1,  # NY Solar (Summer)
                       NA, NA, NA, 1, NA, 1, NA, NA, 1, NA, NA, NA, NA, NA,    # NY Wind (Summer)
                       1, NA, 1, 1, 1, 1, NA, NA, NA, NA, NA, 1, NA, 1,        # FL Hydro (Summer)
                       1, NA, 1, NA, NA, NA, NA, NA, NA, 1, NA, NA, 1, 1,      # FL Solar (Summer)
                       1, NA, NA, NA, NA, NA, NA, 1, 1, 1, NA, 1, 1, NA,       # GA Hydro (Summer)
                       1, 1, 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1, NA,     # GA Solar (Summer) 
                       1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1,   # CA Hydro (Winter)
                       NA, NA, 1, NA, 1, NA, NA, NA, NA, NA, NA, NA, NA, 1,    # CA Solar (Winter)
                       NA, NA, NA, NA, 1, NA, NA, NA, 1, NA, NA, NA, NA, 1,    # CA Wind (Winter)  
                       NA, NA, NA, NA, NA, 1, NA, NA, NA, NA, NA, NA, NA, 1,   # NY Hydro (Winter)
                       1, 1, 1, 1, 1, 1, NA, NA, NA, NA, NA, NA, NA, 1,        # NY Solar (Winter)
                       NA, NA, NA, NA, NA, NA, NA, NA, 1, NA, NA, NA, NA, 1,   # NY Wind (Winter)
                       NA, NA, NA, NA, 1, 1, NA, NA, 1, 1, NA, NA, 1, 1,       # FL Hydro (Winter)
                       NA, 1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, NA, 1,     # FL Solar (Winter)
                       NA, 1, 1, NA, 1, NA, NA, NA, NA, 1, NA, NA, 1, NA,      # GA Hydro (Winter)
                       NA, NA, NA, NA, 1, 1, NA, NA, NA, 1, NA, NA, 1, 1)      # GA Solar (Winter)

states <- c(rep('CA', 14*3), rep('NY', 14*3), rep('FL', 14*2), rep('GA', 14*2), rep('CA', 14*3), rep('NY', 14*3), rep('FL', 14*2), rep('GA', 14*2))
sources <- c(rep('Hydropower',14), rep('Solar', 14), rep('Wind', 14), rep('Hydropower',14), rep('Solar', 14), rep('Wind', 14), rep('Hydropower',14), 
             rep('Solar', 14),rep('Hydropower',14), rep('Solar', 14), rep('Hydropower',14), rep('Solar', 14), rep('Wind', 14), rep('Hydropower',14), 
             rep('Solar', 14), rep('Wind', 14),rep('Hydropower',14), rep('Solar', 14),rep('Hydropower',14), rep('Solar', 14))
seasons <- c(rep('Summer', 140), rep('Winter', 140))

plotdata <- data.frame(variables, selectedvariables, states, sources, seasons)

p1 <- ggplot(plotdata[which(plotdata$seasons == 'Summer'),]) + geom_point(aes(x = states, y = variables, size = selectedvariables), pch = 21, fill = '#386cb0', color = '#386cb0') + theme_light() + 
  xlab('') + ylab('Predictor Variable') + theme(text = element_text(size=20)) + ggtitle('Summer') +
  facet_wrap(~sources) + scale_size_continuous(guide = 'none')

p2 <- ggplot(plotdata[which(plotdata$seasons == 'Winter'),]) + geom_point(aes(x = states, y = variables, size = selectedvariables), pch = 21, fill = '#386cb0', color = '#386cb0') + theme_light() + 
  xlab('') + ylab('Predictor Variable') + theme(text = element_text(size=20)) + ggtitle('Winter') +
  facet_wrap(~sources) + scale_size_continuous(guide = 'none')

setwd(outputdir)
pdf('selectedvariables.pdf', width = 7, height = 8.5)
plot_grid(p1, p2, nrow = 2)
dev.off()


# FIGURE: Variable Selection Results (option 2)

selectedvariables <- c(NA, NA, NA, NA, 3, 2, NA, NA, NA, NA, NA, NA, NA, 1,    # CA Hydro (Summer)
                       NA, NA, NA, NA, NA, 2, NA, NA, NA, NA, NA, NA, NA, 1,   # CA Solar (Summer)
                       5, NA, NA, 4, 6, 3, NA, NA, 2, NA, NA, 7, NA, 1,        # CA Wind (Summer)
                       NA, NA, 4, 2, NA, 3, NA, NA, NA, NA, NA, NA, NA, 1,     # NY Hydro (Summer)
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1,  # NY Solar (Summer)
                       NA, NA, NA, 2, NA, 3, NA, NA, 1, NA, NA, NA, NA, NA,    # NY Wind (Summer)
                       3, NA, 5, 4, 1, 2, NA, NA, NA, NA, NA, 7, NA, 6,        # FL Hydro (Summer)
                       2, NA, 5, NA, NA, NA, NA, NA, NA, 4, NA, NA, 3, 1,      # FL Solar (Summer)
                       6, NA, NA, NA, NA, NA, NA, 4, 5, 1, NA, 3, 2, NA,       # GA Hydro (Summer)
                       1, 3, 2, NA, NA, NA, NA, NA, NA, NA, NA, NA, 4, NA,     # GA Solar (Summer) 
                       2, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1,   # CA Hydro (Winter)
                       NA, NA, 3, NA, 2, NA, NA, NA, NA, NA, NA, NA, NA, 1,    # CA Solar (Winter)
                       NA, NA, NA, NA, 2, NA, NA, NA, 3, NA, NA, NA, NA, 1,    # CA Wind (Winter)  
                       NA, NA, NA, NA, NA, 2, NA, NA, NA, NA, NA, NA, NA, 1,   # NY Hydro (Winter)
                       7, 2, 4, 6, 5, 3, NA, NA, NA, NA, NA, NA, NA, 1,        # NY Solar (Winter)
                       NA, NA, NA, NA, NA, NA, NA, NA, 1, NA, NA, NA, NA, 2,   # NY Wind (Winter)
                       NA, NA, NA, NA, 4, 1, NA, NA, 6, 2, NA, NA, 5, 3,       # FL Hydro (Winter)
                       NA, 1, NA, NA, NA, NA, NA, NA, 3, 4, NA, NA, NA, 2,     # FL Solar (Winter)
                       NA, 3, 5, NA, 4, NA, NA, NA, NA, 1, NA, NA, 2, NA,      # GA Hydro (Winter)
                       NA, NA, NA, NA, 3, 4, NA, NA, NA, 1, NA, NA, 2, 5)      # GA Solar (Winter)

plotdata <- data.frame(variables, selectedvariables, states, sources, seasons)

p1 <- ggplot(plotdata[which(plotdata$seasons == 'Summer'),]) + geom_point(aes(x = states, y = variables, fill = selectedvariables, size = selectedvariables), pch = 21) + theme_light() + 
  xlab('') + ylab('Predictor Variable') + theme(text = element_text(size=20)) + ggtitle('Summer') +
  facet_wrap(~sources) + scale_size_continuous(trans = 'reverse', range = c(8,8), guide = 'none') +
  scale_fill_gradient(high='#f7fbff', low = '#08306b', 
                      breaks = c(1, 5, 10), limits = c(1,12), name = 'Rank') +
  theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm'))

p2 <- ggplot(plotdata[which(plotdata$seasons == 'Winter'),]) + geom_point(aes(x = states, y = variables, fill = selectedvariables, size = selectedvariables), pch = 21) + theme_light() + 
  xlab('') + ylab('Predictor Variable') + theme(text = element_text(size=20)) + ggtitle('Winter') +
  facet_wrap(~sources) + scale_size_continuous(trans = 'reverse', range = c(8,8), guide = 'none') +
  scale_fill_gradient(high='#f7fbff', low = '#08306b', 
                      breaks = c(1, 5, 10), limits = c(1,12), name = 'Rank') +
  theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm'))

setwd(outputdir)
pdf('selectedvariables2.pdf', width = 7.5, height = 12)
plot_grid(p1, p2, nrow = 2)
dev.off()

# FIGURE: Variable Selection Results (option 3)

plotdata <- data.frame(variables, selectedvariables, states, sources, seasons)

p1 <- ggplot(plotdata[which(plotdata$states == 'CA' & plotdata$seasons == 'Summer'),]) + 
  geom_point(aes(x = selectedvariables, y = variables), pch = 21, size = 8, fill = '#6baed6', color = '#2171b5') + theme_light() + 
  xlab('') + ylab('') + theme(text = element_text(size=20)) + scale_x_continuous(breaks = seq(1,max(selectedvariables, na.rm = T))) +
  facet_wrap(~sources) + ggtitle('CA Summer')

p2 <- ggplot(plotdata[which(plotdata$states == 'CA' & plotdata$seasons == 'Winter'),]) + 
  geom_point(aes(x = selectedvariables, y = variables), pch = 21, size = 8, fill = '#6baed6', color = '#2171b5') + theme_light() + 
  xlab('') + ylab('') + theme(text = element_text(size=20)) + scale_x_continuous(breaks = seq(1,max(selectedvariables, na.rm = T))) +
  facet_wrap(~sources) + ggtitle('CA Winter')

p3 <- plot_grid(p1, p2, nrow = 2)

p4 <- ggplot(plotdata[which(plotdata$states == 'NY' & plotdata$seasons == 'Summer'),]) + 
  geom_point(aes(x = selectedvariables, y = variables), pch = 21, size = 8, fill = '#6baed6', color = '#2171b5') + theme_light() + 
  xlab('') + ylab('') + theme(text = element_text(size=20)) + scale_x_continuous(breaks = seq(1,max(selectedvariables, na.rm = T))) +
  facet_wrap(~sources) + ggtitle('NY Summer')

p5 <- ggplot(plotdata[which(plotdata$states == 'NY' & plotdata$seasons == 'Winter'),]) + 
  geom_point(aes(x = selectedvariables, y = variables), pch = 21, size = 8, fill = '#6baed6', color = '#2171b5') + theme_light() + 
  xlab('') + ylab('') + theme(text = element_text(size=20)) + scale_x_continuous(breaks = seq(1,max(selectedvariables, na.rm = T))) +
  facet_wrap(~sources) + ggtitle('NY Winter')

p6 <- plot_grid(p4, p5, nrow = 2)

p7 <- ggplot(plotdata[which(plotdata$states == 'FL' & plotdata$seasons == 'Summer'),]) + 
  geom_point(aes(x = selectedvariables, y = variables), pch = 21, size = 8, fill = '#6baed6', color = '#2171b5') + theme_light() + 
  xlab('') + ylab('') + theme(text = element_text(size=20)) + scale_x_continuous(breaks = seq(1,max(selectedvariables, na.rm = T))) +
  facet_wrap(~sources) + ggtitle('FL Summer')

p8 <- ggplot(plotdata[which(plotdata$states == 'FL' & plotdata$seasons == 'Winter'),]) + 
  geom_point(aes(x = selectedvariables, y = variables), pch = 21, size = 8, fill = '#6baed6', color = '#2171b5') + theme_light() + 
  xlab('') + ylab('') + theme(text = element_text(size=20)) + scale_x_continuous(breaks = seq(1,max(selectedvariables, na.rm = T))) +
  facet_wrap(~sources) + ggtitle('FL Winter')

p9 <- plot_grid(p7, p8, nrow = 2)

p10 <- ggplot(plotdata[which(plotdata$states == 'GA' & plotdata$seasons == 'Summer'),]) + 
  geom_point(aes(x = selectedvariables, y = variables), pch = 21, size = 8, fill = '#6baed6', color = '#2171b5') + theme_light() + 
  xlab('') + ylab('') + theme(text = element_text(size=20)) + scale_x_continuous(breaks = seq(1,max(selectedvariables, na.rm = T))) +
  facet_wrap(~sources) + ggtitle('GA Summer')

p11 <- ggplot(plotdata[which(plotdata$states == 'GA' & plotdata$seasons == 'Winter'),]) + 
  geom_point(aes(x = selectedvariables, y = variables), pch = 21, size = 8, fill = '#6baed6', color = '#2171b5') + theme_light() + 
  xlab('') + ylab('') + theme(text = element_text(size=20)) + scale_x_continuous(breaks = seq(1,max(selectedvariables, na.rm = T))) +
  facet_wrap(~sources) + ggtitle('GA Winter')

p12 <- plot_grid(p10, p11, nrow = 2)

setwd(outputdir)
pdf('selectedvariables3.pdf', width = 15, height = 20)
  plot_grid(p3, p6, p9, p12, nrow = 2)
dev.off()

pdf('selectedvariables3a.pdf', width = 15, height = 10)
plot_grid(p3, p6, nrow = 1)
dev.off()

pdf('selectedvariables3b.pdf', width = 15, height = 10)
plot_grid(p9, p12, nrow = 1)
dev.off()

# FIGURE: Partial Dependence 

predictors <- c(); yhat <- c(); variables <- c(); sources <- c(); seasons <- c(); states <- c()

# loop through all states
for (i in 1:length(allsvmpd)) {
  state <- allsvmpd[[i]]
  
  # loop through each season
  for (t in 1:length(state)) {
    season <- state[[t]]
    
    # loop through all sources
    for (j in 1:length(season)) {
      source <- season[[j]]
      
      # loop through all predictors
      for (k in 1:length(source)) {
        predictors <- append(predictors, source[[k]][,1])
        yhat <- append(yhat, source[[k]][,2])
        variables <- append(variables, rep(names(source[[k]])[1],51))
      }
      
      numpred <- length(source)
      sources <- append(sources, rep(names(season)[j], numpred*51))
      seasons <- append(seasons, rep(names(state)[t], numpred*51))
      states <- append(states, rep(names(allsvmpd)[i], numpred*51))
    }
  }
}

split_string <- strsplit(variables, split = "\\s|_")

plotdata <- data.frame(predictors, yhat, sources, seasons, states, variables, 
                       measure = sapply(split_string, `[`, 1),
                       name = sapply(split_string, `[`, 2))

# California Summer
p1 <- ggplot(plotdata[which(plotdata$states == 'CA' & plotdata$seasons == 'summer' & plotdata$name == 'temp'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Temperature (°C)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean', 'Max', 'Min'), values = c('#66c2a5', '#fc8d62', '#8da0cb'))

p2 <- ggplot(plotdata[which(plotdata$states == 'CA' & plotdata$seasons == 'summer' & plotdata$name == 'rh'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Relative Humidity (%)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Min'), values = c('#8da0cb'))

p3 <- ggplot(plotdata[which(plotdata$states == 'CA' & plotdata$seasons == 'summer' & plotdata$name == 'ws'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Wind Speed (m/s)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Min'), values = c('#8da0cb'))

p4 <- ggplot(plotdata[which(plotdata$states == 'CA' & plotdata$seasons == 'summer' & plotdata$name == 'precip'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Precipitation (mm)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Max'), values = c('#fc8d62'))

p5 <- ggplot(plotdata[which(plotdata$states == 'CA' & plotdata$seasons == 'summer' & plotdata$name == 'rad'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Radiation (W/m^2)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean'), values = c('#66c2a5'))

setwd(outputdir)
pdf('partialdependenceCAsummer.pdf', width = 11, height = 20)
plot_grid(p1, p2, p3, p4, p5, nrow = 5)
dev.off()

# California Winter
p1 <- ggplot(plotdata[which(plotdata$states == 'CA' & plotdata$seasons == 'winter' & plotdata$name == 'temp'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Temperature (°C)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Max'), values = c('#fc8d62'))

p2 <- ggplot(plotdata[which(plotdata$states == 'CA' & plotdata$seasons == 'winter' & plotdata$name == 'rh'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Relative Humidity (%)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean', 'Min'), values = c('#66c2a5', '#8da0cb'))

p3 <- ggplot(plotdata[which(plotdata$states == 'CA' & plotdata$seasons == 'winter' & plotdata$name == 'ws'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Wind Speed (m/s)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean'), values = c('#66c2a5'))

p4 <- ggplot(plotdata[which(plotdata$states == 'CA' & plotdata$seasons == 'winter' & plotdata$name == 'rad'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Radiation (W/m^2)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean'), values = c('#66c2a5'))

setwd(outputdir)
pdf('partialdependenceCAwinter.pdf', width = 11, height = 16)
plot_grid(p1, p2, p3, p4, nrow = 4)
dev.off()

# New York Summer
p1 <- ggplot(plotdata[which(plotdata$states == 'NY' & plotdata$seasons == 'summer' & plotdata$name == 'temp'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Temperature (°C)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean', 'Min'), values = c('#66c2a5', '#8da0cb'))

p2 <- ggplot(plotdata[which(plotdata$states == 'NY' & plotdata$seasons == 'summer' & plotdata$name == 'rh'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Relative Humidity (%)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean'), values = c('#66c2a5'))

p3 <- ggplot(plotdata[which(plotdata$states == 'NY' & plotdata$seasons == 'summer' & plotdata$name == 'ws'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Wind Speed (m/s)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean'), values = c('#66c2a5'))

p4 <- ggplot(plotdata[which(plotdata$states == 'NY' & plotdata$seasons == 'summer' & plotdata$name == 'rad'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Radiation (W/m^2)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean'), values = c('#66c2a5'))

setwd(outputdir)
pdf('partialdependenceNYsummer.pdf', width = 11, height = 16)
plot_grid(p1, p2, p3, p4, nrow = 4)
dev.off()

# New York Winter
p1 <- ggplot(plotdata[which(plotdata$states == 'NY' & plotdata$seasons == 'winter' & plotdata$name == 'temp'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Temperature (°C)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean', 'Max', 'Min'), values = c('#66c2a5', '#fc8d62', '#8da0cb'))

p2 <- ggplot(plotdata[which(plotdata$states == 'NY' & plotdata$seasons == 'winter' & plotdata$name == 'rh'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Relative Humidity (%)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean', 'Max', 'Min'), values = c('#66c2a5', '#fc8d62', '#8da0cb'))

p3 <- ggplot(plotdata[which(plotdata$states == 'NY' & plotdata$seasons == 'winter' & plotdata$name == 'ws'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Wind Speed (m/s)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean'), values = c('#66c2a5'))

p4 <- ggplot(plotdata[which(plotdata$states == 'NY' & plotdata$seasons == 'winter' & plotdata$name == 'rad'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Radiation (W/m^2)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean'), values = c('#66c2a5'))

setwd(outputdir)
pdf('partialdependenceNYwinter.pdf', width = 11, height = 16)
plot_grid(p1, p2, p3, p4, nrow = 4)
dev.off()

# Florida Summer
p1 <- ggplot(plotdata[which(plotdata$states == 'FL' & plotdata$seasons == 'summer' & plotdata$name == 'temp'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Temperature (°C)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean', 'Max', 'Min'), values = c('#66c2a5', '#fc8d62', '#8da0cb'))

p2 <- ggplot(plotdata[which(plotdata$states == 'FL' & plotdata$seasons == 'summer' & plotdata$name == 'rh'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Relative Humidity (%)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean', 'Min'), values = c('#66c2a5', '#8da0cb'))

p3 <- ggplot(plotdata[which(plotdata$states == 'FL' & plotdata$seasons == 'summer' & plotdata$name == 'precip'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Precipitation (mm)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean', 'Max', 'Total'), values = c('#66c2a5','#fc8d62','#e5c494'))

p4 <- ggplot(plotdata[which(plotdata$states == 'FL' & plotdata$seasons == 'summer' & plotdata$name == 'rad'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Radiation (W/m^2)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean'), values = c('#66c2a5'))

setwd(outputdir)
pdf('partialdependenceFLsummer.pdf', width = 11, height = 16)
plot_grid(p1, p2, p3, p4, nrow = 4)
dev.off()

# Florida Winter
p1 <- ggplot(plotdata[which(plotdata$states == 'FL' & plotdata$seasons == 'winter' & plotdata$name == 'temp'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Temperature (°C)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean', 'Max'), values = c('#66c2a5', '#fc8d62'))

p2 <- ggplot(plotdata[which(plotdata$states == 'FL' & plotdata$seasons == 'winter' & plotdata$name == 'rh'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Relative Humidity (%)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Max'), values = c('#fc8d62'))

p3 <- ggplot(plotdata[which(plotdata$states == 'FL' & plotdata$seasons == 'winter' & plotdata$name == 'ws'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Wind Speed (m/s)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean'), values = c('#66c2a5'))

p4 <- ggplot(plotdata[which(plotdata$states == 'FL' & plotdata$seasons == 'winter' & plotdata$name == 'precip'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Precipitation (mm)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean', 'Total'), values = c('#66c2a5', '#e5c494'))

p5 <- ggplot(plotdata[which(plotdata$states == 'FL' & plotdata$seasons == 'winter' & plotdata$name == 'rad'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Radiation (W/m^2)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean'), values = c('#66c2a5'))

setwd(outputdir)
pdf('partialdependenceFLwinter.pdf', width = 11, height = 20)
plot_grid(p1, p2, p3, p4, p5, nrow = 5)
dev.off()

# Georgia Summer
p1 <- ggplot(plotdata[which(plotdata$states == 'GA' & plotdata$seasons == 'summer' & plotdata$name == 'rh'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Relative Humidity (%)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean', 'Max', 'Min'), values = c('#66c2a5', '#fc8d62', '#8da0cb'))

p2 <- ggplot(plotdata[which(plotdata$states == 'GA' & plotdata$seasons == 'summer' & plotdata$name == 'ws'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Wind Speed (m/s)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean', 'Max'), values = c('#66c2a5', '#fc8d62'))

p3 <- ggplot(plotdata[which(plotdata$states == 'GA' & plotdata$seasons == 'summer' & plotdata$name == 'precip'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Precipitation (mm)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean', 'Max', 'Total'), values = c('#66c2a5', '#fc8d62', '#e5c494'))

setwd(outputdir)
pdf('partialdependenceGAsummer.pdf', width = 11, height = 16)
plot_grid(p1, p2, p3, nrow = 3)
dev.off()

# Georgia Winter
p1 <- ggplot(plotdata[which(plotdata$states == 'GA' & plotdata$seasons == 'winter' & plotdata$name == 'temp'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Temperature (°C)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean', 'Max'), values = c('#66c2a5', '#fc8d62'))

p2 <- ggplot(plotdata[which(plotdata$states == 'GA' & plotdata$seasons == 'winter' & plotdata$name == 'rh'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Relative Humidity (%)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean', 'Max'), values = c('#66c2a5', '#fc8d62'))

p3 <- ggplot(plotdata[which(plotdata$states == 'GA' & plotdata$seasons == 'winter' & plotdata$name == 'precip'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Precipitation (mm)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean', 'Total'), values = c('#66c2a5', '#e5c494'))

p4 <- ggplot(plotdata[which(plotdata$states == 'GA' & plotdata$seasons == 'winter' & plotdata$name == 'rad'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Radiation (W/m^2)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean'), values = c('#66c2a5'))

setwd(outputdir)
pdf('partialdependenceGAwinter.pdf', width = 11, height = 16)
plot_grid(p1, p2, p3, p4, nrow = 4)
dev.off()

# FIGURE: Actual vs. Predicted (SVM Only)

predicted <- c(); actual <-c(); sources <- c(); seasons <- c(); states <- c(); models <- c()

# loop through all states
for (i in 1:length(allyhat)) {
  state <- allyhat[[i]]
  
  # loop through all seasons
  for (t in 1:length(state)) {
    season <- state[[t]]
    
    # loop through all sources
    for (j in 1:length(season)) {
      source <- season[[j]]
      
      # loop through all models
      for (k in 1:7) {
        predicted <- append(predicted, source[,k])
        models <- append(models, rep(names(source)[k], nrow(source)))
        actual <- append(actual, source$testData)
      }
      
      ndatapoints <- nrow(source)
      sources <- append(sources, rep(names(season)[j], ndatapoints*7))
      seasons <- append(seasons, rep(names(state)[t], ndatapoints*7))
      states <- append(states, rep(names(allyhat)[i], ndatapoints*7))
    }
  }
}

plotdata <- data.frame(predicted, actual, sources, states, models, seasons)

p1 <- ggplot(plotdata[which(plotdata$models == 'svm' & plotdata$seasons == 'summer'),]) +
  geom_point(aes(x = actual, y = predicted, color = states)) + geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + 
  scale_x_continuous(breaks = breaks_pretty()) + theme_light() + theme(text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'State', values = c('#66c2a5', '#fc8d62','#8da0cb', '#e5c494')) +
  ylab('Predicted Values') + xlab('Actual Values') + ggtitle('Summer') +
  guides(color = guide_legend(override.aes = list(size = 5))) 

p2 <- ggplot(plotdata[which(plotdata$models == 'svm' & plotdata$seasons == 'winter'),]) +
  geom_point(aes(x = actual, y = predicted, color = states)) + geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + 
  scale_x_continuous(breaks = breaks_pretty()) + theme_light() + theme(text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'State', values = c('#66c2a5', '#fc8d62','#8da0cb', '#e5c494')) +
  ylab('Predicted Values') + xlab('Actual Values') + ggtitle('Winter') +
  guides(color = guide_legend(override.aes = list(size = 5))) 

setwd(outputdir)
pdf('predVact_SVM.pdf', width = 10, height = 8)
plot_grid(p1, p2, nrow = 2)
dev.off()

# FIGURE: Actual vs. Predicted (Non-SVM Models)

p1 <- ggplot(plotdata[which(plotdata$models == 'glm' & plotdata$seasons == 'summer'),]) +
  geom_point(aes(x = actual, y = predicted, color = states)) + geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + 
  scale_x_continuous(breaks = breaks_pretty()) + theme_light() + theme(text = element_text(size = 14)) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'State', values = c('#66c2a5', '#fc8d62','#8da0cb', '#e5c494')) +
  ylab('Predicted Values') + xlab('Actual Values') + ggtitle('Generalized Linear Models')

p2 <- ggplot(plotdata[which(plotdata$models == 'mars' & plotdata$seasons == 'summer'),]) +
  geom_point(aes(x = actual, y = predicted, color = states)) + geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + 
  scale_x_continuous(breaks = breaks_pretty()) + theme_light() + theme(text = element_text(size = 14)) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'State', values = c('#66c2a5', '#fc8d62','#8da0cb', '#e5c494')) +
  ylab('Predicted Values') + xlab('Actual Values') + ggtitle('Multivariate Adaptive Regression Splines')

p3 <- ggplot(plotdata[which(plotdata$models == 'cart' & plotdata$seasons == 'summer'),]) +
  geom_point(aes(x = actual, y = predicted, color = states)) + geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + 
  scale_x_continuous(breaks = breaks_pretty()) + theme_light() + theme(text = element_text(size = 14)) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'State', values = c('#66c2a5', '#fc8d62','#8da0cb', '#e5c494')) +
  ylab('Predicted Values') + xlab('Actual Values') + ggtitle('Decision Tree')

p4 <- ggplot(plotdata[which(plotdata$models == 'rf' & plotdata$seasons == 'summer'),]) +
  geom_point(aes(x = actual, y = predicted, color = states)) + geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + 
  scale_x_continuous(breaks = breaks_pretty()) + theme_light() + theme(text = element_text(size = 14)) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'State', values = c('#66c2a5', '#fc8d62','#8da0cb', '#e5c494')) +
  ylab('Predicted Values') + xlab('Actual Values') + ggtitle('Random Forest')

p5 <- ggplot(plotdata[which(plotdata$models == 'bart' & plotdata$seasons == 'summer'),]) +
  geom_point(aes(x = actual, y = predicted, color = states)) + geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + 
  scale_x_continuous(breaks = breaks_pretty()) + theme_light() + theme(text = element_text(size = 14)) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'State', values = c('#66c2a5', '#fc8d62','#8da0cb', '#e5c494')) +
  ylab('Predicted Values') + xlab('Actual Values') + ggtitle('Bayesian Additive Regression Trees')

setwd(outputdir)
pdf('predVact_othermodelsSummer.pdf', width = 8, height = 18)
plot_grid(p1, p2, p3, p4, p5, nrow = 5)
dev.off()

p1 <- ggplot(plotdata[which(plotdata$models == 'glm' & plotdata$seasons == 'winter'),]) +
  geom_point(aes(x = actual, y = predicted, color = states)) + geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + 
  scale_x_continuous(breaks = breaks_pretty()) + theme_light() + theme(text = element_text(size = 14)) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'State', values = c('#66c2a5', '#fc8d62','#8da0cb', '#e5c494')) +
  ylab('Predicted Values') + xlab('Actual Values') + ggtitle('Generalized Linear Models')

p2 <- ggplot(plotdata[which(plotdata$models == 'mars' & plotdata$seasons == 'winter'),]) +
  geom_point(aes(x = actual, y = predicted, color = states)) + geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + 
  scale_x_continuous(breaks = breaks_pretty()) + theme_light() + theme(text = element_text(size = 14)) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'State', values = c('#66c2a5', '#fc8d62','#8da0cb', '#e5c494')) +
  ylab('Predicted Values') + xlab('Actual Values') + ggtitle('Multivariate Adaptive Regression Splines')

p3 <- ggplot(plotdata[which(plotdata$models == 'cart' & plotdata$seasons == 'winter'),]) +
  geom_point(aes(x = actual, y = predicted, color = states)) + geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + 
  scale_x_continuous(breaks = breaks_pretty()) + theme_light() + theme(text = element_text(size = 14)) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'State', values = c('#66c2a5', '#fc8d62','#8da0cb', '#e5c494')) +
  ylab('Predicted Values') + xlab('Actual Values') + ggtitle('Decision Tree')

p4 <- ggplot(plotdata[which(plotdata$models == 'rf' & plotdata$seasons == 'winter'),]) +
  geom_point(aes(x = actual, y = predicted, color = states)) + geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + 
  scale_x_continuous(breaks = breaks_pretty()) + theme_light() + theme(text = element_text(size = 14)) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'State', values = c('#66c2a5', '#fc8d62','#8da0cb', '#e5c494')) +
  ylab('Predicted Values') + xlab('Actual Values') + ggtitle('Random Forest')

p5 <- ggplot(plotdata[which(plotdata$models == 'bart' & plotdata$seasons == 'winter'),]) +
  geom_point(aes(x = actual, y = predicted, color = states)) + geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + 
  scale_x_continuous(breaks = breaks_pretty()) + theme_light() + theme(text = element_text(size = 14)) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'State', values = c('#66c2a5', '#fc8d62','#8da0cb', '#e5c494')) +
  ylab('Predicted Values') + xlab('Actual Values') + ggtitle('Bayesian Additive Regression Trees')

setwd(outputdir)
pdf('predVact_othermodelsWinter.pdf', width = 8, height = 18)
plot_grid(p1, p2, p3, p4, p5, nrow = 5)
dev.off()

# FIGURE: Ensemble Model Performance

nrmse <- c(unlist(allnrmse$CA$summer$Hydropower), unlist(allnrmse$CA$summer$Solar), unlist(allnrmse$CA$summer$Wind),
           unlist(allnrmse$NY$summer$Hydropower), unlist(allnrmse$NY$summer$Solar), unlist(allnrmse$NY$summer$Wind),
           unlist(allnrmse$FL$summer$Hydropower), unlist(allnrmse$FL$summer$Solar),
           unlist(allnrmse$GA$summer$Hydropower), unlist(allnrmse$GA$summer$Solar),
           unlist(allnrmse$CA$winter$Hydropower), unlist(allnrmse$CA$winter$Solar), unlist(allnrmse$CA$winter$Wind),
           unlist(allnrmse$NY$winter$Hydropower), unlist(allnrmse$NY$winter$Solar), unlist(allnrmse$NY$winter$Wind),
           unlist(allnrmse$FL$winter$Hydropower), unlist(allnrmse$FL$winter$Solar),
           unlist(allnrmse$GA$winter$Hydropower), unlist(allnrmse$GA$winter$Solar))

models <- rep(c('Mean-Only', 'GLM', 'MARS', 'CART', 'RF', 'BART', 'SVM'), 20)
states <- c(rep('CA', 7*3), rep('NY', 7*3), rep('FL', 7*2), rep('GA', 7*2), rep('CA', 7*3), rep('NY', 7*3), rep('FL', 7*2), rep('GA', 7*2))
sources <- c(rep('Hydropower',7), rep('Solar', 7), rep('Wind', 7), rep('Hydropower', 7), rep('Solar', 7), rep('Wind', 7), rep('Hydropower', 7), rep('Solar', 7), rep('Hydropower', 7), rep('Solar', 7),
             rep('Hydropower',7), rep('Solar', 7), rep('Wind', 7), rep('Hydropower', 7), rep('Solar', 7), rep('Wind', 7), rep('Hydropower', 7), rep('Solar', 7), rep('Hydropower', 7), rep('Solar', 7))
seasons <- c(rep('Summer', 70), rep('Winter', 70))

modeldata <- data.frame(nrmse, models, states, sources, seasons)
modeldata <- modeldata[which(modeldata$models == 'MARS' | modeldata$models == 'RF' | modeldata$models == 'BART' | modeldata$models == 'SVM'),]

nrmse <- c(allensemblenrmse[[1]][[1]], allensemblenrmse[[2]][[1]], allensemblenrmse[[3]][[1]], allensemblenrmse[[4]][[1]], 
           allensemblenrmse[[1]][[2]], allensemblenrmse[[2]][[2]], allensemblenrmse[[3]][[2]], allensemblenrmse[[4]][[2]])

models <- rep('ZEnsemble', 20)
states <- c(rep('CA', 3), rep('NY', 3), rep('FL', 2), rep('GA', 2), rep('CA', 3), rep('NY', 3), rep('FL', 2), rep('GA', 2))
sources <- c('Hydropower', 'Solar', 'Wind', 'Hydropower', 'Solar', 'Wind', 'Hydropower', 'Solar', 'Hydropower', 'Solar',
             'Hydropower', 'Solar', 'Wind', 'Hydropower', 'Solar', 'Wind', 'Hydropower', 'Solar', 'Hydropower', 'Solar')
seasons <- c(rep('Summer', 10), rep('Winter', 10))

ensembledata2 <- data.frame(nrmse, models, states, sources, seasons)

plotdata <- rbind(modeldata, ensembledata2)

p1 <- ggplot(plotdata[which(plotdata$seasons == 'Summer'),]) + geom_bar(aes(x = states, y = nrmse, fill = models), stat='identity', position = 'dodge') +
  facet_wrap(~sources, scales = 'free', nrow = 1) + theme_light() + ylab('NRMSE') + xlab('State') + ggtitle('Summer') +
  theme(text = element_text(size = 16), legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_fill_manual(name = 'Model', values = c('#66c2a5', '#fc8d62','#8da0cb','#b3b3b3', '#e5c494'), labels = c('BART', 'MARS', 'RF', 'SVM', 'Ensemble'))

p2 <- ggplot(plotdata[which(plotdata$seasons == 'Winter'),]) + geom_bar(aes(x = states, y = nrmse, fill = models), stat='identity', position = 'dodge') +
  facet_wrap(~sources, scales = 'free', nrow = 1) + theme_light() + ylab('NRMSE') + xlab('State') + ggtitle('Winter') +
  theme(text = element_text(size = 16), legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_fill_manual(name = 'Model', values = c('#66c2a5', '#fc8d62','#8da0cb','#b3b3b3', '#e5c494'), labels = c('BART', 'MARS', 'RF', 'SVM', 'Ensemble'))

setwd(outputdir)  
pdf('modelperformance_ensemble.pdf', width = 11, height = 9)
plot_grid(p1, p2, nrow = 2)
dev.off()  


# FIGURE: Ensemble Model Predicted vs. Actual

predicted <- c(); actual <-c(); sources <- c(); seasons <- c(); states <- c()
statenames <- c('CA', 'NY', 'FL', 'GA')
seasonnames <- c('Summer', 'Winter')
sourcenames <- c('Hydropower', 'Solar', 'Wind')

# loop through all states
for (i in 1:length(allyhat)) {
  state <- allensemblemean[[i]]
  
  # loop through each season
  for (t in 1:length(state)) {
    season <- state[[t]]
    
    ndatapoints <- 0
    
    # loop through all sources
    for (j in 1:length(season)) {
      source <- season[[j]]
      
      predicted <- append(predicted, source)
      actual <- append(actual, allyhat[[i]][[t]][[j]]$testData)
      sources <- append(sources, rep(sourcenames[j], length(source)))
      ndatapoints <- ndatapoints + length(source)
    }
    seasons <- append(seasons, rep(seasonnames[t], ndatapoints))
    states <- append(states, rep(statenames[i], ndatapoints))
  }
}


plotdata <- data.frame(predicted, actual, states, sources, seasons)  

p1 <- ggplot(plotdata[which(plotdata$seasons == 'Summer'),]) + geom_point(aes(x = actual, y = predicted, color = states)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme_light() + ylab('Predicted Values') + xlab('Actual Values') +
  theme(text = element_text(size = 16), legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'State', values = c('#66c2a5', '#fc8d62','#8da0cb', '#e5c494')) + 
  ggtitle('Summer') + guides(color = guide_legend(override.aes = list(size = 5))) 

p2 <- ggplot(plotdata[which(plotdata$seasons == 'Winter'),]) + geom_point(aes(x = actual, y = predicted, color = states)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme_light() + ylab('Predicted Values') + xlab('Actual Values') +
  theme(text = element_text(size = 16), legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'State', values = c('#66c2a5', '#fc8d62','#8da0cb', '#e5c494')) + 
  ggtitle('Winter') + guides(color = guide_legend(override.aes = list(size = 5))) 

setwd(outputdir)
pdf('predVact_ensemble.pdf',  width = 10, height = 8)
plot_grid(p1, p2, nrow = 2)
dev.off()  
  
# FIGURE: Model Projections - Relative Change (Summer Only; Ensemble Only)

states <- c(); gcms <- c(); scenarios <- c(); seasons <- c(); sources <- c(); models <- c(); pc <- c(); mwh_cap <- c(); cd_cap <- c()

# loop through all states
for (i in 1:length(percentchange)) {
  state <- percentchange[[i]]; state2 <- mwhchange[[i]]; state3 <- currdata[[i]]
  
  # loop through all GCMs
  for (j in 1:length(state)) {
    gcm <- state[[j]]; gcm2 <- state2[[j]]; gcm3 <- state3[[j]]
    
    # loop through all scenarios
    for (k in 1:length(gcm)) {
      scenario <- gcm[[k]]; scenario2 <- gcm2[[k]]; scenario3 <- gcm3[[k]]
      
      # loop through all seasons
      for (l in 1:length(scenario)) {
        season <- scenario[[l]]; season2 <- scenario2[[l]]; season3 <- scenario3[[l]]
        
        # loop through all sources
        for (m in 1:length(season)) {
          # extract data for ensemble
          pc <- append(pc, season[[m]])
          mwh_cap <- append(mwh_cap, season2[[m]])
          cd_cap <- append(cd_cap, season3[[m]])
        }
        
        # store names of labels
        n <- length(season)*7 # number of sources*7 models
        models <- append(models, rep(names(season[[m]]), length(season)))
        sources <- append(sources, rep(names(season), each = 7))
        seasons <- append(seasons, rep(names(scenario)[l], n))
        scenarios <- append(scenarios, rep(names(gcm)[k], n))
        gcms <- append(gcms, rep(names(state)[j], n))
        states <- append(states, rep(names(percentchange)[i], n))
      }
    }
  }
}

unlisteddata <- data.frame(states, gcms, scenarios, seasons, sources, models, pc, mwh_cap, cd_cap)

# population data from: https://doi.org/10.1088/1748-9326/aba5b1
unlisteddata <- unlisteddata %>% mutate(population = case_when((states == 'CA' & scenarios == 'ssp126') ~ 51401694,
                                                               (states == 'CA' & scenarios == 'ssp585') ~ 58734194,
                                                               (states == 'NY' & scenarios == 'ssp126') ~ 27884922,
                                                               (states == 'NY' & scenarios == 'ssp585') ~ 31954504,
                                                               (states == 'FL' & scenarios == 'ssp126') ~ 23306711,
                                                               (states == 'FL' & scenarios == 'ssp585') ~ 26647795,
                                                               (states == 'GA' & scenarios == 'ssp126') ~ 11790569,
                                                               (states == 'GA' & scenarios == 'ssp585') ~ 13781366),
                                        mwh = mwh_cap*population, cd_mwh = cd_cap*population)

compositedata <- unlisteddata %>% group_by(states, gcms, scenarios, seasons, models) %>%
  summarize(mwh_cap = sum(mwh_cap), mwh = sum(mwh), cd_mwh = sum(cd_mwh), cd_cap = sum(cd_cap)) %>% 
  mutate(sources = 'z', population = NA, pc = (mwh/cd_mwh)*100)

combineddata <- rbind(unlisteddata, compositedata)

summerdata <- combineddata[which(combineddata$seasons == 'summer' & combineddata$models == 'ensemble'),]
ensembledata <- summerdata[which(summerdata$gcms == 'ensemble'),]
minsmaxs <- summerdata %>% group_by(states, scenarios, sources) %>% summarise(mins = min(pc), maxs = max(pc))

plotdata <- merge(ensembledata, minsmaxs)

sourcenames <- list('z' = 'Composite', 'Hydropower' = 'Hydropower', 'Solar' = 'Solar', 'Wind' = 'Wind')
sourcelabels <- function(variable,value){
  return(sourcenames[value])
}

setwd(outputdir)
pdf('projections_summerensemble.pdf', width = 11, height = 4.7)
ggplot(plotdata, aes(x = states, ymin = mins, ymax = maxs, lower = mins, upper = maxs, middle = pc)) + 
  geom_abline(slope = 0, intercept = 0, linetype = 'dashed') +
  geom_boxplot(aes(fill = scenarios), stat = 'identity') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Scenario', values = c('#67a9cf', '#ef8a62'), labels = c('SSP1-2.6', 'SSP5-8.5')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free', nrow = 1, labeller = sourcelabels) + ylab('Difference (%)')
dev.off()

p1 <- ggplot(plotdata, aes(x = states, ymin = mins, ymax = maxs, lower = mins, upper = maxs, middle = pc)) + 
  geom_abline(slope = 0, intercept = 0, linetype = 'dashed') +
  geom_boxplot(aes(fill = scenarios), stat = 'identity') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Scenario', values = c('#67a9cf', '#ef8a62'), labels = c('SSP1-2.6', 'SSP5-8.5')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free', nrow = 1, labeller = sourcelabels) + ylab('Difference (%)') + ggtitle('Summer')

# FIGURE: Model Projections - Relative Change (Winter Only; Ensemble Only)

winterdata <- combineddata[which(combineddata$seasons == 'winter' & combineddata$models == 'ensemble'),]
ensembledata <- winterdata[which(winterdata$gcms == 'ensemble'),]
minsmaxs <- winterdata %>% group_by(states, scenarios, sources) %>% summarise(mins = min(pc), maxs = max(pc))

plotdata <- merge(ensembledata, minsmaxs)

setwd(outputdir)
pdf('projections_winterensemble.pdf', width = 11, height = 4.7)
ggplot(plotdata, aes(x = states, ymin = mins, ymax = maxs, lower = mins, upper = maxs, middle = pc)) + 
  geom_abline(slope = 0, intercept = 0, linetype = 'dashed') +
  geom_boxplot(aes(fill = scenarios), stat = 'identity') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Scenario', values = c('#67a9cf', '#ef8a62'), labels = c('SSP1-2.6', 'SSP5-8.5')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free', nrow = 1, labeller = sourcelabels) + ylab('Difference (%)')
dev.off()

p2 <- ggplot(plotdata, aes(x = states, ymin = mins, ymax = maxs, lower = mins, upper = maxs, middle = pc)) + 
  geom_abline(slope = 0, intercept = 0, linetype = 'dashed') +
  geom_boxplot(aes(fill = scenarios), stat = 'identity') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Scenario', values = c('#67a9cf', '#ef8a62'), labels = c('SSP1-2.6', 'SSP5-8.5')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free', nrow = 1, labeller = sourcelabels) + ylab('Difference (%)') + ggtitle('Winter')

# FIGURE: Model Projections - Relative Change (Both Seasons; Ensemble Only)

setwd(outputdir)
pdf('projections_ensemble.pdf', width = 11, height = 10)
plot_grid(p1, p2, nrow = 2)
dev.off()

# FIGURE: Model Projections - MWh Change (Both Seasons; Ensemble Only)
summerdata <- combineddata[which(combineddata$seasons == 'summer' & combineddata$models == 'ensemble'),]
ensembledata <- summerdata[which(summerdata$gcms == 'ensemble'),]
minsmaxs <- summerdata %>% group_by(states, scenarios, sources) %>% summarise(mins = min(mwh), maxs = max(mwh))

plotdata <- merge(ensembledata, minsmaxs)

sourcenames <- list('z' = 'Composite', 'Hydropower' = 'Hydropower', 'Solar' = 'Solar', 'Wind' = 'Wind')
sourcelabels <- function(variable,value){
  return(sourcenames[value])
}

p1 <- ggplot(plotdata, aes(x = states, ymin = mins, ymax = maxs, lower = mins, upper = maxs, middle = mwh)) + 
  geom_abline(slope = 0, intercept = 0, linetype = 'dashed') +
  geom_boxplot(aes(fill = scenarios), stat = 'identity') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Scenario', values = c('#67a9cf', '#ef8a62'), labels = c('SSP1-2.6', 'SSP5-8.5')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free', nrow = 1, labeller = sourcelabels) + ylab('Difference (MWh)') + ggtitle('Summer')

winterdata <- combineddata[which(combineddata$seasons == 'winter' & combineddata$models == 'ensemble'),]
ensembledata <- winterdata[which(winterdata$gcms == 'ensemble'),]
minsmaxs <- winterdata %>% group_by(states, scenarios, sources) %>% summarise(mins = min(mwh), maxs = max(mwh))

plotdata <- merge(ensembledata, minsmaxs)

p2 <- ggplot(plotdata, aes(x = states, ymin = mins, ymax = maxs, lower = mins, upper = maxs, middle = mwh)) + 
  geom_abline(slope = 0, intercept = 0, linetype = 'dashed') +
  geom_boxplot(aes(fill = scenarios), stat = 'identity') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Scenario', values = c('#67a9cf', '#ef8a62'), labels = c('SSP1-2.6', 'SSP5-8.5')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free', nrow = 1, labeller = sourcelabels) + ylab('Difference (%)') + ggtitle('Winter')

setwd(outputdir)
pdf('projections_ensemble_mwh.pdf', width = 14, height = 10)
plot_grid(p1, p2, nrow = 2)
dev.off()

# FIGURE: Model Projections (Summer Only; All ML Models)

summerdata <- unlisteddata[which(unlisteddata$seasons == 'summer'),]
ensembledata <- summerdata[which(summerdata$gcms == 'ensemble'),]
minsmaxs <- summerdata %>% group_by(states, scenarios, sources, models) %>% summarise(mins = min(pc), maxs = max(pc))

plotdata <- merge(ensembledata, minsmaxs)

p1 <- ggplot(plotdata[which(plotdata$scenarios == 'ssp126'),], aes(x = states, ymin = mins, ymax = maxs, lower = mins, upper = maxs, middle = pc)) + 
  geom_abline(slope = 0, intercept = 0, linetype = 'dashed') +
  geom_boxplot(aes(fill = models), stat = 'identity') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Model', values = c('#7fc97f','#beaed4','#fdc086','#386cb0','#ffff99','#bf5b17', '#f0027f')) +
                                 theme(legend.position = 'bottom', text = element_text(size = 20)) +
                                 facet_wrap(~sources, scales = 'free') + ylab('Difference (%)') +
  ggtitle('SSP1-2.6')

p2 <- ggplot(plotdata[which(plotdata$scenarios == 'ssp585'),], aes(x = states, ymin = mins, ymax = maxs, lower = mins, upper = maxs, middle = pc)) + 
  geom_abline(slope = 0, intercept = 0, linetype = 'dashed') +
  geom_boxplot(aes(fill = models), stat = 'identity') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Model', values = c('#7fc97f','#beaed4','#fdc086','#386cb0','#ffff99','#bf5b17', '#f0027f')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('Difference (%)') +
  ggtitle('SSP5-8.5')

setwd(outputdir)
pdf('projections_summerallMLmodels.pdf', width = 11, height = 11)
plot_grid(p1, p2, nrow = 2)
dev.off()

# FIGURE: Model Projections (Winter Only; All ML Models)

winterdata <- unlisteddata[which(unlisteddata$seasons == 'winter'),]
ensembledata <- winterdata[which(winterdata$gcms == 'ensemble'),]
minsmaxs <- winterdata %>% group_by(states, scenarios, sources, models) %>% summarise(mins = min(pc), maxs = max(pc))

plotdata <- merge(ensembledata, minsmaxs)

p1 <- ggplot(plotdata[which(plotdata$scenarios == 'ssp126'),], aes(x = states, ymin = mins, ymax = maxs, lower = mins, upper = maxs, middle = pc)) + 
  geom_abline(slope = 0, intercept = 0, linetype = 'dashed') +
  geom_boxplot(aes(fill = models), stat = 'identity') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Model', values = c('#7fc97f','#beaed4','#fdc086','#386cb0','#ffff99','#bf5b17', '#f0027f')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('Difference (%)') +
  ggtitle('SSP1-2.6')

p2 <- ggplot(plotdata[which(plotdata$scenarios == 'ssp585'),], aes(x = states, ymin = mins, ymax = maxs, lower = mins, upper = maxs, middle = pc)) + 
  geom_abline(slope = 0, intercept = 0, linetype = 'dashed') +
  geom_boxplot(aes(fill = models), stat = 'identity') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Model', values = c('#7fc97f','#beaed4','#fdc086','#386cb0','#ffff99','#bf5b17', '#f0027f')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('Difference (%)') +
  ggtitle('SSP5-8.5')

setwd(outputdir)
pdf('projections_winterallMLmodels.pdf', width = 11, height = 11)
plot_grid(p1, p2, nrow = 2)
dev.off()

# FIGURE: model projections (Summer Only; All GCMs; Ensemble Only)

plotdata <- unlisteddata[which(unlisteddata$seasons == 'summer' & unlisteddata$models == 'ensemble'),]

p1 <- ggplot(plotdata[which(plotdata$scenarios == 'ssp126'),]) + 
  geom_abline(slope = 0, intercept = 0, linetype = 'dashed') +
  geom_bar(aes(x = states, y = pc, fill = gcms), stat = 'identity', position = 'dodge') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'GCM', values = c('#7fc97f','#beaed4','#fdc086','#386cb0','#ffff99','#bf5b17')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('Difference (%)') +
  ggtitle('SSP1-2.6')

p2 <- ggplot(plotdata[which(plotdata$scenarios == 'ssp585'),]) + 
  geom_abline(slope = 0, intercept = 0, linetype = 'dashed') +
  geom_bar(aes(x = states, y = pc, fill = gcms), stat = 'identity', position = 'dodge') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'GCM', values = c('#7fc97f','#beaed4','#fdc086','#386cb0','#ffff99','#bf5b17')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('Difference (%)') +
  ggtitle('SSP5-8.5')

setwd(outputdir)
pdf('projections_summerallGCMs.pdf', width = 11, height = 11)
plot_grid(p1, p2, nrow = 2)
dev.off()

# FIGURE: model projections (Winter Only; All GCMs; Ensemble Only)

plotdata <- unlisteddata[which(unlisteddata$seasons == 'winter' & unlisteddata$models == 'ensemble'),]

p1 <- ggplot(plotdata[which(plotdata$scenarios == 'ssp126'),]) + 
  geom_abline(slope = 0, intercept = 0, linetype = 'dashed') +
  geom_bar(aes(x = states, y = pc, fill = gcms), stat = 'identity', position = 'dodge') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'GCM', values = c('#7fc97f','#beaed4','#fdc086','#386cb0','#ffff99','#bf5b17')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('Difference (%)') +
  ggtitle('SSP1-2.6')

p2 <- ggplot(plotdata[which(plotdata$scenarios == 'ssp585'),]) + 
  geom_abline(slope = 0, intercept = 0, linetype = 'dashed') +
  geom_bar(aes(x = states, y = pc, fill = gcms), stat = 'identity', position = 'dodge') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'GCM', values = c('#7fc97f','#beaed4','#fdc086','#386cb0','#ffff99','#bf5b17')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('Difference (%)') +
  ggtitle('SSP5-8.5')

setwd(outputdir)
pdf('projections_winterallGCMs.pdf', width = 11, height = 11)
plot_grid(p1, p2, nrow = 2)
dev.off()

# FIGURE: Comparing Normalization Techniques (NRMSE; Summer Only)

setwd(rdatadir)
load('normalizationcomparison.RData')

transformation <- c(); states <- c(); seasons <- c(); sources <- c(); models <- c(); nrmse <- c()

# loop through each dataset
for (d in 1:length(datasetsnrmse)) {
  dataset <- datasetsnrmse[[d]]
  
  # loop through all states
  for (i in 1:length(dataset)) {
    state <- dataset[[i]]
    
    # loop through all seasons
    for (l in 1:length(state)) {
      season <- state[[l]]
      
      # loop through all sources
      for (m in 1:length(season)) {
        # extract percent change for ensemble
        nrmse <- append(nrmse, unlist(season[[m]]))
      }
      
      # store names of labels
      n <- length(season)*7 # number of sources*7 models
      models <- append(models, rep(names(season[[m]]), length(season)))
      sources <- append(sources, rep(names(season), each = 7))
      seasons <- append(seasons, rep(names(scenario)[l], n))
      states <- append(states, rep(names(percentchange)[i], n))
      transformation <- append(transformation, rep(names(datasetsnrmse)[d], n))
    }
  }
}

plotdata <- data.frame(nrmse, models, sources, seasons, states, transformation)

p1 <- ggplot(plotdata[which(plotdata$seasons == 'summer' & plotdata$models == 'meanonly'),]) +
  geom_bar(aes(x = states, y = nrmse, fill= transformation), stat = 'identity', position = 'dodge') +
  theme_light() + xlab('') + ggtitle('Mean-Only') + ylab('NRMSE') +
  scale_fill_manual(name = 'Transformation', values = c('#66c2a5', '#fc8d62','#8da0cb'), labels = c('Capacity-Based', 'None', 'De-Trending')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free')

p2 <- ggplot(plotdata[which(plotdata$seasons == 'summer' & plotdata$models == 'glm'),]) +
  geom_bar(aes(x = states, y = nrmse, fill= transformation), stat = 'identity', position = 'dodge') +
  theme_light() + xlab('') + ggtitle('GLM') + ylab('NRMSE') +
  scale_fill_manual(name = 'Transformation', values = c('#66c2a5', '#fc8d62','#8da0cb'), labels = c('Capacity-Based', 'None', 'De-Trending')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free')

p3 <- ggplot(plotdata[which(plotdata$seasons == 'summer' & plotdata$models == 'mars'),]) +
  geom_bar(aes(x = states, y = nrmse, fill= transformation), stat = 'identity', position = 'dodge') +
  theme_light() + xlab('') + ggtitle('MARS') + ylab('NRMSE') +
  scale_fill_manual(name = 'Transformation', values = c('#66c2a5', '#fc8d62','#8da0cb'), labels = c('Capacity-Based', 'None', 'De-Trending')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free')

p4 <- ggplot(plotdata[which(plotdata$seasons == 'summer' & plotdata$models == 'cart'),]) +
  geom_bar(aes(x = states, y = nrmse, fill= transformation), stat = 'identity', position = 'dodge') +
  theme_light() + xlab('') + ggtitle('Decision Tree') + ylab('NRMSE') +
  scale_fill_manual(name = 'Transformation', values = c('#66c2a5', '#fc8d62','#8da0cb'), labels = c('Capacity-Based', 'None', 'De-Trending')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free')

p5 <- ggplot(plotdata[which(plotdata$seasons == 'summer' & plotdata$models == 'rf'),]) +
  geom_bar(aes(x = states, y = nrmse, fill= transformation), stat = 'identity', position = 'dodge') +
  theme_light() + xlab('') + ggtitle('Random Forest') + ylab('NRMSE') +
  scale_fill_manual(name = 'Transformation', values = c('#66c2a5', '#fc8d62','#8da0cb'), labels = c('Capacity-Based', 'None', 'De-Trending')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free')

p6 <- ggplot(plotdata[which(plotdata$seasons == 'summer' & plotdata$models == 'bart'),]) +
  geom_bar(aes(x = states, y = nrmse, fill= transformation), stat = 'identity', position = 'dodge') +
  theme_light() + xlab('') + ggtitle('BART') + ylab('NRMSE') +
  scale_fill_manual(name = 'Transformation', values = c('#66c2a5', '#fc8d62','#8da0cb'), labels = c('Capacity-Based', 'None', 'De-Trending')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free')

p7 <- ggplot(plotdata[which(plotdata$seasons == 'summer' & plotdata$models == 'svm'),]) +
  geom_bar(aes(x = states, y = nrmse, fill= transformation), stat = 'identity', position = 'dodge') +
  theme_light() + xlab('') + ggtitle('SVM') + ylab('NRMSE') +
  scale_fill_manual(name = 'Transformation', values = c('#66c2a5', '#fc8d62','#8da0cb'), labels = c('Capacity-Based', 'None', 'De-Trending')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free')

setwd(outputdir)
pdf('normalizationNRMSE_summer.pdf', width = 8, height = 22)
plot_grid(p1, p2, p3, p4, p5, p6, p7, nrow = 7)
dev.off()

setwd(outputdir)
pdf('normalizationNRMSE_summer_subset.pdf', width = 8, height = 12)
plot_grid(p3, p5, p6, p7, nrow = 4)
dev.off()

# FIGURE: Comparing Normalization Techniques (NRMSE; Winter Only)

p1 <- ggplot(plotdata[which(plotdata$seasons == 'winter' & plotdata$models == 'meanonly'),]) +
  geom_bar(aes(x = states, y = nrmse, fill= transformation), stat = 'identity', position = 'dodge') +
  theme_light() + xlab('') + ggtitle('Mean-Only') + ylab('NRMSE') +
  scale_fill_manual(name = 'Transformation', values = c('#66c2a5', '#fc8d62','#8da0cb'), labels = c('Capacity-Based', 'None', 'De-Trending')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free')

p2 <- ggplot(plotdata[which(plotdata$seasons == 'winter' & plotdata$models == 'glm'),]) +
  geom_bar(aes(x = states, y = nrmse, fill= transformation), stat = 'identity', position = 'dodge') +
  theme_light() + xlab('') + ggtitle('GLM') + ylab('NRMSE') +
  scale_fill_manual(name = 'Transformation', values = c('#66c2a5', '#fc8d62','#8da0cb'), labels = c('Capacity-Based', 'None', 'De-Trending')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free')

p3 <- ggplot(plotdata[which(plotdata$seasons == 'winter' & plotdata$models == 'mars'),]) +
  geom_bar(aes(x = states, y = nrmse, fill= transformation), stat = 'identity', position = 'dodge') +
  theme_light() + xlab('') + ggtitle('MARS') + ylab('NRMSE') +
  scale_fill_manual(name = 'Transformation', values = c('#66c2a5', '#fc8d62','#8da0cb'), labels = c('Capacity-Based', 'None', 'De-Trending')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free')

p4 <- ggplot(plotdata[which(plotdata$seasons == 'winter' & plotdata$models == 'cart'),]) +
  geom_bar(aes(x = states, y = nrmse, fill= transformation), stat = 'identity', position = 'dodge') +
  theme_light() + xlab('') + ggtitle('Decision Tree') + ylab('NRMSE') +
  scale_fill_manual(name = 'Transformation', values = c('#66c2a5', '#fc8d62','#8da0cb'), labels = c('Capacity-Based', 'None', 'De-Trending')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free')

p5 <- ggplot(plotdata[which(plotdata$seasons == 'winter' & plotdata$models == 'rf'),]) +
  geom_bar(aes(x = states, y = nrmse, fill= transformation), stat = 'identity', position = 'dodge') +
  theme_light() + xlab('') + ggtitle('Random Forest') + ylab('NRMSE') +
  scale_fill_manual(name = 'Transformation', values = c('#66c2a5', '#fc8d62','#8da0cb'), labels = c('Capacity-Based', 'None', 'De-Trending')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free')

p6 <- ggplot(plotdata[which(plotdata$seasons == 'winter' & plotdata$models == 'bart'),]) +
  geom_bar(aes(x = states, y = nrmse, fill= transformation), stat = 'identity', position = 'dodge') +
  theme_light() + xlab('') + ggtitle('BART') + ylab('NRMSE') +
  scale_fill_manual(name = 'Transformation', values = c('#66c2a5', '#fc8d62','#8da0cb'), labels = c('Capacity-Based', 'None', 'De-Trending')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free')

p7 <- ggplot(plotdata[which(plotdata$seasons == 'winter' & plotdata$models == 'svm'),]) +
  geom_bar(aes(x = states, y = nrmse, fill= transformation), stat = 'identity', position = 'dodge') +
  theme_light() + xlab('') + ggtitle('SVM') + ylab('NRMSE') +
  scale_fill_manual(name = 'Transformation', values = c('#66c2a5', '#fc8d62','#8da0cb'), labels = c('Capacity-Based', 'None', 'De-Trending')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free')

setwd(outputdir)
pdf('normalizationNRMSE_winter.pdf', width = 8, height = 22)
plot_grid(p1, p2, p3, p4, p5, p6, p7, nrow = 7)
dev.off()

setwd(outputdir)
pdf('normalizationNRMSE_winter_subset.pdf', width = 8, height = 12)
plot_grid(p3, p5, p6, p7, nrow = 4)
dev.off()

# FIGURE: Comparing VSURF Output

setwd(rdatadir)
load('vsurfexperiment.RData')

states <- c(); gcms <- c(); scenarios <- c(); seasons <- c(); sources <- c(); models <- c(); types <- c(); nrmse <- c()

# loop through all states
for (i in 1:length(allnrmse)) {
  state <- allnrmse[[i]]
  
  # loop through all seasons
  for (l in 1:length(state)) {
    season <- state[[l]]
    
    # loop through all sources
    for (m in 1:length(season)) {
      vsurfruns <- season[[m]]
      
      # loop through run types
      for (t in 1:length(vsurfruns)) {
        nrmse <- append(nrmse, vsurfruns[,t])
        types <- append(types, rep(names(vsurfruns)[t], 10))
      }
    }
    # store names of labels
    n <- length(season)*30 # number of sources * 3 run types * 10 repetitions
    sources <- append(sources, rep(names(season), each = 30))
    seasons <- append(seasons, rep(names(state)[l], n))
    states <- append(states, rep(names(allnrmse)[i], n))
  }
}

unlisteddata <- data.frame(states, seasons, sources, types, nrmse)

plotdata <- unlisteddata %>% group_by(states, sources, types, seasons) %>% summarise(mins = min(nrmse), maxs = max(nrmse), means = mean(nrmse))

p1 <- ggplot(plotdata[which(plotdata$seasons == 'summer'),], aes(x = states, ymin = mins, ymax = maxs, lower = mins, upper = maxs, middle = means)) + 
  geom_boxplot(aes(fill = types), stat = 'identity') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Run Type', values = c('#66c2a5', '#fc8d62','#8da0cb'), labels = c('Interpretation', 'Prediction', 'Threshold')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('NRMSE') + ggtitle('Summer')

p2 <- ggplot(plotdata[which(plotdata$seasons == 'winter'),], aes(x = states, ymin = mins, ymax = maxs, lower = mins, upper = maxs, middle = means)) + 
  geom_boxplot(aes(fill = types), stat = 'identity') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Run Type', values = c('#66c2a5', '#fc8d62','#8da0cb'), labels = c('Interpretation', 'Prediction', 'Threshold')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('NRMSE') + ggtitle('Winter')

setwd(outputdir)
pdf('vsurfcomparison_boxplots.pdf', width = 11, height = 9)
plot_grid(p1, p2, nrow = 2)
dev.off()

p1 <- ggplot(plotdata[which(plotdata$seasons == 'summer'),]) + 
  geom_bar(aes(x = states, y = means, fill = types), stat = 'identity', position = 'dodge') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Run Type', values = c('#66c2a5', '#fc8d62','#8da0cb'), labels = c('Interpretation', 'Prediction', 'Threshold')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('NRMSE') + ggtitle('Summer')

p2 <- ggplot(plotdata[which(plotdata$seasons == 'winter'),]) + 
  geom_bar(aes(x = states, y = means, fill = types), stat = 'identity', position = 'dodge') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Run Type', values = c('#66c2a5', '#fc8d62','#8da0cb'), labels = c('Interpretation', 'Prediction', 'Threshold')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('NRMSE') + ggtitle('Winter')

setwd(outputdir)
pdf('vsurfcomparison_barplots.pdf', width = 11, height = 9)
plot_grid(p1, p2, nrow = 2)
dev.off()

# FIGURE: Comparing Different Ensembles

setwd(rdatadir)
load('ensembleexperiments.RData')

states <- c(); gcms <- c(); scenarios <- c(); seasons <- c(); sources <- c(); models <- c(); types = c(); ensembles <- c(); nrmse <- c()

# loop through all states
for (i in 1:length(allensemblenrmse)) {
  state <- allensemblenrmse[[i]]
  
  # loop through all seasons
  for (l in 1:length(state)) {
    season <- state[[l]]
    
    # loop through all sources
    for (m in 1:length(season)) {
      source <- season[[m]]
      
      # loop through run types
      for (t in 1:length(source)) {
        type <- source[[t]]
        
        nrmse <- append(nrmse, unlist(type))
        ensembles <- append(ensembles, names(type))
      }
      types <- append(types, rep(names(source), each = 6))
    }
    # store names of labels
    n <- length(season)*12 # number of sources * 2 run types * 6 ensembles
    sources <- append(sources, rep(names(season), each = 12))
    seasons <- append(seasons, rep(names(state)[l], n))
    states <- append(states, rep(names(allensemblenrmse)[i], n))
  }
}

plotdata <- data.frame(states, seasons, sources, types, ensembles, nrmse)

p1 <- ggplot(plotdata[which(plotdata$seasons == 'summer' & plotdata$types == 'unweighted'),]) + 
  geom_bar(aes(x = states, y = nrmse, fill = ensembles), stat = 'identity', position = 'dodge') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Ensembles', values = c('#66c2a5', '#fc8d62','#8da0cb','#b3b3b3', '#e5c494','#ffd92f')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('NRMSE') + ggtitle('Unweighted Ensembles')

p2 <- ggplot(plotdata[which(plotdata$seasons == 'summer' & plotdata$types == 'weighted'),]) + 
  geom_bar(aes(x = states, y = nrmse, fill = ensembles), stat = 'identity', position = 'dodge') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Ensembles', values = c('#66c2a5', '#fc8d62','#8da0cb','#b3b3b3', '#e5c494','#ffd92f')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('NRMSE') + ggtitle('Weighted Ensembles')

setwd(outputdir)
pdf('ensemblecomparison_summer.pdf', width = 13, height = 9)
plot_grid(p1, p2, nrow = 2)
dev.off()

p1 <- ggplot(plotdata[which(plotdata$seasons == 'winter' & plotdata$types == 'unweighted'),]) + 
  geom_bar(aes(x = states, y = nrmse, fill = ensembles), stat = 'identity', position = 'dodge') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Ensembles', values = c('#66c2a5', '#fc8d62','#8da0cb','#b3b3b3', '#e5c494','#ffd92f')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('NRMSE') + ggtitle('Unweighted Ensembles')

p2 <- ggplot(plotdata[which(plotdata$seasons == 'winter' & plotdata$types == 'weighted'),]) + 
  geom_bar(aes(x = states, y = nrmse, fill = ensembles), stat = 'identity', position = 'dodge') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Ensembles', values = c('#66c2a5', '#fc8d62','#8da0cb','#b3b3b3', '#e5c494','#ffd92f')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('NRMSE') + ggtitle('Weighted Ensembles')

setwd(outputdir)
pdf('ensemblecomparison_winter.pdf', width = 13, height = 9)
plot_grid(p1, p2, nrow = 2)
dev.off()

p1 <- ggplot(plotdata[which(plotdata$seasons == 'summer' & plotdata$types == 'weighted'),]) + 
  geom_bar(aes(x = states, y = nrmse, fill = ensembles), stat = 'identity', position = 'dodge') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Ensembles', values = c('#66c2a5', '#fc8d62','#8da0cb','#b3b3b3', '#e5c494','#ffd92f')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('NRMSE') + ggtitle('Summer')

p2 <- ggplot(plotdata[which(plotdata$seasons == 'winter' & plotdata$types == 'weighted'),]) + 
  geom_bar(aes(x = states, y = nrmse, fill = ensembles), stat = 'identity', position = 'dodge') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Ensembles', values = c('#66c2a5', '#fc8d62','#8da0cb','#b3b3b3', '#e5c494','#ffd92f')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('NRMSE') + ggtitle('Winter')

setwd(outputdir)
pdf('ensemblecomparison_weighted.pdf', width = 13, height = 9)
plot_grid(p1, p2, nrow = 2)
dev.off()

p1 <- ggplot(plotdata[which(plotdata$seasons == 'summer' & plotdata$types == 'unweighted'),]) + 
  geom_bar(aes(x = states, y = nrmse, fill = ensembles), stat = 'identity', position = 'dodge') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Ensembles', values = c('#66c2a5', '#fc8d62','#8da0cb','#b3b3b3', '#e5c494','#ffd92f')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('NRMSE') + ggtitle('Summer')

p2 <- ggplot(plotdata[which(plotdata$seasons == 'winter' & plotdata$types == 'unweighted'),]) + 
  geom_bar(aes(x = states, y = nrmse, fill = ensembles), stat = 'identity', position = 'dodge') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Ensembles', values = c('#66c2a5', '#fc8d62','#8da0cb','#b3b3b3', '#e5c494','#ffd92f')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('NRMSE') + ggtitle('Winter')

setwd(outputdir)
pdf('ensemblecomparison_unweighted.pdf', width = 13, height = 9)
plot_grid(p1, p2, nrow = 2)
dev.off()

# FIGURE: Comparison of results with and without net radiation

setwd(rdatadir)
load('radiationexperiment.RData')

nrmse <- c(unlist(allnrmse$CA$summer$Hydropower), unlist(allnrmse$CA$summer$Solar), unlist(allnrmse$CA$summer$Wind),
           unlist(allnrmse$NY$summer$Hydropower), unlist(allnrmse$NY$summer$Solar), unlist(allnrmse$NY$summer$Wind),
           unlist(allnrmse$FL$summer$Hydropower), unlist(allnrmse$FL$summer$Solar),
           unlist(allnrmse$GA$summer$Hydropower), unlist(allnrmse$GA$summer$Solar),
           unlist(allnrmse$CA$winter$Hydropower), unlist(allnrmse$CA$winter$Solar), unlist(allnrmse$CA$winter$Wind),
           unlist(allnrmse$NY$winter$Hydropower), unlist(allnrmse$NY$winter$Solar), unlist(allnrmse$NY$winter$Wind),
           unlist(allnrmse$FL$winter$Hydropower), unlist(allnrmse$FL$winter$Solar),
           unlist(allnrmse$GA$winter$Hydropower), unlist(allnrmse$GA$winter$Solar),
           unlist(allradnrmse$CA$summer$Hydropower), unlist(allradnrmse$CA$summer$Solar), unlist(allradnrmse$CA$summer$Wind),
           unlist(allradnrmse$NY$summer$Hydropower), unlist(allradnrmse$NY$summer$Solar), unlist(allradnrmse$NY$summer$Wind),
           unlist(allradnrmse$FL$summer$Hydropower), unlist(allradnrmse$FL$summer$Solar),
           unlist(allradnrmse$GA$summer$Hydropower), unlist(allradnrmse$GA$summer$Solar),
           unlist(allradnrmse$CA$winter$Hydropower), unlist(allradnrmse$CA$winter$Solar), unlist(allradnrmse$CA$winter$Wind),
           unlist(allradnrmse$NY$winter$Hydropower), unlist(allradnrmse$NY$winter$Solar), unlist(allradnrmse$NY$winter$Wind),
           unlist(allradnrmse$FL$winter$Hydropower), unlist(allradnrmse$FL$winter$Solar),
           unlist(allradnrmse$GA$winter$Hydropower), unlist(allradnrmse$GA$winter$Solar))

models <- rep(c('Mean-Only', 'GLM', 'MARS', 'CART', 'RF', 'BART', 'SVM'), 40)
states <- c(rep('CA', 7*3), rep('NY', 7*3), rep('FL', 7*2), rep('GA', 7*2), rep('CA', 7*3), rep('NY', 7*3), rep('FL', 7*2), rep('GA', 7*2),
            rep('CA', 7*3), rep('NY', 7*3), rep('FL', 7*2), rep('GA', 7*2), rep('CA', 7*3), rep('NY', 7*3), rep('FL', 7*2), rep('GA', 7*2))
sources <- c(rep('Hydropower',7), rep('Solar', 7), rep('Wind', 7), rep('Hydropower', 7), rep('Solar', 7), rep('Wind', 7), rep('Hydropower', 7), rep('Solar', 7), rep('Hydropower', 7), rep('Solar', 7),
             rep('Hydropower',7), rep('Solar', 7), rep('Wind', 7), rep('Hydropower', 7), rep('Solar', 7), rep('Wind', 7), rep('Hydropower', 7), rep('Solar', 7), rep('Hydropower', 7), rep('Solar', 7),
             rep('Hydropower',7), rep('Solar', 7), rep('Wind', 7), rep('Hydropower', 7), rep('Solar', 7), rep('Wind', 7), rep('Hydropower', 7), rep('Solar', 7), rep('Hydropower', 7), rep('Solar', 7),
             rep('Hydropower',7), rep('Solar', 7), rep('Wind', 7), rep('Hydropower', 7), rep('Solar', 7), rep('Wind', 7), rep('Hydropower', 7), rep('Solar', 7), rep('Hydropower', 7), rep('Solar', 7))
seasons <- c(rep('Summer', 70), rep('Winter', 70), rep('Summer', 70), rep('Winter', 70))
radiation <- c(rep('No', 140), rep('Yes', 140))

plotdata <- data.frame(nrmse, models, states, sources, seasons, radiation)

p1 <- ggplot(plotdata[which(plotdata$seasons == 'Summer' & plotdata$models == 'GLM'),]) + 
  geom_bar(aes(x = states, y = nrmse, fill = radiation), stat = 'identity', position = 'dodge') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Radiation Included?', values = c('#66c2a5', '#fc8d62')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('NRMSE') +
  ggtitle('GLM')

p2 <- ggplot(plotdata[which(plotdata$seasons == 'Summer' & plotdata$models == 'MARS'),]) + 
  geom_bar(aes(x = states, y = nrmse, fill = radiation), stat = 'identity', position = 'dodge') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Radiation Included?', values = c('#66c2a5', '#fc8d62')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('NRMSE') +
  ggtitle('MARS')

p3 <- ggplot(plotdata[which(plotdata$seasons == 'Summer' & plotdata$models == 'CART'),]) + 
  geom_bar(aes(x = states, y = nrmse, fill = radiation), stat = 'identity', position = 'dodge') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Radiation Included?', values = c('#66c2a5', '#fc8d62')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('NRMSE') +
  ggtitle('Decision Tree')

p4 <- ggplot(plotdata[which(plotdata$seasons == 'Summer' & plotdata$models == 'RF'),]) + 
  geom_bar(aes(x = states, y = nrmse, fill = radiation), stat = 'identity', position = 'dodge') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Radiation Included?', values = c('#66c2a5', '#fc8d62')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('NRMSE') +
  ggtitle('Random Forest')

p5 <- ggplot(plotdata[which(plotdata$seasons == 'Summer' & plotdata$models == 'BART'),]) + 
  geom_bar(aes(x = states, y = nrmse, fill = radiation), stat = 'identity', position = 'dodge') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Radiation Included?', values = c('#66c2a5', '#fc8d62')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('NRMSE') +
  ggtitle('BART')

p6 <- ggplot(plotdata[which(plotdata$seasons == 'Summer' & plotdata$models == 'SVM'),]) + 
  geom_bar(aes(x = states, y = nrmse, fill = radiation), stat = 'identity', position = 'dodge') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Radiation Included?', values = c('#66c2a5', '#fc8d62')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('NRMSE') +
  ggtitle('SVM')

setwd(outputdir)
pdf('radiationcomparison_summer.pdf', width = 8, height = 18)
plot_grid(p1, p2, p3, p4, p5, p6, nrow = 6)
dev.off()

setwd(outputdir)
pdf('radiationcomparison_summer_subset.pdf', width = 8, height = 12)
plot_grid(p2, p4, p5, p6, nrow = 4)
dev.off()

p1 <- ggplot(plotdata[which(plotdata$seasons == 'Winter' & plotdata$models == 'GLM'),]) + 
  geom_bar(aes(x = states, y = nrmse, fill = radiation), stat = 'identity', position = 'dodge') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Radiation Included?', values = c('#66c2a5', '#fc8d62')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('NRMSE') +
  ggtitle('GLM')

p2 <- ggplot(plotdata[which(plotdata$seasons == 'Winter' & plotdata$models == 'MARS'),]) + 
  geom_bar(aes(x = states, y = nrmse, fill = radiation), stat = 'identity', position = 'dodge') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Radiation Included?', values = c('#66c2a5', '#fc8d62')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('NRMSE') +
  ggtitle('MARS')

p3 <- ggplot(plotdata[which(plotdata$seasons == 'Winter' & plotdata$models == 'CART'),]) + 
  geom_bar(aes(x = states, y = nrmse, fill = radiation), stat = 'identity', position = 'dodge') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Radiation Included?', values = c('#66c2a5', '#fc8d62')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('NRMSE') +
  ggtitle('Decision Tree')

p4 <- ggplot(plotdata[which(plotdata$seasons == 'Winter' & plotdata$models == 'RF'),]) + 
  geom_bar(aes(x = states, y = nrmse, fill = radiation), stat = 'identity', position = 'dodge') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Radiation Included?', values = c('#66c2a5', '#fc8d62')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('NRMSE') +
  ggtitle('Random Forest')

p5 <- ggplot(plotdata[which(plotdata$seasons == 'Winter' & plotdata$models == 'BART'),]) + 
  geom_bar(aes(x = states, y = nrmse, fill = radiation), stat = 'identity', position = 'dodge') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Radiation Included?', values = c('#66c2a5', '#fc8d62')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('NRMSE') +
  ggtitle('BART')

p6 <- ggplot(plotdata[which(plotdata$seasons == 'Winter' & plotdata$models == 'SVM'),]) + 
  geom_bar(aes(x = states, y = nrmse, fill = radiation), stat = 'identity', position = 'dodge') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Radiation Included?', values = c('#66c2a5', '#fc8d62')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + ylab('NRMSE') +
  ggtitle('SVM')

setwd(outputdir)
pdf('radiationcomparison_winter.pdf', width = 8, height = 18)
plot_grid(p1, p2, p3, p4, p5, p6, nrow = 6)
dev.off()

setwd(outputdir)
pdf('radiationcomparison_winter_subset.pdf', width = 8, height = 12)
plot_grid(p2, p4, p5, p6, nrow = 4)
dev.off()

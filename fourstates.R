# Project: Predicting Renewable Energy Generation in the US based on Climate Data (States: CA, NY, FL, and GA)
# Author: Renee Obringer
# Last Ran: 01 October 2024

# ORGANIZATION: 
# This code is organized into sections, the start of each is denoted by multiple #
# The sections can be run independently by loading the rdata files at the beginning of each section
# Each section is described below
#
# LOAD DATA: load data
# DATA PRE-PROCESSING: pre-processing to integrate generation and weather data
# MODEL RUNS: conduct variable selection & run all models for all sources across the four states
# INTERPRETATION: analyze model performance
# COMPOSITE MODEL PERFORMANCE: evaluate the performance of an ensemble of SVM, RF, and BART models
# FIGURES AND TABLES: code for plotting figures and creating tables included in manuscript 

rm(list=ls())
options(scipen = 999)

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

# set main directory path
# NOTE: set this path to the folder on your personal machine which contains the downloaded data and code
# for example: path <- '/Users/rqo5125/Downloads/ClimateAnalogs_WEN-main'

path <- '    ' # primary directory path

# set directories
datadir1 <- paste(path, '/EIAdata', sep = '')     # contains the renewable energy data from EIA
datadir2 <- paste(path, '/NARRdata', sep = '')    # contains the climate data from NARR
datadir3 <- paste(path, '/CensusData', sep = '')  # contains the population data from the US Census Bureau
rdatadir <- paste(path, '/rdatafiles', sep = '')  # contains the rdata files
  
# OPTIONAL: create an output directory for any non-rdata output files (e.g., csv, pdf, etc.)
outputdir <- paste(path, '/outputdir', sep = '')
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

# EIA Data
setwd(datadir1)

gendata <- c()
for (i in 1:16) {
  sheetdata <- read_excel(paste(datadir1, "/monthlygenerationdata.xlsx", sep = ""), sheet = i)
  names(sheetdata) <- c('Year', 'Month', 'State', 'TypeOfProducer', 'EnergySource','Generation_MWh')
  gendata <- rbind(gendata, sheetdata)
}

# Population Data
setwd(datadir3)

pop2000_10 <- read.csv('st-est00int-01.csv')
pop2010_20 <- read.csv('nst-est2020.csv')
pop2020_23 <- read.csv('NST-EST2023-POP.csv')

################ DATA PRE-PROCESSING #################

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
                   narrdata[[5]][,which(names(narrdata[[5]]) == 'ID_06')]); names(CAweather)[4:8] <- narrdatanames

NYweather <- cbind(narrdata[[1]][,c(1,2,3,which(names(narrdata[[1]]) == 'ID_36'))], narrdata[[2]][,which(names(narrdata[[2]]) == 'ID_36')], 
                   narrdata[[3]][,which(names(narrdata[[3]]) == 'ID_36')], narrdata[[4]][,which(names(narrdata[[4]]) == 'ID_36')], 
                   narrdata[[5]][,which(names(narrdata[[5]]) == 'ID_36')]); names(NYweather)[4:8] <- narrdatanames

FLweather <- cbind(narrdata[[1]][,c(1,2,3,which(names(narrdata[[1]]) == 'ID_12'))], narrdata[[2]][,which(names(narrdata[[2]]) == 'ID_12')], 
                   narrdata[[3]][,which(names(narrdata[[3]]) == 'ID_12')], narrdata[[4]][,which(names(narrdata[[4]]) == 'ID_12')], 
                   narrdata[[5]][,which(names(narrdata[[5]]) == 'ID_12')]); names(FLweather)[4:8] <- narrdatanames

GAweather <- cbind(narrdata[[1]][,c(1,2,3,which(names(narrdata[[1]]) == 'ID_13'))], narrdata[[2]][,which(names(narrdata[[2]]) == 'ID_13')], 
                   narrdata[[3]][,which(names(narrdata[[3]]) == 'ID_13')], narrdata[[4]][,which(names(narrdata[[4]]) == 'ID_13')], 
                   narrdata[[5]][,which(names(narrdata[[5]]) == 'ID_13')]); names(GAweather)[4:8] <- narrdatanames

stateNARRdata <- list(CAweather, NYweather, FLweather, GAweather)

# aggregate to monthly values
monthlyNARRdata <- list()

for (i in 1:4) {
  state <- stateNARRdata[[i]]
  
  # calculate wind speed
  state$windspeed <- sqrt(state$daily_uwind10m_2000_2022^2 + state$daily_vwind10m_2000_2022^2)
  
  # convert temperature units
  state$daily_tavg_2000_2022 <- state$daily_tavg_2000_2022 - 273.15
  
  # aggregate to monthlyh values
  monthlyvalues <- state %>% group_by(Year, Month) %>% 
    summarize(min_rh = min(daily_rh_2000_2022), max_rh = max(daily_rh_2000_2022), avg_rh = mean(daily_rh_2000_2022), 
              min_temp = min(daily_tavg_2000_2022), max_temp = max(daily_tavg_2000_2022), avg_temp = mean(daily_tavg_2000_2022), 
              min_ws = min(windspeed), max_ws = max(windspeed), avg_ws = mean(windspeed), total_precip = sum(daily_precip_2000_2022), 
              min_precip = min(daily_precip_2000_2022), max_precip = max(daily_precip_2000_2022), avg_precip = mean(daily_precip_2000_2022)) 
  
  # store
  monthlyNARRdata[[i]] <- monthlyvalues
}

# combine generation with NARR data
alldata <- list()

for (i in 1:4) {
  alldata[[i]] <- merge(monthlyNARRdata[[i]], stateGENdata[[i]], by = c('Year','Month'))
}

setwd(rdatadir)
save(alldata, stateGENdata, monthlyNARRdata, file = "cleaneddata.RData")

################ MODEL RUNS ##########################

setwd(rdatadir)
load('cleaneddata.RData')

# initialize variables
allrsq <- list(); allrmse <- list(); allnrmse <- list(); allmodels <- list(); allyhat <- list(); allselecteddata <- list(); allvarimpvalues <- list(); allsvmpd <- list()

# loop through each state
for (s in 1:4) {
  state <- alldata[[s]]
  
  # initialize variables
  sourcersq <- list(); sourcermse <- list(); sourcenrmse <- list(); sourcemodels <- list(); sourceyhat <- list(); selecteddata <- list(); varimpvalues <- list(); svmpd <- list()
  
  # loop through each source
  for (g in 1:3) {
    source <- state[,c(g+15, 3:15)]
    
    # remove NA values
    source <- na.omit(source)
    
    # implement VSURF for variable selection
    vsurf_obj <- VSURF(x = source[,2:length(source)], y = source[,1])
    
    # store variables
    if (length(vsurf_obj$varselect.pred) > 0) {
      updatedsource <- source[,c(1, 1 + vsurf_obj$varselect.pred)]
    } else {
      updatedsource <- source[,c(1, 1 + vsurf_obj$varselect.interp)]
    }
    
    # set up 5-fold cross validation with selected variables
    k <- 5
    n <- nrow(updatedsource)
    set.seed(11)
    updatedsource <- updatedsource[sample(n),]
    folds <- cut(seq(1,n),breaks = k, labels = FALSE)
    
    # initialize R^2, RMSE, NRSME, yhat, and test data lists
    cvrsq <- list(); cvrmse <- list(); cvnrmse <- list()
    glmyhat_all <- c(); marsyhat_all <- c(); cartyhat_all <- c(); rfyhat_all <- c()
    bartyhat_all <- c(); svmyhat_all <- c(); testdata_all <- c()
    
    # run models
    for (j in 1:k) {
      # split into training and test dataset
      testIndex <- which(folds == j, arr.ind = TRUE)
      testData <- updatedsource[testIndex,]
      trainData <- updatedsource[-testIndex,]
      
      # create formula
      myformula <- paste(names(trainData)[1], "~", paste(names(trainData[2:length(trainData)]), collapse="+"))
      myformula <- as.formula(myformula)
      
      # run models
      glmmodel <- glm(formula = myformula, data = trainData); marsmodel <- earth(formula = myformula, data = trainData)
      cartmodel <- rpart(formula = myformula, data = trainData); rfmodel <- randomForest(formula = myformula, data = trainData)
      bartmodel <- bartMachine(X = trainData[,2:length(trainData)], y = trainData[,1])
      svmmodel <- svm(formula = myformula, data = trainData)
      
      # fit models
      glmyhat <- predict(glmmodel, newdata = testData); marsyhat <- predict(marsmodel, newdata = testData)
      cartyhat <- predict(cartmodel, newdata = testData); rfyhat <- predict(rfmodel, newdata = testData)
      bartyhat <- predict(bartmodel, new_data = testData[,2:length(testData)])
      svmyhat <- predict(svmmodel, newdata = testData)
      
      # store yhat 
      glmyhat_all <- append(glmyhat_all, glmyhat); marsyhat_all <- append(marsyhat_all, marsyhat)
      cartyhat_all <- append(cartyhat_all, cartyhat); rfyhat_all <- append(rfyhat_all, rfyhat)
      bartyhat_all <- append(bartyhat_all, bartyhat); svmyhat_all <- append(svmyhat_all, svmyhat)
      
      # store test data 
      testdata_all <- append(testdata_all, testData[,1])
      
      # measures of error
      cvrsq[["glm"]][j] <- cor(glmyhat, testData[,1])^2
      cvrsq[["mars"]][j] <- cor(marsyhat, testData[,1])^2
      cvrsq[["cart"]][j] <- cor(cartyhat, testData[,1])^2
      cvrsq[["rf"]][j] <- cor(rfyhat, testData[,1])^2
      cvrsq[["bart"]][j] <- cor(bartyhat, testData[,1])^2
      cvrsq[["svm"]][j] <- cor(svmyhat, testData[,1])^2
      
      cvrmse[["glm"]][j] <- sqrt(sum((glmyhat-testData[,1])^2)/nrow(testData))
      cvrmse[["mars"]][j] <- sqrt(sum((marsyhat-testData[,1])^2)/nrow(testData))
      cvrmse[["cart"]][j] <- sqrt(sum((cartyhat-testData[,1])^2)/nrow(testData))
      cvrmse[["rf"]][j] <- sqrt(sum((rfyhat-testData[,1])^2)/nrow(testData))
      cvrmse[["bart"]][j] <- sqrt(sum((bartyhat-testData[,1])^2)/nrow(testData))
      cvrmse[["svm"]][j] <- sqrt(sum((svmyhat-testData[,1])^2)/nrow(testData))
      
      cvnrmse[["glm"]][j] <- sqrt(sum((glmyhat-testData[,1])^2)/nrow(testData))/(range(testData[,1])[2]-range(testData[,1])[1])
      cvnrmse[["mars"]][j] <- sqrt(sum((marsyhat-testData[,1])^2)/nrow(testData))/(range(testData[,1])[2]-range(testData[,1])[1])
      cvnrmse[["cart"]][j] <- sqrt(sum((cartyhat-testData[,1])^2)/nrow(testData))/(range(testData[,1])[2]-range(testData[,1])[1])
      cvnrmse[["rf"]][j] <- sqrt(sum((rfyhat-testData[,1])^2)/nrow(testData))/(range(testData[,1])[2]-range(testData[,1])[1])
      cvnrmse[["bart"]][j] <- sqrt(sum((bartyhat-testData[,1])^2)/nrow(testData))/(range(testData[,1])[2]-range(testData[,1])[1])
      cvnrmse[["svm"]][j] <- sqrt(sum((svmyhat-testData[,1])^2)/nrow(testData))/(range(testData[,1])[2]-range(testData[,1])[1])
    }
    
    # store source-specific data
    varname <- names(updatedsource)[1]
    
    sourcersq[[varname]] <- cvrsq
    sourcermse[[varname]] <- cvrmse
    sourcenrmse[[varname]] <- cvnrmse
    sourcemodels[[varname]] <- list(glmmodel, marsmodel, cartmodel, rfmodel, bartmodel, svmmodel)
    sourceyhat[[varname]] <- data.frame("glm" = glmyhat_all, "mars" = marsyhat_all, "cart" = cartyhat_all, "rf" = rfyhat_all, "bart" = bartyhat_all, "svm" = svmyhat_all, "testData" = testdata_all)
    selecteddata[[varname]] <- updatedsource
    varimpvalues[[varname]] <- data.frame("name" = names(source)[1 + vsurf_obj$imp.mean.dec.ind], "value" = vsurf_obj$imp.mean.dec)
    
    pdvalues <- list()
    for (p in 2:length(updatedsource)) {
      pdvalues[[names(updatedsource)[p]]] <- partial(svmmodel, pred.var = names(updatedsource)[p])
    }
    
    svmpd[[varname]] <- pdvalues
    
    # account for some state without all three sources
    if (length(state) == 17 & g == 2) {
      break
    }
  }
  
  # store state-specific data
  statenames <- c('CA', 'NY', 'FL', 'GA')
  
  allrsq[[statenames[s]]] <- sourcersq
  allrmse[[statenames[s]]] <- sourcermse
  allnrmse[[statenames[s]]] <- sourcenrmse
  allmodels[[statenames[s]]] <- sourcemodels
  allyhat[[statenames[s]]] <- sourceyhat
  allselecteddata[[statenames[s]]] <- selecteddata
  allvarimpvalues[[statenames[s]]] <- varimpvalues
  allsvmpd[[statenames[s]]] <- svmpd
}

setwd(rdatadir)
save(alldata, allselecteddata, allvarimpvalues, allsvmpd, allrsq, allrmse, allnrmse, allmodels, allyhat, file = "modelrunresults.RData")


################ INTERPRETATION ######################
setwd(rdatadir)
load('modelrunresults.RData')

# Measures of Error

# California 
unlist(lapply(allnrmse[['CA']][['Hydropower']], mean)) 
unlist(lapply(allnrmse[['CA']][['Solar']], mean)) 
unlist(lapply(allnrmse[['CA']][['Wind']], mean)) 

unlist(lapply(allrsq[['CA']][['Hydropower']], mean)) 
unlist(lapply(allrsq[['CA']][['Solar']], mean)) 
unlist(lapply(allrsq[['CA']][['Wind']], mean)) 

# New York 
unlist(lapply(allnrmse[['NY']][['Hydropower']], mean)) 
unlist(lapply(allnrmse[['NY']][['Solar']], mean)) 
unlist(lapply(allnrmse[['NY']][['Wind']], mean)) 

unlist(lapply(allrsq[['NY']][['Hydropower']], mean)) 
unlist(lapply(allrsq[['NY']][['Solar']], mean)) 
unlist(lapply(allrsq[['NY']][['Wind']], mean))

# Florida
unlist(lapply(allnrmse[['FL']][['Hydropower']], mean)) 
unlist(lapply(allnrmse[['FL']][['Solar']], mean))

unlist(lapply(allrsq[['FL']][['Hydropower']], mean)) 
unlist(lapply(allrsq[['FL']][['Solar']], mean))

# Georgia
unlist(lapply(allnrmse[['GA']][['Hydropower']], mean)) 
unlist(lapply(allnrmse[['GA']][['Solar']], mean))

unlist(lapply(allrsq[['GA']][['Hydropower']], mean)) 
unlist(lapply(allrsq[['GA']][['Solar']], mean))

# t-tests

t.test(allnrmse[['NY']][['Hydropower']]$svm, allnrmse[['NY']][['Hydropower']]$rf)
t.test(allnrmse[['NY']][['Wind']]$svm, allnrmse[['NY']][['Wind']]$glm)
t.test(allnrmse[['GA']][['Hydropower']]$svm, allnrmse[['GA']][['Hydropower']]$glm)

t.test(allrsq[['CA']][['Hydropower']]$svm, allrsq[['CA']][['Hydropower']]$rf)
t.test(allrsq[['NY']][['Hydropower']]$svm, allrsq[['NY']][['Hydropower']]$mars)
t.test(allrsq[['FL']][['Hydropower']]$svm, allrsq[['FL']][['Hydropower']]$mars)
t.test(allrsq[['FL']][['Solar']]$svm, allrsq[['FL']][['Solar']]$bart)
t.test(allrsq[['GA']][['Hydropower']]$svm, allrsq[['GA']][['Hydropower']]$glm)

################ COMPOSITE MODEL PERFORMANCE ######################

setwd(rdatadir)
load('modelrunresults.RData')

allensemblenrmse <- list(); allensemblemean <- list()

# loop through each state
for (i in 1:length(allyhat)) {
  state <- allyhat[[i]]
  
  ensemblenrmse <- c(); ensemblemean <- list()
  
  # loop through each source
  for (j in 1:length(state)) {
    source <- state[[j]]
    
    ensembledata <- data.frame('rf' = source$rf, 'bart' = source$bart, 'svm' = source$svm)
    ensemblemean[[j]] <- rowMeans(ensembledata)
    
    ensemblenrmse[j] <- sqrt(sum((ensemblemean[[j]]-source$testData)^2)/nrow(source))/(range(source$testData)[2]-range(source$testData)[1])
  }
  allensemblenrmse[[i]] <- ensemblenrmse
  allensemblemean[[i]] <- ensemblemean
}

setwd(rdatadir)
save(allensemblenrmse, allensemblemean, file = "ensembleresults.RData")


################ FIGURES & TABLES ####################

setwd(rdatadir)
load('modelrunresults.RData')
load('ensembleresults.RData')

# FIGURE 2: Variable Selection Results 

variables <- rep(c('RH - Min', 'RH - Max', 'RH - Mean', 'Temp - Min', 'Temp - Max', 'Temp - Mean', 'WS - Min',
                   'WS - Max', 'WS - Mean', 'Precip - Total', 'Precip - Min', 'Precip - Max', 'Precip - Mean'), 10)

selectedvariables <- c(1, NA, NA, NA, 1, 1, NA, NA, 1, NA, NA, NA, NA, # CA Hydro
                       1, 1, 1, 1, 1, 1, 1, 1, 1, 1, NA, 1, 1, # CA Solar
                       1, NA, NA, 1, 1, 1, 1, NA, 1, NA, NA, 1, NA, # CA Wind
                       NA, NA, 1, 1, 1, 1, NA, NA, NA, 1, NA, NA, NA, # NY Hydro
                       1, 1, 1, NA, 1, 1, NA, NA, 1, NA, NA, NA, NA, # NY Solar
                       NA, NA, NA, NA, 1, 1, NA, NA, 1, NA, NA, NA, NA, # NY Wind
                       1, NA, 1, NA, 1, 1, NA, NA, 1, 1, NA, NA, NA, # FL Hydro
                       NA, 1, 1, 1, NA, 1, NA, NA, 1, NA, NA, NA, NA, # FL Solar
                       NA, 1, 1, 1, 1, 1, NA, NA, 1, 1, NA, 1, 1, # GA Hydro
                       NA, 1, NA, 1, NA, 1, NA, NA, NA, NA, NA, NA, NA) # GA Solar

states <- c(rep('CA', 13*3), rep('NY', 13*3), rep('FL', 13*2), rep('GA', 13*2))
sources <- c(rep('Hydropower',13), rep('Solar', 13), rep('Wind', 13), rep('Hydropower',13), rep('Solar', 13), rep('Wind', 13),
             rep('Hydropower',13), rep('Solar', 13),rep('Hydropower',13), rep('Solar', 13))

plotdata <- data.frame(variables, selectedvariables, states, sources)

setwd(outputdir)
pdf('figure2.pdf', width = 7, height = 4)
ggplot(plotdata) + geom_point(aes(x = states, y = variables, size = selectedvariables), pch = 21, fill = '#386cb0', color = '#386cb0') + theme_light() + 
  xlab('') + ylab('Predictor Variable') + theme(text = element_text(size=20)) +
  facet_wrap(~sources) + scale_size_continuous(guide = 'none')
dev.off()

# FIGURE 3: Actual vs. Predicted - SVM Only

predicted <- c(); actual <-c(); sources <- c(); states <- c(); models <- c()

# loop through all states
for (i in 1:length(allyhat)) {
  state <- allyhat[[i]]
  
  # loop through all sources
  for (j in 1:length(state)) {
    source <- state[[j]]
    
    # loop through all models
    for (k in 1:6) {
      predicted <- append(predicted, source[,k])
      models <- append(models, rep(names(source)[k], nrow(source)))
      actual <- append(actual, source$testData)
    }
    
    ndatapoints <- nrow(source)
    
    sources <- append(sources, rep(names(state)[j], ndatapoints*6))
    states <- append(states, rep(names(allyhat)[i], ndatapoints*6))
  }
}

plotdata <- data.frame(predicted, actual, sources, states, models)

setwd(outputdir)
pdf('figure3.pdf', width = 10, height = 4)
ggplot(plotdata[which(plotdata$models == 'svm'),]) +
  geom_point(aes(x = actual, y = predicted, color = states)) + geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + 
  scale_x_continuous(breaks = breaks_pretty()) + theme_light() + theme(text = element_text(size = 20)) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'State', values = c('#66c2a5', '#fc8d62','#8da0cb', '#e5c494')) +
  ylab('Predicted Values') + xlab('Actual Values')
dev.off()

# FIGURE 4: Partial Dependence - Top Predictor for Each State-Source Combo

predictors <- c(allsvmpd$CA$Hydropower[[1]][,1], allsvmpd$CA$Solar[[1]][,1],allsvmpd$CA$Wind[[1]][,1])
yhat <- c(allsvmpd$CA$Hydropower[[1]][,2], allsvmpd$CA$Solar[[1]][,2],allsvmpd$CA$Wind[[1]][,2])
sources <- c(rep('Hydropower',51), rep('Solar', 51), rep('Wind', 51))

plotdata <- data.frame(predictors, yhat, sources)

p1 <- ggplot(plotdata) + geom_line(aes(x = predictors, y = yhat)) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Top Predictor') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + ggtitle('California')

predictors <- c(allsvmpd$NY$Hydropower[[1]][,1], allsvmpd$NY$Solar[[1]][,1],allsvmpd$NY$Wind[[1]][,1])
yhat <- c(allsvmpd$NY$Hydropower[[1]][,2], allsvmpd$NY$Solar[[1]][,2],allsvmpd$NY$Wind[[1]][,2])
sources <- c(rep('Hydropower',51), rep('Solar', 51), rep('Wind', 51))

plotdata <- data.frame(predictors, yhat, sources)

p2 <- ggplot(plotdata) + geom_line(aes(x = predictors, y = yhat)) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Top Predictor') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + ggtitle('New York')

predictors <- c(allsvmpd$FL$Hydropower[[1]][,1], allsvmpd$FL$Solar[[1]][,1])
yhat <- c(allsvmpd$FL$Hydropower[[1]][,2], allsvmpd$FL$Solar[[1]][,2])
sources <- c(rep('Hydropower',51), rep('Solar', 51))

plotdata <- data.frame(predictors, yhat, sources)

p3 <- ggplot(plotdata) + geom_line(aes(x = predictors, y = yhat)) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Top Predictor') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + ggtitle('Florida')

predictors <- c(allsvmpd$GA$Hydropower[[1]][,1], allsvmpd$GA$Solar[[1]][,1])
yhat <- c(allsvmpd$GA$Hydropower[[1]][,2], allsvmpd$GA$Solar[[1]][,2])
sources <- c(rep('Hydropower',51), rep('Solar', 51))

plotdata <- data.frame(predictors, yhat, sources)

p4 <- ggplot(plotdata) + geom_line(aes(x = predictors, y = yhat)) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Top Predictor') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + ggtitle('Georgia')

setwd(outputdir)
pdf('figure4.pdf', width = 10, height = 16)
plot_grid(p1, p2, p3, p4, nrow = 4)
# NOTE: x-axis labels were updated in post-processing
dev.off()

# FIGURE 5: Ensemble Model Performance

allmeans <- c(mean(allnrmse$CA$Hydropower$rf), mean(allnrmse$CA$Hydropower$bart), mean(allnrmse$CA$Hydropower$svm), 
              mean(allnrmse$CA$Solar$rf), mean(allnrmse$CA$Solar$bart), mean(allnrmse$CA$Solar$svm), 
              mean(allnrmse$CA$Wind$rf), mean(allnrmse$CA$Wind$bart), mean(allnrmse$CA$Wind$svm),
              mean(allnrmse$NY$Hydropower$rf), mean(allnrmse$NY$Hydropower$bart), mean(allnrmse$NY$Hydropower$svm), 
              mean(allnrmse$NY$Solar$rf), mean(allnrmse$NY$Solar$bart), mean(allnrmse$NY$Solar$svm), 
              mean(allnrmse$NY$Wind$rf), mean(allnrmse$NY$Wind$bart), mean(allnrmse$NY$Wind$svm), 
              mean(allnrmse$FL$Hydropower$rf), mean(allnrmse$FL$Hydropower$bart), mean(allnrmse$FL$Hydropower$svm), 
              mean(allnrmse$FL$Solar$rf), mean(allnrmse$FL$Solar$bart), mean(allnrmse$FL$Solar$svm),
              mean(allnrmse$GA$Hydropower$rf), mean(allnrmse$GA$Hydropower$bart), mean(allnrmse$GA$Hydropower$svm), 
              mean(allnrmse$GA$Solar$rf), mean(allnrmse$GA$Solar$bart), mean(allnrmse$GA$Solar$svm),
              unlist(allensemblenrmse))

models <- c(rep(c('2RF', '1BART', '3SVM'), 10), rep('4Ensemble', 10))
states <- c(rep('CA', 9), rep('NY', 9), rep('FL', 6), rep('GA', 6), rep('CA', 3), rep('NY', 3), rep('FL', 2), rep('GA',2)) 
sources <- c(rep(c(rep('Hydro', 3), rep('Solar', 3), rep('Wind', 3)), 2), rep(c(rep('Hydro', 3), rep('Solar', 3)),2), rep(c('Hydro', 'Solar', 'Wind'), 2), rep(c('Hydro', 'Solar'), 2))

plotdata <- data.frame(allmeans, models, states, sources) 

setwd(outputdir)  
pdf('figure5.pdf', width = 11, height = 4.5)
ggplot(plotdata) + geom_bar(aes(x = states, y = allmeans, fill = models), stat='identity', position = 'dodge') +
  facet_wrap(~sources, scales = 'free', nrow = 1) + theme_light() + ylab('NRMSE') + xlab('State') +
  theme(text = element_text(size = 16), legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_fill_manual(name = 'Model', values = c('#66c2a5', '#fc8d62','#8da0cb', '#e5c494'), labels = c('BART', 'RF', 'SVM', 'Ensemble'))
dev.off()  

# SUPPLEMENTARY FIGURES: 

# FIGURE S1: Variable Selection Results 

plotdata <- data.frame(variables, selectedvariables, states, sources)

p1 <- ggplot(plotdata[which(plotdata$states == 'CA'),]) + geom_point(aes(x = selectedvariables, y = variables), pch = 21, size = 8, fill = '#6baed6', color = '#2171b5') + theme_light() + 
  xlab('') + ylab('') + theme(text = element_text(size=20)) + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources) 

p2 <- ggplot(plotdata[which(plotdata$states == 'NY'),]) + geom_point(aes(x = selectedvariables, y = variables), pch = 21, size = 8, fill = '#6baed6', color = '#2171b5') + theme_light() + 
  xlab('') + ylab('') + theme(text = element_text(size=20)) + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources) 

p3 <- ggplot(plotdata[which(plotdata$states == 'FL'),]) + geom_point(aes(x = selectedvariables, y = variables), pch = 21, size = 8, fill = '#6baed6', color = '#2171b5') + theme_light() + 
  xlab('') + ylab('') + theme(text = element_text(size=20)) + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources) 

p4 <- ggplot(plotdata[which(plotdata$states == 'GA'),]) + geom_point(aes(x = selectedvariables, y = variables), pch = 21, size = 8, fill = '#6baed6', color = '#2171b5') + theme_light() + 
  xlab('') + ylab ('') + theme(text = element_text(size=20)) + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources) 

setwd(outputdir)
pdf('figureS1.pdf', width = 9, height = 16)
plot_grid(p1, p2, p3, p4, nrow = 4)
dev.off()

# FIGURE S2: Model Performance - NRMSE

means <- c(unlist(lapply(allnrmse$CA$Hydropower, function(x) mean(x))), unlist(lapply(allnrmse$CA$Solar, function(x) mean(x))), unlist(lapply(allnrmse$CA$Wind, function(x) mean(x))),
           unlist(lapply(allnrmse$NY$Hydropower, function(x) mean(x))), unlist(lapply(allnrmse$NY$Solar, function(x) mean(x))), unlist(lapply(allnrmse$NY$Wind, function(x) mean(x))),
           unlist(lapply(allnrmse$FL$Hydropower, function(x) mean(x))), unlist(lapply(allnrmse$FL$Solar, function(x) mean(x))),
           unlist(lapply(allnrmse$GA$Hydropower, function(x) mean(x))), unlist(lapply(allnrmse$GA$Solar, function(x) mean(x))))

maxs <- c(unlist(lapply(allnrmse$CA$Hydropower, function(x) max(x))), unlist(lapply(allnrmse$CA$Solar, function(x) max(x))), unlist(lapply(allnrmse$CA$Wind, function(x) max(x))),
          unlist(lapply(allnrmse$NY$Hydropower, function(x) max(x))), unlist(lapply(allnrmse$NY$Solar, function(x) max(x))), unlist(lapply(allnrmse$NY$Wind, function(x) max(x))),
          unlist(lapply(allnrmse$FL$Hydropower, function(x) max(x))), unlist(lapply(allnrmse$FL$Solar, function(x) max(x))),
          unlist(lapply(allnrmse$GA$Hydropower, function(x) max(x))), unlist(lapply(allnrmse$GA$Solar, function(x) max(x))))

mins <- c(unlist(lapply(allnrmse$CA$Hydropower, function(x) min(x))), unlist(lapply(allnrmse$CA$Solar, function(x) min(x))), unlist(lapply(allnrmse$CA$Wind, function(x) min(x))),
          unlist(lapply(allnrmse$NY$Hydropower, function(x) min(x))), unlist(lapply(allnrmse$NY$Solar, function(x) min(x))), unlist(lapply(allnrmse$NY$Wind, function(x) min(x))),
          unlist(lapply(allnrmse$FL$Hydropower, function(x) min(x))), unlist(lapply(allnrmse$FL$Solar, function(x) min(x))),
          unlist(lapply(allnrmse$GA$Hydropower, function(x) min(x))), unlist(lapply(allnrmse$GA$Solar, function(x) min(x))))

models <- rep(c('GLM', 'MARS', 'CART', 'RF', 'BART', 'SVM'), 10)
states <- c(rep('CA', 6*3), rep('NY', 6*3), rep('FL', 6*2), rep('GA', 6*2))
sources <- c(rep('Hydropower',6), rep('Solar', 6), rep('Wind', 6), rep('Hydropower',6), rep('Solar', 6), rep('Wind', 6), rep('Hydropower',6), rep('Solar', 6), rep('Hydropower',6), rep('Solar', 6))

plotdata <- data.frame(means, mins, maxs, models, states, sources)

setwd(outputdir)
pdf('figureS2.pdf', width = 11, height = 4.7)
ggplot(plotdata, aes(x = states, ymin = mins, ymax = maxs, lower = mins, upper = maxs, middle = means)) + 
  geom_boxplot(aes(fill = models), stat = 'identity') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Model', values = c('#7fc97f','#beaed4','#fdc086','#386cb0','#ffff99','#bf5b17')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources)
dev.off()

# FIGURE S3: Model Performance - R^2

means <- c(unlist(lapply(allrsq$CA$Hydropower, function(x) mean(x, na.rm = T))), unlist(lapply(allrsq$CA$Solar, function(x) mean(x, na.rm = T))), unlist(lapply(allrsq$CA$Wind, function(x) mean(x, na.rm = T))),
           unlist(lapply(allrsq$NY$Hydropower, function(x) mean(x, na.rm = T))), unlist(lapply(allrsq$NY$Solar, function(x) mean(x, na.rm = T))), unlist(lapply(allrsq$NY$Wind, function(x) mean(x, na.rm = T))),
           unlist(lapply(allrsq$FL$Hydropower, function(x) mean(x, na.rm = T))), unlist(lapply(allrsq$FL$Solar, function(x) mean(x, na.rm = T))),
           unlist(lapply(allrsq$GA$Hydropower, function(x) mean(x, na.rm = T))), unlist(lapply(allrsq$GA$Solar, function(x) mean(x, na.rm = T))))

maxs <- c(unlist(lapply(allrsq$CA$Hydropower, function(x) max(x, na.rm = T))), unlist(lapply(allrsq$CA$Solar, function(x) max(x, na.rm = T))), unlist(lapply(allrsq$CA$Wind, function(x) max(x, na.rm = T))),
          unlist(lapply(allrsq$NY$Hydropower, function(x) max(x, na.rm = T))), unlist(lapply(allrsq$NY$Solar, function(x) max(x, na.rm = T))), unlist(lapply(allrsq$NY$Wind, function(x) max(x, na.rm = T))),
          unlist(lapply(allrsq$FL$Hydropower, function(x) max(x, na.rm = T))), unlist(lapply(allrsq$FL$Solar, function(x) max(x, na.rm = T))),
          unlist(lapply(allrsq$GA$Hydropower, function(x) max(x, na.rm = T))), unlist(lapply(allrsq$GA$Solar, function(x) max(x, na.rm = T))))

mins <- c(unlist(lapply(allrsq$CA$Hydropower, function(x) min(x, na.rm = T))), unlist(lapply(allrsq$CA$Solar, function(x) min(x, na.rm = T))), unlist(lapply(allrsq$CA$Wind, function(x) min(x, na.rm = T))),
          unlist(lapply(allrsq$NY$Hydropower, function(x) min(x, na.rm = T))), unlist(lapply(allrsq$NY$Solar, function(x) min(x, na.rm = T))), unlist(lapply(allrsq$NY$Wind, function(x) min(x, na.rm = T))),
          unlist(lapply(allrsq$FL$Hydropower, function(x) min(x, na.rm = T))), unlist(lapply(allrsq$FL$Solar, function(x) min(x, na.rm = T))),
          unlist(lapply(allrsq$GA$Hydropower, function(x) min(x, na.rm = T))), unlist(lapply(allrsq$GA$Solar, function(x) min(x, na.rm = T))))

plotdata <- data.frame(means, mins, maxs, models, states, sources)

setwd(outputdir)
pdf('figureS3.pdf', width = 11, height = 4.7)
ggplot(plotdata, aes(x = states, ymin = mins, ymax = maxs, lower = mins, upper = maxs, middle = means)) + 
  geom_boxplot(aes(fill = models), stat = 'identity') + theme_light() + 
  xlab('') + scale_fill_manual(name = 'Model', values = c('#7fc97f','#beaed4','#fdc086','#386cb0','#ffff99','#bf5b17')) +
  theme(legend.position = 'bottom', text = element_text(size = 20)) +
  facet_wrap(~sources)
dev.off()

# FIGURES S5-S8: Partial Dependence

predictors <- c(); yhat <- c(); variables <- c(); sources <- c(); states <- c()

# loop through all states
for (i in 1:length(allsvmpd)) {
  state <- allsvmpd[[i]]
  
  # loop through all sources
  for (j in 1:length(state)) {
    source <- state[[j]]
    
    # loop through all predictors
    for (k in 1:length(source)) {
      predictors <- append(predictors, source[[k]][,1])
      yhat <- append(yhat, source[[k]][,2])
      variables <- append(variables, rep(names(source[[k]])[1],51))
    }
    
    numpred <- length(source)
    sources <- append(sources, rep(names(state)[j], numpred*51))
    states <- append(states, rep(names(allsvmpd)[i], numpred*51))
  }
}

split_string <- strsplit(variables, split = "\\s|_")

plotdata <- data.frame(predictors, yhat, sources, states, variables, 
                       measure = sapply(split_string, `[`, 1),
                       name = sapply(split_string, `[`, 2))

# California
p1 <- ggplot(plotdata[which(plotdata$states == 'CA' & plotdata$name == 'temp'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Temperature (°C)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean', 'Max', 'Min'), values = c('#66c2a5', '#fc8d62', '#8da0cb'))

p2 <- ggplot(plotdata[which(plotdata$states == 'CA' & plotdata$name == 'rh'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Relative Humidity (%)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean', 'Max', 'Min'), values = c('#66c2a5', '#fc8d62', '#8da0cb'))

p3 <- ggplot(plotdata[which(plotdata$states == 'CA' & plotdata$name == 'ws'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Wind Speed (m/s)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean', 'Max', 'Min'), values = c('#66c2a5', '#fc8d62', '#8da0cb'))

p4 <- ggplot(plotdata[which(plotdata$states == 'CA' & plotdata$name == 'precip'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Precipitation (mm)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean', 'Max', 'Total'), values = c('#66c2a5', '#fc8d62', '#e5c494'))

setwd(outputdir)
pdf('figureS5.pdf', width = 11, height = 16)
plot_grid(p1, p2, p3, p4, nrow = 4)
dev.off()

# New York
p1 <- ggplot(plotdata[which(plotdata$states == 'NY' & plotdata$name == 'temp'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Temperature (°C)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean', 'Max', 'Min'), values = c('#66c2a5', '#fc8d62', '#8da0cb'))

p2 <- ggplot(plotdata[which(plotdata$states == 'NY' & plotdata$name == 'rh'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Relative Humidity (%)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean', 'Max', 'Min'), values = c('#66c2a5', '#fc8d62', '#8da0cb'))

p3 <- ggplot(plotdata[which(plotdata$states == 'NY' & plotdata$name == 'ws'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Wind Speed (m/s)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean'), values = c('#66c2a5'))

p4 <- ggplot(plotdata[which(plotdata$states == 'NY' & plotdata$name == 'precip'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Precipitation (mm)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Total'), values = c('#e5c494'))

setwd(outputdir)
pdf('figureS6.pdf', width = 11, height = 16)
plot_grid(p1, p2, p3, p4, nrow = 4)
dev.off()

# Florida
p1 <- ggplot(plotdata[which(plotdata$states == 'FL' & plotdata$name == 'temp'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Temperature (°C)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean', 'Max', 'Min'), values = c('#66c2a5', '#fc8d62', '#8da0cb'))

p2 <- ggplot(plotdata[which(plotdata$states == 'FL' & plotdata$name == 'rh'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Relative Humidity (%)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean', 'Max', 'Min'), values = c('#66c2a5', '#fc8d62', '#8da0cb'))

p3 <- ggplot(plotdata[which(plotdata$states == 'FL' & plotdata$name == 'ws'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Wind Speed (m/s)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean'), values = c('#66c2a5'))

p4 <- ggplot(plotdata[which(plotdata$states == 'FL' & plotdata$name == 'precip'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Precipitation (mm)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Total'), values = c('#e5c494'))

setwd(outputdir)
pdf('figureS7.pdf', width = 11, height = 16)
plot_grid(p1, p2, p3, p4, nrow = 4)
dev.off()

# Georgia
p1 <- ggplot(plotdata[which(plotdata$states == 'GA' & plotdata$name == 'temp'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Temperature (°C)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean', 'Max', 'Min'), values = c('#66c2a5', '#fc8d62', '#8da0cb'))

p2 <- ggplot(plotdata[which(plotdata$states == 'GA' & plotdata$name == 'rh'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Relative Humidity (%)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean', 'Max'), values = c('#66c2a5', '#fc8d62'))

p3 <- ggplot(plotdata[which(plotdata$states == 'GA' & plotdata$name == 'ws'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Wind Speed (m/s)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean'), values = c('#66c2a5'))

p4 <- ggplot(plotdata[which(plotdata$states == 'GA' & plotdata$name == 'precip'),]) + 
  geom_line(aes(x = predictors, y = yhat, color = measure), linewidth = 1) +
  theme_light() + theme(text = element_text(size = 20)) + xlab('Precipitation (mm)') +
  ylab('Generation (MWh/capita)') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'Measure', labels = c('Mean', 'Max', 'Total'), values = c('#66c2a5', '#fc8d62', '#e5c494'))

setwd(outputdir)
pdf('figureS8.pdf', width = 11, height = 16)
plot_grid(p1, p2, p3, p4, nrow = 4)
dev.off()

# FIGURE S4: Actual vs. Predicted - All Models

p1 <- ggplot(plotdata[which(plotdata$models == 'glm'),]) +
  geom_point(aes(x = actual, y = predicted, color = states)) + geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + 
  scale_x_continuous(breaks = breaks_pretty()) + theme_light() + theme(text = element_text(size = 14)) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'State', values = c('#66c2a5', '#fc8d62','#8da0cb', '#e5c494')) +
  ylab('Predicted Values') + xlab('Actual Values') + ggtitle('Generalized Linear Models')

p2 <- ggplot(plotdata[which(plotdata$models == 'mars'),]) +
  geom_point(aes(x = actual, y = predicted, color = states)) + geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + 
  scale_x_continuous(breaks = breaks_pretty()) + theme_light() + theme(text = element_text(size = 14)) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'State', values = c('#66c2a5', '#fc8d62','#8da0cb', '#e5c494')) +
  ylab('Predicted Values') + xlab('Actual Values') + ggtitle('Multivariate Adaptive Regression Splines')

p3 <- ggplot(plotdata[which(plotdata$models == 'cart'),]) +
  geom_point(aes(x = actual, y = predicted, color = states)) + geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + 
  scale_x_continuous(breaks = breaks_pretty()) + theme_light() + theme(text = element_text(size = 14)) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'State', values = c('#66c2a5', '#fc8d62','#8da0cb', '#e5c494')) +
  ylab('Predicted Values') + xlab('Actual Values') + ggtitle('Decision Tree')

p4 <- ggplot(plotdata[which(plotdata$models == 'rf'),]) +
  geom_point(aes(x = actual, y = predicted, color = states)) + geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + 
  scale_x_continuous(breaks = breaks_pretty()) + theme_light() + theme(text = element_text(size = 14)) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'State', values = c('#66c2a5', '#fc8d62','#8da0cb', '#e5c494')) +
  ylab('Predicted Values') + xlab('Actual Values') + ggtitle('Random Forest')

p5 <- ggplot(plotdata[which(plotdata$models == 'bart'),]) +
  geom_point(aes(x = actual, y = predicted, color = states)) + geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + 
  scale_x_continuous(breaks = breaks_pretty()) + theme_light() + theme(text = element_text(size = 14)) +
  facet_wrap(~sources, scales = 'free') + theme(legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'State', values = c('#66c2a5', '#fc8d62','#8da0cb', '#e5c494')) +
  ylab('Predicted Values') + xlab('Actual Values') + ggtitle('Bayesian Additive Regression Trees')

setwd(outputdir)
pdf('figureS4.pdf', width = 8, height = 18)
plot_grid(p1, p2, p3, p4, p5, nrow = 5)
dev.off()

# FIGURE S9: Ensemble Model Predicted vs. Actual

predicted <- c(); actual <-c()

# loop through all states
for (i in 1:length(allyhat)) {
  state <- allensemblemean[[i]]
  
  # loop through all sources
  for (j in 1:length(state)) {
    source <- state[[j]]
    
    predicted <- append(predicted, source)
    actual <- append(actual, allyhat[[i]][[j]]$testData)
  }
}

states <- c(rep('CA', 792), rep('NY', 662), rep('FL', 423), rep('GA', 396))
sources <- c(rep(c('Hydro', 'Solar', 'Wind'), each = 264), rep('Hydro', 264), rep('Solar', 134), rep('Wind', 264), rep('Hydro', 264), rep('Solar', 159), rep('Hydro', 264), rep('Solar', 132))

plotdata <- data.frame(predicted, actual, states, sources)  

setwd(outputdir)
pdf('figureS9.pdf',  width = 10, height = 4)
ggplot(plotdata) + geom_point(aes(x = actual, y = predicted, color = states)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + scale_x_continuous(breaks = breaks_pretty()) +
  facet_wrap(~sources, scales = 'free') + theme_light() + ylab('Predicted Values') + xlab('Actual Values') +
  theme(text = element_text(size = 16), legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) +
  scale_color_manual(name = 'State', values = c('#66c2a5', '#fc8d62','#8da0cb', '#e5c494'))
dev.off()  
  







             

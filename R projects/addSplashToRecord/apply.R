# Script to add SPLASH line to existing MassBank records
# Erik Müller
# Copyright (c) 2016
# LICENSE: GPL 3.0

## Install BioCLite and the newest version of RMassBank
biocLite("BiocInstaller")
source("https://bioconductor.org/biocLite.R")
biocLite("BiocInstaller")
install_github("MassBank/RMassBank")
install.packages("RMassBankData")
library("RMassBank", lib.loc="c:/R/win-library/3.2") # change the lib.loc to your preferences
library("RMassBankData", lib.loc="c:/R/win-library/3.2") # change the lib.loc to your preferences

source('c:/your_path/addSplashToRecord.R') # make the functions available
.addSplashToRecord("c:/recorddir") # change to directory containing the records, adds SPLASH to records
source('C:/R_code/MassBankAdministrationScripts/R projects/addSplashToRecord/addSplashToRecord.R')

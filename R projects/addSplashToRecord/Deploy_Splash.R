# Script to add SPLASH line to existing MassBank records
# Erik Müller, Tobias Schulze
# Copyright (c) 2016-2017
# LICENSE: GPL 3.0

## Install BioCLite and the newest version of RMassBank
biocLite("BiocInstaller")
source("https://bioconductor.org/biocLite.R")
biocLite("BiocInstaller")
install_github("MassBank/RMassBank")
library("RMassBank", lib.loc="c:/R/win-library/3.2") # change the lib.loc to your preferences


setwd(path_to_your_working_directory)
source("./addSplashToRecord.R")

# Apply the script to a folder with records, can be recursive
.addSplashToRecord("path_to_records")

# This changes the data to new_date (Created: old_date)
.changeRecordDate(recpath = "path_to_records")

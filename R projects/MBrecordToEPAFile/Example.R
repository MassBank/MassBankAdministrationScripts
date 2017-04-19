##Script to run the getInfo and getInfoFixKey functions with your MassBank records
##Authors: Erik Mueller, Tobias Schulze, Emma Schymanski
##Copyright: (C) 2014
##License: GPL 2.0
##Last edit: 7/7/14 by Tobias Schulze


##Set working directory
##setw(PathToWorkingDirectory)
setwd("D:/annotation")
babel_dir <- "c:/OpenBabel/"

##Run this script for extraction of information (details see getInfo.R) for EPA Dashboard
source("c:/R_code/MassBankAdministrationScripts/R projects/MBrecordToEPAFile/getInfo_4EPA.R")
getInfo.EPA("D:/annotation/","MassBank_Dump_2016-10-06_EPA.csv")


##Script to run the getInfo and getInfoFixKey functions with your MassBank records
##Authors: Erik Mueller, Tobias Schulze, Emma Schymanski
##Copyright: (C) 2014
##License: GPL 2.0
##Last edit: 7/7/14 by Tobias Schulze


##Set working directory
##setw(PathToWorkingDirectory)
setwd("D:/MassBank/MB_Dump/Records")

##Run this script for extraction of information (details see getInfo.R) without InChIKey
source("getInfo.R")
getInfo("MB_all","Result_multiDirs_diffIfElse.csv")

##Run this script for extraction of information (details see getInfo.R) with InChIKey
##Missing InChIkey will be added by using OpenBabel
##OpenBabel is available here: http://openbabel.org/wiki/Main_Page
##The path to OpenBabel must not contain spaces.
##This routine will run some time due to single retrival of InChIKey
source("getInfo_and_InChIKeys.R")
getInfoFixKey("MB_all","Result_multiDirs_diffIfElse_InChIKeys.csv","c:/OpenBabel")


##Run this script for extended extraction of information (details see getInfo_and_InChIKeys_extended.R) with InChIKey
##Missing InChIkey will be added by using OpenBabel
##OpenBabel is available here: http://openbabel.org/wiki/Main_Page
##The path to OpenBabel must not contain spaces.
##This routine will run some time due to single retrival of InChIKey
source("getInfo_and_InChIKeys_extended.R")
getInfoFixKey("MB_all","Result_multiDirs_diffIfElse_InChIKeys.csv","c:/OpenBabel")
## Script by Erik Müller, Tobias Schulze, Emma Schymanski
## Script to parse information in MassBank records to CSV lists
## Copyright: (c) 2014-2017
## License: GPL 3.0
## Edited 14/7/7 E. Schymanski (adding InChI Key functionality)
## Edited 16/1/31 T. Schulze (adding parsing of more fields)
## Edited 17/4/19 T. Schulze (adding parsing of more fields)
## note new dependence on obabel.exe


# get InChI Keys using Open Babel
# need to define the directory containing Open Babel, specifically "obabel.exe"
# This does not work if the path contains a space; the exe can be pasted anywhere
create.inchikey <- function(SMILES, babel_dir) {
  cmd <- paste(babel_dir, "/obabel -:", SMILES, " -oinchikey", sep="")
  res <- system(cmd, intern=TRUE, ignore.stderr=TRUE)
  return(res)
}
# batch conversion would be faster, but stops at first error without warning.

##Directory is the name of the directory
##csvname is the designated name that the csv-file will get
##babel_dir is the directory path (no spaces!) containing obabel.exe
getInfoFixKey <- function(Directory,csvname, babel_dir){
		Files <- list.files(Directory, pattern="*.txt", full.names=TRUE, recursive=TRUE)
    #recursive=TRUE should get all sub-dirs
    #need to add pattern to skip the mols and tsvs
    #
## Good to know how many files will be processed
print(paste0("We will process ",length(Files), " Files!"))
		
		wantedmat <- matrix(0,length(Files),(38))
		for(i in 1:length(Files)){
			fileConnection <- file(normalizePath(Files[i]))
			record <- readLines(fileConnection)
			close(fileConnection)
      
      ## Check if fields contain NAs
			CSIDTRUE <- grep('CH$LINK: CHEMSPIDER',record, value = TRUE, fixed = TRUE)
			CSIDFALSE <- "N/A"
			CASTRUE <- grep('CH$LINK: CAS',record, value = TRUE, fixed = TRUE)
			CASFALSE <- "N/A"
			CIDTRUE <- grep('CH$LINK: PUBCHEM',record, value = TRUE, fixed = TRUE)
			CIDFALSE <- "N/A"
			CSIDTRUE <- grep('CH$LINK: CHEMSPIDER',record, value = TRUE, fixed = TRUE)
			CSIDFALSE <- "N/A"
			INCHIKEYTRUE <- grep('CH$LINK: INCHIKEY',record, value = TRUE, fixed = TRUE)
			INCHIKEYFALSE <- "N/A"
			INCHIKEY <- ifelse(length(INCHIKEYTRUE)==1, substring(grep('CH$LINK: INCHIKEY',record, value = TRUE, fixed = TRUE),19), INCHIKEYFALSE)
			SMILES <- substring(grep('CH$SMILES:',record, value = TRUE, fixed = TRUE),12)
			SMILES_NA <- grep("N/A",SMILES, value = TRUE, fixed = TRUE)
			INCHIKEY_NA <- grep("N/A",INCHIKEY, value = TRUE, fixed = TRUE)
			INSTRUMENT_TYPE_TRUE <- grep('AC$INSTRUMENT_TYPE:',record, value = TRUE, fixed = TRUE)
			INSTRUMENT_TYPE_FALSE <- "N/A"
			INSTRUMENT_TRUE <- grep('AC$INSTRUMENT:',record, value = TRUE, fixed = TRUE)
			INSTRUMENT_FALSE <- "N/A"
			RESOLUTION_TRUE <- grep('AC$MASS_SPECTROMETRY: RESOLUTION',record, value = TRUE, fixed = TRUE)
			RESOLUTION_FALSE <- "N/A"
			MS_TYPE_TRUE <- grep('AC$MASS_SPECTROMETRY: MS_TYPE',record, value = TRUE, fixed = TRUE)
			MS_TYPE_FALSE <- "N/A"
			PRECURSOR_TYPE_TRUE <- grep('MS$FOCUSED_ION: PRECURSOR_TYPE',record, value = TRUE, fixed = TRUE)
			PRECURSOR_TYPE_FALSE <- "N/A"
			PRECURSOR_MZ_TRUE <- grep('MS$FOCUSED_ION: PRECURSOR_M/Z',record, value = TRUE, fixed = TRUE)
			PRECURSOR_MZ_FALSE <- "N/A"
			BASE_PEAK_TRUE <- grep('MS$FOCUSED_ION: BASE_PEAK',record, value = TRUE, fixed = TRUE)
			BASE_PEAK_FALSE <- "N/A"
			IONIZATION_TRUE <- grep('AC$MASS_SPECTROMETRY: IONIZATION',record, value = TRUE, fixed = TRUE)
			IONIZATION_FALSE <- "N/A"
			FRAGMENTATION_MODE_TRUE <- grep('AC$MASS_SPECTROMETRY: FRAGMENTATION_MODE',record, value = TRUE, fixed = TRUE)
			FRAGMENTATION_MODE_FALSE <- "N/A"
			COLL_E_TRUE <- grep('AC$MASS_SPECTROMETRY: COLLISION_ENERGY',record, value = TRUE, fixed = TRUE)
			COLL_E_FALSE <- "N/A"
			COLUMN_NAME_TRUE <- grep('AC$CHROMATOGRAPHY: COLUMN_NAME',record, value = TRUE, fixed = TRUE)
			COLUMN_NAME_FALSE <- "N/A"
		  FLOW_GRADIENT_TRUE <- grep('AC$CHROMATOGRAPHY: FLOW_GRADIENT',record, value = TRUE, fixed = TRUE)
			FLOW_GRADIENT_FALSE <- "N/A"
			FLOW_RATE_TRUE <- grep('AC$CHROMATOGRAPHY: FLOW_RATE',record, value = TRUE, fixed = TRUE)
			FLOW_RATE_FALSE <- "N/A"
			RETENTION_TIME_TRUE <- grep('AC$CHROMATOGRAPHY: RETENTION_TIME',record, value = TRUE, fixed = TRUE)
		  RETENTION_TIME_FALSE <- "N/A"
		  SOLVENT_A_TRUE <- grep('AC$CHROMATOGRAPHY: SOLVENT A',record, value = TRUE, fixed = TRUE)
		  SOLVENT_A_FALSE <- "N/A"
		  SOLVENT_B_TRUE <- grep('AC$CHROMATOGRAPHY: SOLVENT B',record, value = TRUE, fixed = TRUE)
		  SOLVENT_B_FALSE <- "N/A"

		  SPLASH_TRUE <- grep('PK$SPLASH:',record, value = TRUE, fixed = TRUE)
		  SPLASH_FALSE <- "N/A"
		  
		  # Collapse MS$DATA_PROCESSING
		  processing <- list()
		  processing <- as.list(substring(grep('MS$DATA_PROCESSING:',record, value = TRUE, fixed = TRUE),21))
		  
		  DATA_PROCESSING_TRUE <- paste(processing, '#', collapse = '')
		  DATA_PROCESSING_FALSE <-"N/A"
		  
		  # Collapse COMMENT
		  comments <- list()
		  comments <- as.list(substring(grep('COMMENT:',record, value = TRUE, fixed = TRUE),10))
		  
		  COMMENT_TRUE <- paste(comments, '#', collapse = '')
		  COMMENT_FALSE <-"N/A"

		  
      #fill in missing InChI Key where possible with Open Babel conversion from SMILES
      # Can only attempt this if SMILES exists; takes a while so don't recalculate unless necessary
      if((length(INCHIKEY_NA)==1)&&(length(SMILES_NA)!=1)) {
        new_inchikey <- create.inchikey(SMILES, babel_dir)
        if(length(new_inchikey)>=1) {
          INCHIKEY <- new_inchikey
        }
      }
## Good to know what R is doing
print(paste0("In progress with #",i," of ",length(Files)," records with ACCESSION ",substring(grep('ACCESSION:',record, value = TRUE, fixed = TRUE),12),"."))
		  
## Parse the fields from the records	  
colnames(wantedmat) <- c("ACCESSION","AUTHORS","LICENSE","DATE","COMMENT","NAME","FORMULA","EXACT_MASS","IUPAC","INCHIKEY","SMILES","CSID","CID","CAS","INSTRUMENT","INSTRUMENT_TYPE","MS_TYPE","IONIZATION","ION_MODE","FRAGMENTATION_MODE","COLL_E","COLL_E_UNIT","RESOLUTION","COLUMN_NAME","FLOW_GRADIENT","FLOW_RATE","FLOW_RATE_UNIT","RETENTION_TIME","RETENTION_TIME_UNIT","SOLVENT_A","SOLVENT_B","BASE_PEAK","PRECURSOR_MZ","PRECURSOR_TYPE","DATA_PROCESSING","SPLASH","EULINK","JPLINK")
## The information block
wantedmat[i,'ACCESSION'] <- substring(grep('ACCESSION:',record, value = TRUE, fixed = TRUE),12)
wantedmat[i,'AUTHORS'] <- substring(grep('AUTHORS:',record, value = TRUE, fixed = TRUE),10)
wantedmat[i,'LICENSE'] <- substring(grep('LICENSE:',record, value = TRUE, fixed = TRUE),10)
wantedmat[i,'DATE'] <- substring(grep('DATE:',record, value = TRUE, fixed = TRUE),7)
ifelse(length(COMMENT_TRUE)==1, wantedmat[i,'COMMENT'] <- COMMENT_TRUE, wantedmat[i,'COMMENT'] <- COMMENT_FALSE)
chnames <- list()
chnames <- as.list(substring(grep('CH$NAME:',record, value = TRUE, fixed = TRUE),10))
wantedmat[i,'NAME'] <- chnames[[1]]
#wantedmat[i,'SMILES'] <- substring(grep('CH$SMILES:',record, value = TRUE, fixed = TRUE),12)
wantedmat[i,'FORMULA'] <- substring(grep('CH$FORMULA:',record, value = TRUE, fixed = TRUE),13)
wantedmat[i,'EXACT_MASS'] <- substring(grep('CH$EXACT_MASS',record, value = TRUE, fixed = TRUE),16)
wantedmat[i,'SMILES'] <- SMILES
wantedmat[i,'IUPAC'] <- substring(grep('CH$IUPAC:',record, value = TRUE, fixed = TRUE),11)

## The next lines check if field is NA or not (for optional fields)
#ifelse(is.na(INCHIKEYTRUE) == TRUE, wantedmat[i,'INCHIKEY'] <- INCHIKEYFALSE, wantedmat[i,'INCHIKEY'] <- substring(grep('CH$LINK: INCHIKEY',record, value = TRUE, fixed = TRUE),19))
#ifelse(is.na(CSIDTRUE) == TRUE, wantedmat[i,'CSID'] <- CSIDFALSE, wantedmat[i,'CSID'] <- substring(grep('CH$LINK: CHEMSPIDER',record, value = TRUE, fixed = TRUE),21))
#INCHIKEY <- ifelse(length(INCHIKEYTRUE)==1, wantedmat[i,'INCHIKEY'] <- substring(grep('CH$LINK: INCHIKEY',record, value = TRUE, fixed = TRUE),19), wantedmat[i,'INCHIKEY'] <- INCHIKEYFALSE)
wantedmat[i,'INCHIKEY'] <- INCHIKEY
ifelse(length(CSIDTRUE)==1, wantedmat[i,'CSID'] <- substring(grep('CH$LINK: CHEMSPIDER',record, value = TRUE, fixed = TRUE),21), wantedmat[i,'CSID'] <- CSIDFALSE)
ifelse(length(CIDTRUE)==1, wantedmat[i,'CID'] <- gsub("[a-z, A-Z, ,:]","", substring(grep('CH$LINK: PUBCHEM',record, value = TRUE, fixed = TRUE),17)), wantedmat[i,'CID'] <- CIDFALSE)
ifelse(length(CASTRUE)==1, wantedmat[i,'CAS'] <- as.character(substring(grep('CH$LINK: CAS',record, value = TRUE, fixed = TRUE),14)), wantedmat[i,'CAS'] <- CASFALSE)

## The instrument block
ifelse(length(INSTRUMENT_TRUE)==1, wantedmat[i,'INSTRUMENT'] <- substring(grep('AC$INSTRUMENT:',record, value = TRUE, fixed = TRUE),16), wantedmat[i,'INSTRUMENT'] <- INSTRUMENT_FALSE)
ifelse(length(INSTRUMENT_TYPE_TRUE)==1, wantedmat[i,'INSTRUMENT_TYPE'] <- substring(grep('AC$INSTRUMENT_TYPE:',record, value = TRUE, fixed = TRUE),21), wantedmat[i,'INSTRUMENT_TYPE'] <- INSTRUMENT_TYPE_FALSE)

## The spectroscopy block
ifelse(length(MS_TYPE_TRUE)==1, wantedmat[i,'MS_TYPE'] <- substring(grep('AC$MASS_SPECTROMETRY: MS_TYPE',record, value = TRUE, fixed = TRUE),31), wantedmat[i,'MS_TYPE'] <- MS_TYPE_FALSE)
ifelse(length(IONIZATION_TRUE)==1, wantedmat[i,'IONIZATION'] <- substring(grep('AC$MASS_SPECTROMETRY: IONIZATION',record, value = TRUE, fixed = TRUE),34), wantedmat[i,'IONIZATION'] <- IONIZATION_FALSE)
wantedmat[i,'ION_MODE'] <- substring(grep('AC$MASS_SPECTROMETRY: ION_MODE',record, value = TRUE, fixed = TRUE),32)
ifelse(length(FRAGMENTATION_MODE_TRUE)==1, wantedmat[i,'FRAGMENTATION_MODE'] <- substring(grep('AC$MASS_SPECTROMETRY: FRAGMENTATION_MODE',record, value = TRUE, fixed = TRUE),42), wantedmat[i,'FRAGMENTATION_MODE'] <- FRAGMENTATION_MODE_FALSE)
ifelse(length(COLL_E_TRUE)==1, wantedmat[i,'COLL_E'] <- gsub("[a-z,A-Z, ,(,),%,-]","",substring(grep('AC$MASS_SPECTROMETRY: COLLISION_ENERGY',record, value = TRUE, fixed = TRUE),40)), wantedmat[i,'COLL_E'] <- COLL_E_FALSE)
ifelse(length(COLL_E_TRUE)==1, wantedmat[i,'COLL_E_UNIT'] <- gsub("[0-9, ,(,),.]","",substring(grep('AC$MASS_SPECTROMETRY: COLLISION_ENERGY',record, value = TRUE, fixed = TRUE),40)), wantedmat[i,'COLL_E_UNIT'] <- COLL_E_FALSE)
ifelse(length(RESOLUTION_TRUE)==1, wantedmat[i,'RESOLUTION'] <- substring(grep('AC$MASS_SPECTROMETRY: RESOLUTION',record, value = TRUE, fixed = TRUE),34), wantedmat[i,'RESOLUTION'] <- RESOLUTION_FALSE)

## The chromatography block
ifelse(length(COLUMN_NAME_TRUE)==1, wantedmat[i,'COLUMN_NAME'] <- substring(grep('AC$CHROMATOGRAPHY: COLUMN_NAME',record, value = TRUE, fixed = TRUE),32), wantedmat[i,'COLUMN_NAME'] <- COLUMN_NAME_FALSE)
ifelse(length(FLOW_GRADIENT_TRUE)==1, wantedmat[i,'FLOW_GRADIENT'] <- substring(grep('AC$CHROMATOGRAPHY: FLOW_GRADIENT',record, value = TRUE, fixed = TRUE),34), wantedmat[i,'FLOW_GRADIENT'] <- FLOW_GRADIENT_FALSE)
ifelse(length(FLOW_RATE_TRUE)==1, wantedmat[i,'FLOW_RATE'] <- gsub("[a-z,A-Z, ,/]", "", substring(grep('AC$CHROMATOGRAPHY: FLOW_RATE',record, value = TRUE, fixed = TRUE),30)), wantedmat[i,'FLOW_RATE'] <- FLOW_RATE_FALSE)
ifelse(length(FLOW_RATE_TRUE)==1, wantedmat[i,'FLOW_RATE_UNIT'] <- gsub("[0-9, ,.]", "", substring(grep('AC$CHROMATOGRAPHY: FLOW_RATE',record, value = TRUE, fixed = TRUE),30)), wantedmat[i,'FLOW_RATE_UNIT'] <- FLOW_RATE_FALSE)
ifelse(length(RETENTION_TIME_TRUE)==1, wantedmat[i,'RETENTION_TIME'] <- gsub("[a-z,A-Z, ]", "", substring(grep('AC$CHROMATOGRAPHY: RETENTION_TIME',record, value = TRUE, fixed = TRUE),35)), wantedmat[i,'RETENTION_TIME'] <- RETENTION_TIME_FALSE)
ifelse(length(RETENTION_TIME_TRUE)==1, wantedmat[i,'RETENTION_TIME_UNIT'] <- gsub("[0-9, ,.]","", substring(grep('AC$CHROMATOGRAPHY: RETENTION_TIME',record, value = TRUE, fixed = TRUE),35)), wantedmat[i,'RETENTION_TIME_UNIT'] <- RETENTION_TIME_FALSE)
ifelse(length(SOLVENT_A_TRUE)==1, wantedmat[i,'SOLVENT_A'] <- substring(grep('AC$CHROMATOGRAPHY: SOLVENT A',record, value = TRUE, fixed = TRUE),30), wantedmat[i,'SOLVENT_A'] <- SOLVENT_A_FALSE)
ifelse(length(SOLVENT_B_TRUE)==1, wantedmat[i,'SOLVENT_B'] <- substring(grep('AC$CHROMATOGRAPHY: SOLVENT B',record, value = TRUE, fixed = TRUE),30), wantedmat[i,'SOLVENT_B'] <- SOLVENT_A_FALSE)

## The MS block
ifelse(length(BASE_PEAK_TRUE)==1, wantedmat[i,'BASE_PEAK'] <- substring(grep('MS$FOCUSED_ION: BASE_PEAK ',record, value = TRUE, fixed = TRUE),26), wantedmat[i,'BASE_PEAK'] <- BASE_PEAK_FALSE)
ifelse(length(PRECURSOR_MZ_TRUE)==1, wantedmat[i,'PRECURSOR_MZ'] <- substring(grep('MS$FOCUSED_ION: PRECURSOR_M/Z ',record, value = TRUE, fixed = TRUE),30), wantedmat[i,'PRECURSOR_MZ'] <- PRECURSOR_MZ_FALSE)
ifelse(length(PRECURSOR_TYPE_TRUE)==1, wantedmat[i,'PRECURSOR_TYPE'] <- substring(grep('MS$FOCUSED_ION: PRECURSOR_TYPE ',record, value = TRUE, fixed = TRUE),31), wantedmat[i,'PRECURSOR_TYPE'] <- PRECURSOR_TYPE_FALSE)
ifelse(length(DATA_PROCESSING_TRUE)==1, wantedmat[i,'DATA_PROCESSING'] <- DATA_PROCESSING_TRUE, wantedmat[i,'DATA_PROCESSING'] <- DATA_PROCESSING_FALSE)


# The Peak block
ifelse(length(SPLASH_TRUE)==1, wantedmat[i,'SPLASH'] <- substring(grep('PK$SPLASH:',record, value = TRUE, fixed = TRUE),12), wantedmat[i,'SPLASH'] <- SPLASH_FALSE)


# wantedmat[i,'SPLASH'] <- substring(grep('PK$SPLASH:',record, value = TRUE, fixed = TRUE),12)

## The deep links
wantedmat[i,'EULINK'] <- paste("http://massbank.eu/MassBank/jsp/FwdRecord.jsp?id=", substring(grep('ACCESSION:',record, value = TRUE, fixed = TRUE),12), sep="")
wantedmat[i,'JPLINK'] <- paste("http://www.massbank.jp/jsp/FwdRecord.jsp?id=", substring(grep('ACCESSION:',record, value = TRUE, fixed = TRUE),12), sep="")
print(wantedmat[i,c('ACCESSION','SPLASH')])
}

## Write the csv file
write.csv(wantedmat,csvname)
return("Successfully wrote the csv")
}

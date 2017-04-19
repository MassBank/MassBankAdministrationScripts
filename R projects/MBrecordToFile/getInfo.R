##Script by Erik Müller, Tobias Schulze, Emma Schymanski
## To retrieve information from MassBank records and create summary CSV
## Copyright: (c) 2014-2017
## License: GPL 3.0

##Directory is the name of the directory
##csvname is the designated name that the csv-file will get
getInfo <- function(Directory,csvname){
		Files <- list.files(Directory, pattern="*.txt", full.names=TRUE, recursive=TRUE)
    #recursive=TRUE should get all sub-dirs
    #need to add pattern to skip the mols and tsvs
		wantedmat <- matrix(0,length(Files),(12))
		for(i in 1:length(Files)){
			fileConnection <- file(normalizePath(Files[i]))
			record <- readLines(fileConnection)
			close(fileConnection)
      
      ## Check if fields contain NAs
			CSIDTRUE <- grep('CH$LINK: CHEMSPIDER',record, value = TRUE, fixed = TRUE)
			CSIDFALSE <- "N/A"
      
      INCHIKEYTRUE <- grep('CH$LINK: INCHIKEY',record, value = TRUE, fixed = TRUE)
      INCHIKEYFALSE <- "N/A"
			
			colnames(wantedmat) <- c("ACCESSION","NAME","SMILES","EXACT_MASS","ION_MODE","FORMULA","LICENSE","IUPAC","INCHIKEY","CSID","EULINK","JPLINK")
			wantedmat[i,'ACCESSION'] <- substring(grep('ACCESSION:',record, value = TRUE, fixed = TRUE),12)
			chnames <- list()
			chnames <- as.list(substring(grep('CH$NAME:',record, value = TRUE, fixed = TRUE),10))
			wantedmat[i,'NAME'] <- chnames[[1]]
			wantedmat[i,'SMILES'] <- substring(grep('CH$SMILES:',record, value = TRUE, fixed = TRUE),12)
			wantedmat[i,'EXACT_MASS'] <- substring(grep('CH$EXACT_MASS',record, value = TRUE, fixed = TRUE),16)
			wantedmat[i,'ION_MODE'] <- substring(grep('AC$MASS_SPECTROMETRY: ION_MODE',record, value = TRUE, fixed = TRUE),31)
			wantedmat[i,'FORMULA'] <- substring(grep('CH$FORMULA:',record, value = TRUE, fixed = TRUE),13)
			wantedmat[i,'LICENSE'] <- substring(grep('LICENSE:',record, value = TRUE, fixed = TRUE),10)
      wantedmat[i,'IUPAC'] <- substring(grep('CH$IUPAC:',record, value = TRUE, fixed = TRUE),11)
			
      ## The next lines check if field is NA or not (for optional fields)
      #ifelse(is.na(INCHIKEYTRUE) == TRUE, wantedmat[i,'INCHIKEY'] <- INCHIKEYFALSE, wantedmat[i,'INCHIKEY'] <- substring(grep('CH$LINK: INCHIKEY',record, value = TRUE, fixed = TRUE),19))
			#ifelse(is.na(CSIDTRUE) == TRUE, wantedmat[i,'CSID'] <- CSIDFALSE, wantedmat[i,'CSID'] <- substring(grep('CH$LINK: CHEMSPIDER',record, value = TRUE, fixed = TRUE),21))
      ifelse(length(INCHIKEYTRUE)==1, wantedmat[i,'INCHIKEY'] <- substring(grep('CH$LINK: INCHIKEY',record, value = TRUE, fixed = TRUE),19), wantedmat[i,'INCHIKEY'] <- INCHIKEYFALSE)
  		ifelse(length(CSIDTRUE)==1, wantedmat[i,'CSID'] <- substring(grep('CH$LINK: CHEMSPIDER',record, value = TRUE, fixed = TRUE),21), wantedmat[i,'CSID'] <- CSIDFALSE)
      
			wantedmat[i,'EULINK'] <- paste("http://massbank.eu/MassBank/jsp/FwdRecord.jsp?id=", substring(grep('ACCESSION:',record, value = TRUE, fixed = TRUE),12), sep="")
      wantedmat[i,'JPLINK'] <- paste("http://www.massbank.jp/jsp/FwdRecord.jsp?id=", substring(grep('ACCESSION:',record, value = TRUE, fixed = TRUE),12), sep="")
		}
		write.csv(wantedmat,csvname)
		return("Successfully wrote the csv")
}

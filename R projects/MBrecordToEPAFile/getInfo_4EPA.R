## Script by Erik Müller, Tobias Schulze, Emma Schymanski
## To retrieve information from MassBank records and create summary CSV
## Modified for EPA, including some checks
## E. Schymanski, 5/10/2016
## LICENSE: GPL 3.0


# get InChI Keys using Open Babel
# need to define the directory containing Open Babel, specifically "obabel.exe"
# This does not work if the path contains a space; the exe can be pasted anywhere
# batch conversion would be faster, but stops at first error without warning.
smiles.to.inchikey <- function(smiles, babel_dir) {
  cmd <- paste(babel_dir, "/obabel -:", smiles, " -oinchikey", sep="")
  res <- system(cmd, intern=TRUE, ignore.stderr=TRUE)
  return(res)
}
inchi.to.inchikey <- function(inchi, babel_dir) {
  cmd <- paste(babel_dir, "/obabel -:", inchi, " -oinchikey", sep="")
  res <- system(cmd, intern=TRUE, ignore.stderr=TRUE)
  return(res)
}

##Directory is the name of the directory
##csvname is the designated name that the csv-file will get
getInfo.EPA <- function(Directory,csvname){
		Files <- list.files(Directory, pattern="*.txt", full.names=TRUE, recursive=TRUE)
    #recursive=TRUE should get all sub-dirs
    #need to add pattern to skip the mols and tsvs
		wantedmat <- matrix(0,length(Files),(22))
		for(i in 1:length(Files)){
			fileConnection <- file(normalizePath(Files[i]))
			record <- readLines(fileConnection)
			close(fileConnection)
      
      ## Check if fields contain NAs
			CSIDTRUE <- grep('CH$LINK: CHEMSPIDER',record, value = TRUE, fixed = TRUE)
			CSIDFALSE <- "N/A"
      
			PCIDTRUE <- grep('CH$LINK: PUBCHEM CID:',record, value = TRUE, fixed = TRUE)
			PCIDFALSE <- "N/A"
			
			CASTRUE <- grep('CH$LINK: CAS',record, value = TRUE, fixed = TRUE)
			CASFALSE <- "N/A"
			
			INCHIKEYTRUE <- grep('CH$LINK: INCHIKEY',record, value = TRUE, fixed = TRUE)
      INCHIKEYFALSE <- "N/A"
      
#       RTTRUE <- grep('AC$CHROMATOGRAPHY: RETENTION_TIME ',record, value = TRUE, fixed = TRUE)
#       RTFALSE <- "N/A"
      #rt <- substring(grep('AC$CHROMATOGRAPHY: RETENTION_TIME ',record, value = TRUE, fixed = TRUE),34)
      PRECMZTRUE <- grep('MS$FOCUSED_ION: PRECURSOR_M/Z ',record, value = TRUE, fixed = TRUE)
      PRECMZFALSE <- "N/A"
      PRECTYPETRUE <- grep('MS$FOCUSED_ION: PRECURSOR_TYPE ',record, value = TRUE, fixed = TRUE)
      PRECTYPEFALSE <- "N/A"
      IONIZNTRUE <- grep('AC$MASS_SPECTROMETRY: IONIZATION ',record, value = TRUE, fixed = TRUE)
      IONIZNFALSE <- "N/A"
      
			
			colnames(wantedmat) <- c("ACCESSION","NAME", "NAME_B", "NAME_C", "SMILES","EXACT_MASS","PRECURSOR_MZ",
			                         "ION_MODE","FORMULA",
			                         "INSTR","INSTR_TYPE","MS_TYPE","IONIZATION","PREC_TYPE",
			                         "LICENSE","IUPAC","INCHIKEY","CSID","PC_CID","CAS_RN","EULINK","JPLINK")
			wantedmat[i,'ACCESSION'] <- substring(grep('ACCESSION:',record, value = TRUE, fixed = TRUE),12)
			# Print out all names now
			chnames <- list()
			chnames <- as.list(substring(grep('CH$NAME:',record, value = TRUE, fixed = TRUE),10))
			wantedmat[i,'NAME'] <- chnames[[1]]
			if (length(chnames)>2) {
			  wantedmat[i,'NAME_B'] <- chnames[[2]]
			  wantedmat[i,'NAME_C'] <- chnames[[3]]
			} else if (length(chnames)>1) {
			  wantedmat[i,'NAME_B'] <- chnames[[2]]
			  wantedmat[i,'NAME_C'] <- ""
			} else {
			  wantedmat[i,'NAME_B'] <- ""
			  wantedmat[i,'NAME_C'] <- ""
			}
			 
			wantedmat[i,'SMILES'] <- substring(grep('CH$SMILES:',record, value = TRUE, fixed = TRUE),12)
			wantedmat[i,'EXACT_MASS'] <- substring(grep('CH$EXACT_MASS',record, value = TRUE, fixed = TRUE),16)
#			wantedmat[i,'PRECURSOR_MZ'] <- substring(grep('MS$FOCUSED_ION: PRECURSOR_M/Z',record, value = TRUE, fixed = TRUE),31)
#			wantedmat[i,'PREC_TYPE'] <- substring(grep('MS$FOCUSED_ION: PRECURSOR_TYPE',record, value = TRUE, fixed = TRUE),32)
			wantedmat[i,'ION_MODE'] <- substring(grep('AC$MASS_SPECTROMETRY: ION_MODE',record, value = TRUE, fixed = TRUE),31)
			wantedmat[i,'MS_TYPE'] <- substring(grep('AC$MASS_SPECTROMETRY: MS_TYPE',record, value = TRUE, fixed = TRUE),30)
#			wantedmat[i,'IONIZATION'] <- substring(grep('AC$MASS_SPECTROMETRY: IONIZATION',record, value = TRUE, fixed = TRUE),33)
			wantedmat[i,'FORMULA'] <- substring(grep('CH$FORMULA:',record, value = TRUE, fixed = TRUE),13)
			wantedmat[i,'LICENSE'] <- substring(grep('LICENSE:',record, value = TRUE, fixed = TRUE),10)
      wantedmat[i,'IUPAC'] <- substring(grep('CH$IUPAC:',record, value = TRUE, fixed = TRUE),11)
      wantedmat[i,'INSTR'] <- substring(grep('AC$INSTRUMENT:',record, value = TRUE, fixed = TRUE),16)
      wantedmat[i,'INSTR_TYPE'] <- substring(grep('AC$INSTRUMENT_TYPE:',record, value = TRUE, fixed = TRUE),21)
      
      ## The next lines check if field is NA or not (for optional fields)
      #ifelse(is.na(INCHIKEYTRUE) == TRUE, wantedmat[i,'INCHIKEY'] <- INCHIKEYFALSE, wantedmat[i,'INCHIKEY'] <- substring(grep('CH$LINK: INCHIKEY',record, value = TRUE, fixed = TRUE),19))
			#ifelse(is.na(CSIDTRUE) == TRUE, wantedmat[i,'CSID'] <- CSIDFALSE, wantedmat[i,'CSID'] <- substring(grep('CH$LINK: CHEMSPIDER',record, value = TRUE, fixed = TRUE),21))
      ifelse(length(INCHIKEYTRUE)==1, wantedmat[i,'INCHIKEY'] <- substring(grep('CH$LINK: INCHIKEY',record, value = TRUE, fixed = TRUE),19), wantedmat[i,'INCHIKEY'] <- INCHIKEYFALSE)
  		ifelse(length(CSIDTRUE)==1, wantedmat[i,'CSID'] <- substring(grep('CH$LINK: CHEMSPIDER',record, value = TRUE, fixed = TRUE),21), wantedmat[i,'CSID'] <- CSIDFALSE)
  		ifelse(length(CASTRUE)==1, wantedmat[i,'CAS_RN'] <- substring(grep('CH$LINK: CAS',record, value = TRUE, fixed = TRUE),14), wantedmat[i,'CAS_RN'] <- CASFALSE)
  		ifelse(length(PCIDTRUE)==1, wantedmat[i,'PC_CID'] <- substring(grep('CH$LINK: PUBCHEM CID:',record, value = TRUE, fixed = TRUE),22), wantedmat[i,'PC_CID'] <- PCIDFALSE)
#  		ifelse(length(RTTRUE)==1, wantedmat[i,'RT'] <- sub(" min","",substring(grep('AC$CHROMATOGRAPHY: RETENTION_TIME ',record, value = TRUE, fixed = TRUE),35)), wantedmat[i,'RT'] <- RTFALSE)
  		ifelse(length(PRECMZTRUE)==1, wantedmat[i,'PRECURSOR_MZ'] <- substring(grep('MS$FOCUSED_ION: PRECURSOR_M/Z',record, value = TRUE, fixed = TRUE),31), wantedmat[i,'PRECURSOR_MZ'] <- PRECMZFALSE)
  		ifelse(length(PRECTYPETRUE)==1, wantedmat[i,'PREC_TYPE'] <- substring(grep('MS$FOCUSED_ION: PRECURSOR_TYPE',record, value = TRUE, fixed = TRUE),32), wantedmat[i,'PREC_TYPE'] <- PRECTYPEFALSE)
  		ifelse(length(IONIZNTRUE)==1, wantedmat[i,'IONIZATION'] <- substring(grep('AC$MASS_SPECTROMETRY: IONIZATION',record, value = TRUE, fixed = TRUE),33), wantedmat[i,'IONIZATION'] <- IONIZNFALSE)
  		
			wantedmat[i,'EULINK'] <- paste("http://massbank.eu/MassBank/jsp/FwdRecord.jsp?id=", substring(grep('ACCESSION:',record, value = TRUE, fixed = TRUE),12), sep="")
      wantedmat[i,'JPLINK'] <- paste("http://www.massbank.jp/jsp/FwdRecord.jsp?id=", substring(grep('ACCESSION:',record, value = TRUE, fixed = TRUE),12), sep="")
		}
		write.csv(wantedmat,csvname, row.names=F)
		return("Successfully wrote the csv")
}

fillInfo.EPA <- function(csv_in, csvname, babel_dir) {
  info <- read.csv(csv_in)
}
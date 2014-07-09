##Script by Erik Müller, Tobias Schulze, Emma Schymanski
## Edited 7.7.14 E. Schymanski (adding InChI Key functionality)
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
      INCHIKEY <- ifelse(length(INCHIKEYTRUE)==1, substring(grep('CH$LINK: INCHIKEY',record, value = TRUE, fixed = TRUE),19), INCHIKEYFALSE)
      SMILES <- substring(grep('CH$SMILES:',record, value = TRUE, fixed = TRUE),12)
      SMILES_NA <- grep("N/A",SMILES, value = TRUE, fixed = TRUE)
      INCHIKEY_NA <- grep("N/A",INCHIKEY, value = TRUE, fixed = TRUE)
      #fill in missing InChI Key where possible with Open Babel conversion from SMILES
      # Can only attempt this if SMILES exists; takes a while so don't recalculate unless necessary
      if((length(INCHIKEY_NA)==1)&&(length(SMILES_NA)!=1)) {
        new_inchikey <- create.inchikey(SMILES, babel_dir)
        if(length(new_inchikey)>=1) {
          INCHIKEY <- new_inchikey
        }
      }
			
			colnames(wantedmat) <- c("ACCESSION","NAME","SMILES","EXACT_MASS","ION_MODE","FORMULA","LICENSE","IUPAC","INCHIKEY","CSID","EULINK","JPLINK")
			wantedmat[i,'ACCESSION'] <- substring(grep('ACCESSION:',record, value = TRUE, fixed = TRUE),12)
			chnames <- list()
			chnames <- as.list(substring(grep('CH$NAME:',record, value = TRUE, fixed = TRUE),10))
			wantedmat[i,'NAME'] <- chnames[[1]]
			#wantedmat[i,'SMILES'] <- substring(grep('CH$SMILES:',record, value = TRUE, fixed = TRUE),12)
      wantedmat[i,'SMILES'] <- SMILES
			wantedmat[i,'EXACT_MASS'] <- substring(grep('CH$EXACT_MASS',record, value = TRUE, fixed = TRUE),16)
			wantedmat[i,'ION_MODE'] <- substring(grep('AC$MASS_SPECTROMETRY: ION_MODE',record, value = TRUE, fixed = TRUE),31)
			wantedmat[i,'FORMULA'] <- substring(grep('CH$FORMULA:',record, value = TRUE, fixed = TRUE),13)
			wantedmat[i,'LICENSE'] <- substring(grep('LICENSE:',record, value = TRUE, fixed = TRUE),10)
      wantedmat[i,'IUPAC'] <- substring(grep('CH$IUPAC:',record, value = TRUE, fixed = TRUE),11)
      wantedmat[i,'INCHIKEY'] <- INCHIKEY
			
      ## The next lines check if field is NA or not (for optional fields)
      #ifelse(is.na(INCHIKEYTRUE) == TRUE, wantedmat[i,'INCHIKEY'] <- INCHIKEYFALSE, wantedmat[i,'INCHIKEY'] <- substring(grep('CH$LINK: INCHIKEY',record, value = TRUE, fixed = TRUE),19))
			#ifelse(is.na(CSIDTRUE) == TRUE, wantedmat[i,'CSID'] <- CSIDFALSE, wantedmat[i,'CSID'] <- substring(grep('CH$LINK: CHEMSPIDER',record, value = TRUE, fixed = TRUE),21))
      #INCHIKEY <- ifelse(length(INCHIKEYTRUE)==1, wantedmat[i,'INCHIKEY'] <- substring(grep('CH$LINK: INCHIKEY',record, value = TRUE, fixed = TRUE),19), wantedmat[i,'INCHIKEY'] <- INCHIKEYFALSE)
      ifelse(length(CSIDTRUE)==1, wantedmat[i,'CSID'] <- substring(grep('CH$LINK: CHEMSPIDER',record, value = TRUE, fixed = TRUE),21), wantedmat[i,'CSID'] <- CSIDFALSE)
      
			wantedmat[i,'EULINK'] <- paste("http://massbank.eu/MassBank/jsp/FwdRecord.jsp?id=", substring(grep('ACCESSION:',record, value = TRUE, fixed = TRUE),12))
      wantedmat[i,'JPLINK'] <- paste("http://www.massbank.jp/jsp/FwdRecord.jsp?id=", substring(grep('ACCESSION:',record, value = TRUE, fixed = TRUE),12))
		}
		write.csv(wantedmat,csvname)
		return("Successfully wrote the csv")
}

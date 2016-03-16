.addLineToRecord <- function(path, section, value, tail=TRUE, newDate=TRUE){
	
	# If the file does not exist, stop
	if(!file.exists(path)){
		stop("The supplied record path does not exist")
	}
    
	# Extract first two letters of section, in case it's "CH", "AC", "MS" or "PK"
	# And check if the section is valid
	secLetters <- substr(section,1,2)
	if(!(section == "COMMENT") && !(secLetters  %in% c("CH", "AC", "MS", "PK"))){
		stop("Unknown section")
	}
	
	# File stuff, read lines
	recConn <- file(path, open="r+")
	recLines <- readLines(recConn)
	
	if(paste0(section, ": ", value) %in% recLines){
		warning("The specified line is already in the record, no change made")
		close(recConn)
		return(0)
	}
	

	
	# Where to insert the new line? First try: Just search for the section name
	# And if that exists, note the line number
    if(tail){
        SectionLine <- tail(grep(section, recLines, fixed = TRUE),1)
	} else{
        SectionLine <- head(grep(section, recLines, fixed = TRUE),1)
    }
    print(SectionLine)
	# If section does not exist, differentiate between COMMENT and AC/MS/CH
	if(!length(SectionLine)){
		if(section == "COMMENT"){
			# Order of important sections before COMMENT
			# PUBLICATION and "COPYRIGHT are optional, but must be in a certain order
			# Comment must come after PUBLICATION if it's there, else it comes after COPYRIGHT, 
			# else it comes after LICENSE.
			
			# If section is COMMENT, search for PUBLICATION, COPYRIGHT, LICENSE in that order
			# (hence the 3:1) and note the line number
			i <- 1
			while(!length(SectionLine)){
				SectionLine <- tail(grep(c("PUBLICATION", "COPYRIGHT", "LICENSE")[i], recLines, fixed = TRUE),1)
				i <- i + 1
			}
		}else{
				# Search for 2-letter prefix and note the final line of that section
                if(tail){
                    SectionLine <- tail(grep(paste0(secLetters,"$"), recLines, fixed = TRUE),1)
				} else{
                    SectionLine <- head(grep(paste0(secLetters,"$"), recLines, fixed = TRUE),1)
                }
                
				# If section does not exist, it must be "MS", so add it after "AC"
				if(!length(SectionLine)){
					SectionLine <- tail(grep("AC$", recLines, fixed = TRUE),1)
				}
		}
	}
    if(tail){
        recLines <- c(recLines[1:SectionLine], paste0(section, ": ", value), recLines[(SectionLine + 1):length(recLines)])
    } else{
        recLines <- c(recLines[1:(SectionLine-1)], paste0(section, ": ", value), recLines[(SectionLine):length(recLines)])
    }
	writeLines(recLines,recConn)
	close(recConn)
}

.addSplashToRecord <- function(path){
	require(RMassBank)
	if(file.info(path[1])$isdir){
	    Files <- list.files(path = path,
                                recursive=TRUE, 
                                full.names = TRUE)
	} else {Files <- path}
	
	pb <- txtProgressBar(min = 0, max = length(Files), initial = 0, style=3, char = "=")
	
	spEnv <- new.env()
	spEnv$i <- 0
    sapply(Files, function(f){
        C <- capture.output(a <- parseMassBank(f))
        peaks <- a@compiled_ok[[1]][['PK$PEAK']][,c("m/z","int")]
        .addLineToRecord(f, "PK$SPLASH", RMassBank:::getSplash(peaks), FALSE)
        spEnv$i <- spEnv$i + 1
        setTxtProgressBar(pb, spEnv$i)
    })
	return(1)
}

changeRecordDate <- function(recpath){
    
    if(file.info(recpath[1])$isdir){
	    Files <- list.files(path = recpath,
                                recursive=TRUE, 
                                full.names = TRUE)
	} else {Files <- recpath}
    
    if(!file.exists(recpath)){
        stop("The supplied record path does not exist")
	}
    
    
    sapply(Files, function(x){
        recConn <- file(x, open="r+")
        recLines <- readLines(recConn)
        
        ## Change Date:
        Dateline <- grep("DATE:", recLines, fixed=TRUE)[1]
        openBracket <- regexpr("(",recLines[Dateline],fixed=TRUE)
        currentDate <- format(Sys.time(), "%Y.%m.%d")
        if(openBracket != -1){
            closeBracket <- regexpr(")",recLines[Dateline],fixed=TRUE)
            inBracket <- substr(recLines[Dateline], openBracket+1,closeBracket-1)
            oldDate <- substr(recLines[Dateline],7,openBracket-1)
            oldDate <- gsub(" ", "", oldDate, fixed = TRUE)
            dateLineContent <- paste("DATE:", currentDate, paste0("(",inBracket,", modified ", oldDate, ")"))
        } else{
            oldDate <- substr(recLines[Dateline],7,nchar(recLines[Dateline]))
            dateLineContent <- paste("DATE:", currentDate, paste0("(Created ",oldDate,")"))
        }
        recLines[Dateline] <- dateLineContent
        writeLines(recLines,recConn)
        close(recConn)
    })
    return(1)
}

changeRecordLCSettings <- function(recpath, settings){
  # settings must be a vector of gradient, flow, column, solvent a, b and c
  
  if(file.info(recpath[1])$isdir){
    Files <- list.files(path = recpath,
                        recursive=TRUE, 
                        full.names = TRUE)
  } else {Files <- recpath}
  
  if(!file.exists(recpath)){
    stop("The supplied record path does not exist")
  }
  
  
  sapply(Files, function(x){
    recConn <- file(x, open="r+")
    recLines <- readLines(recConn)
    close(recConn)
    # find lines to change
    lc_gradient_i <- grep("AC$CHROMATOGRAPHY: FLOW_GRADIENT", recLines, fixed=TRUE)[1]
    lc_flow_i <- grep("AC$CHROMATOGRAPHY: FLOW_RATE", recLines, fixed=TRUE)[1]
    lc_column_i <- grep("AC$CHROMATOGRAPHY: COLUMN_NAME", recLines, fixed=TRUE)[1]
    lc_solventa_i <- grep("AC$CHROMATOGRAPHY: SOLVENT A", recLines, fixed=TRUE)[1]
    lc_solventb_i <- grep("AC$CHROMATOGRAPHY: SOLVENT B", recLines, fixed=TRUE)[1]
    lc_solventc_i <- grep("AC$CHROMATOGRAPHY: SOLVENT C", recLines, fixed=TRUE)[1]
    # add new settings
    lc_gradient <- paste("AC$CHROMATOGRAPHY: FLOW_GRADIENT", settings[1])
    lc_flow <- paste("AC$CHROMATOGRAPHY: FLOW_RATE", settings[2])
    lc_column <- paste("AC$CHROMATOGRAPHY: COLUMN_NAME", settings[3])
    lc_solventa <- paste("AC$CHROMATOGRAPHY: SOLVENT A", settings[4])
    lc_solventb <- paste("AC$CHROMATOGRAPHY: SOLVENT B", settings[5])
    lc_solventc <- paste("AC$CHROMATOGRAPHY: SOLVENT C", settings[6])
    # add new lines
    recLines[lc_gradient_i] <- lc_gradient
    recLines[lc_flow_i] <- lc_flow
    recLines[lc_column_i] <- lc_column
    recLines[lc_solventa_i] <- lc_solventa
    recLines[lc_solventb_i] <- lc_solventb
    if (!is.na(lc_solventc_i)) {
      recLines[lc_solventc_i] <- lc_solventc
    } else {
      recLines[lc_solventb_i] <- paste(lc_solventb,lc_solventc,sep="\n")
      #.addLineToRecord(x,section = "AC$CHROMATOGRAPHY: SOLVENT C",value = settings[6],newDate = FALSE,tail = FALSE)
    }
    recConn <- file(x, open="r+")
    writeLines(recLines,recConn)
    close(recConn)
  })
  return(1)
}


changePublicationSettings <- function(recpath, publ_text){
  # publ_text must be a string containing the information to come after "PUBLICATION: "
  # copy records into a new dir first!
  if(file.info(recpath[1])$isdir){
    Files <- list.files(path = recpath,
                        recursive=TRUE, 
                        full.names = TRUE)
  } else {Files <- recpath}
  
  if(!file.exists(recpath)){
    stop("The supplied record path does not exist")
  }
  
  
  sapply(Files, function(x){
    recConn <- file(x, open="r+")
    recLines <- readLines(recConn)
    close(recConn)
    
    # find lines to change
    publication_i <- grep("PUBLICATION: ", recLines, fixed=TRUE)[1]
    copyright_i <- grep("COPYRIGHT: ", recLines, fixed=TRUE)[1]
    # add new settings
    publication <- paste("PUBLICATION: ", publ_text,sep="")
    copyright <- recLines[copyright_i]
    # add new lines
    if (!is.na(publication_i)) {
      recLines[publication_i] <- publication
    } else {
      recLines[copyright_i] <- paste(copyright,publication,sep="\n")
      #.addLineToRecord(x,section = "PUBLICATION:",value = publ_text,newDate = FALSE,tail = FALSE)
    }
    recConn <- file(x, open="r+")
    writeLines(recLines,recConn)
    close(recConn)
  })
  return(1)
}

# this fixed the temporary SPLASH bug with PK$SPLASH SPLASH splash10 - no longer needed
changeSPLASHSettings <- function(recpath, splash_text){
  # splash_text must be a string containing the text to delete in the SPLASH entry"
  if(file.info(recpath[1])$isdir){
    Files <- list.files(path = recpath,
                        recursive=TRUE, 
                        full.names = TRUE)
  } else {Files <- recpath}
  
  if(!file.exists(recpath)){
    stop("The supplied record path does not exist")
  }

  sapply(Files, function(x){
    recConn <- file(x, open="r+")
    recLines <- readLines(recConn)
    n_lines <- length(recLines)
    close(recConn)
    
    # find lines to change
    splash_i <- grep("PK$SPLASH: splash", recLines, fixed=TRUE)[1]
    splash_ii <- grep("PK$SPLASH: SPLASH splash", recLines, fixed=TRUE)[1]
#     # add new settings
#     splash_entry <- sub(splash_text,"", recLines[splash_i],fixed=TRUE)
    #copyright <- recLines[copyright_i]
    # add new lines
    if (!is.na(splash_ii)) {
      recLines <- recLines[-splash_ii]
    } 
    recConn <- file(x, open="wt")
    writeLines(recLines,recConn)
    #writeLines(recLines[1:(length(recLines)-1)],recConn)
    close(recConn)
  })
  return(1)
}

# Note this needs the babel dir from the RMassBank settings (not just dir location) as it uses an RMB internal function.
# Also, had a bug one molfile (created NA.mol) but not reproducible
generateMoldataFromRecords <- function(recpath, molpath, babel_dir = getOption("RMassBank")$babeldir){
  # will read files in recpath and create mols and list.tsv in molpath
  if(file.info(recpath[1])$isdir){
    Files <- list.files(path = recpath,
                        recursive=FALSE, 
                        full.names = TRUE)
  } else {Files <- recpath}
  
  if(!file.exists(recpath)){
    stop("The supplied record path does not exist")
  }
  # create and open list.tsv
  list_path <- paste(molpath,"/list.tsv",sep="")
  listConn <- file(list_path,open="at")
  # read records
  sapply(Files, function(x){
    recConn <- file(x, open="r+")
    recLines <- readLines(recConn)
    close(recConn)
    
    # find name, ID and SMILES of the substance
    name_i <- grep("CH$NAME: ", recLines, fixed=TRUE)[1]
    id_i <- grep("COMMENT: INTERNAL_ID ", recLines, fixed=TRUE)[1]
    smiles_i <- grep("CH$SMILES: ", recLines, fixed=TRUE)[1]
    # get name and ID
    rec_name <- sub("CH$NAME: ","", recLines[name_i],fixed=TRUE)
    rec_id <- sub("COMMENT: INTERNAL_ID ","", recLines[id_i],fixed=TRUE)
    #rec_smiles <- sub("CH$SMILES ","", recLines[smiles_i],fixed=TRUE)
    rec_smiles <- substring(recLines[smiles_i],12)
    molfile_name <- paste(rec_id,".mol",sep="")
    molfile_path <- paste(molpath,"/",molfile_name,sep="")
    molfiles <- list.files(molpath)
    molfile_there <- grep(molfile_name,molfiles)
    # generate molfiles
    if (nchar(rec_smiles)>3 && length(molfile_there)==0) {
      list_entry <- paste(rec_name,molfile_name,sep="\t")
      writeLines(list_entry,listConn)
      createMolfile(rec_smiles,molfile_path)
    } 
  })
  close(listConn)
  return(1)
  
}


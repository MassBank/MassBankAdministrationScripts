# Script to add SPLASH line to existing MassBank records
# Erik Müller
# Copyright (c) 2016-2017
# LICENSE: GPL 3.0

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
require("splashR")
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
        .addLineToRecord(f, "PK$SPLASH", splashR::getSplash(peaks),F,F)
        spEnv$i <- spEnv$i + 1
        setTxtProgressBar(pb, spEnv$i)
    })
	return(1)
}

.changeRecordDate <- function(recpath){
    
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
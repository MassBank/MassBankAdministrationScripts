# Generic script to update the publication record and add a location
# This script could be generalised to fixing any records if necessary.
# E. Schymanski, 25/6/2014, generalized 7/7/14

# Record path for records to be fixed:
rec_dir <- "C:/DATA/my_rec_dir"
new_rec_dir <- "C:/DATA/new_rec_dir/recdata"
rec_files <- list.files(rec_dir, full.names = TRUE)

#set the values we need to change:
new_file_name <- ""
info <- ""
mb_line <- ""
publicn <- "Authors (Date) Journal, vol:pages, DOI:.... Title at the end because it is so long..."
new_date <- "DATE: YYYY.MM.DD"
location <- "GAZ:00000000 GAZ:00000000"
sample_comment <- "Samples A,B,C collected from XXX (GAZ:00000000), samples D, E, F from YYY (GAZ:00000000)"
#if only certain records need locations, adjust the numbers below

#test for a few files first:
#rec_files <- rec_files[1:3]

#note: this updates a publication entry; different code is needed to ADD a publication.
for (i in 1:length(rec_files)) {
  info <- readLines(rec_files[i])
  new_file_name <- file.path(new_rec_dir,basename(rec_files[i]))
  file.con <- file(new_file_name,"w")
  #cat("ACCESSION: ", record_name, "\n", file=file.con, sep="")
  #cat("COMMENT: Source; ",files[i],"\n", file=file.con, sep="")
  #cat("PK$NUM_PEAK: ", length(peaks$mz),"\n", file=file.con, sep="")
  #cat("PK$PEAK: m/z int. rel.int.", "\n", file=file.con, sep="")
  for (j in 1:length(info)) {
    if(length(grep("DATE: ",info[j], fixed=TRUE))==1) {
      info[j] <- new_date
      writeLines(info[j],file.con)
    } else if(length(grep("PUBLICATION: ",info[j], fixed=TRUE))==1) {
      info[j] <- paste("PUBLICATION: ", publicn, sep="")
      writeLines(info[j],file.con)
    } else if(length(grep("AC$INSTRUMENT: ",info[j], fixed=TRUE))==1) {
      #only write out location for the samples, not for reference standards
      #in this case, this is the first 15 records
      if(i<16) {
        locn_entry <- paste("SP$SAMPLE: LOCATION ", location, sep="")
        writeLines(locn_entry, file.con)
        locn_comment <- paste("SP$SAMPLE: COMMENT ", sample_comment, sep="")
        writeLines(locn_comment, file.con)
      }
      writeLines(info[j],file.con)
    } else {
      writeLines(info[j], file.con)
    }
  }
  close(file.con)
}

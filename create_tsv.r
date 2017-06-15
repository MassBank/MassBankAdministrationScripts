library(stringr)

Directory <- "D:/MassBank/MassBank_170414/DB/molfile/Funatsu_backup"
Files <- list.files(Directory, pattern="*.mol", full.names=TRUE, recursive=TRUE)
Filename <- list.files(Directory, pattern="*.mol", full.names=FALSE, recursive=TRUE)

list <- list()

for(i in 1:length(Files)){
  fileConnection <- file(normalizePath(Files[i]))
  record <- readLines(fileConnection, n = 1)
  Compoundname <- strsplit(record, "\\.")[[1]][1]
  PS <- paste(Compoundname, Filename[i], sep="#")
  list <- append(list,PS)
  close(fileConnection)
}

list <- as.character(list)
dim(list)
list
write.csv(list, paste0(Directory,"/list.csv"), sep = TRUE)
)

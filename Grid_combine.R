### Combine CCSRA and murSST (grid) extractions into one csv file
# Trys to concatenate files based on names of files in path.ccsra
# If necessary, users should update file paths


## User path descriptions
# path.ccsra:  Folder with extracted ccsra data, with one csv file per day
# path.mursst: Folder with extracted mursst data, with one csv file per day
# path.out:    Folder to write combined, extracted data as single csv file

source("User_script_local.R", local = TRUE, echo = FALSE)
if (user == "KAF") {
  path.ccsra  <- ""
  path.mursst  <- ""
  path.out <- ""
  
} else if (user == "EAB") {
  path.ccsra  <- ""
  path.mursst  <- ""
  path.out <- ""
  
} else if (user == "SMW") {
  path.ccsra <- "../whale-model-prep_data/Grid/Grid_CCSRA/"
  path.mursst <- "../whale-model-prep_data/Grid/Grid_murSST/"
  path.out <- "../whale-model-prep_data/Grid/Grid_all/"
  
} else {
  stop("Invalid value supplied for 'user' object")
}


### Loop through and combine data from files
files.ccsra <- list.files(path.ccsra, full.names = TRUE)
files.mursst <- list.files(path.mursst, full.names = TRUE)

substrRight <- function(x, n) substr(x, nchar(x) - n + 1, nchar(x))

# d <- Sys.time()
for (i in seq_along(files.ccsra)) {
  curr.ccsra.date <- substrRight(files.ccsra[i], 14)
  curr.mursst.date <- substrRight(files.mursst[i], 14)
  
  
  if (file.exists(paste0(path.out, "WEAR_3km_", curr.ccsra.date))) {
    print(paste0(i, ": Already exists: ", files.ccsra[i]))
    
  } else {
    print(paste0(i, ": ", curr.ccsra.date))
    
    curr.ccsra <- read.csv(files.ccsra[i])
    curr.mursst <- read.csv(files.mursst[i])
    
    # Lon and lat columns are reversed in extracted data - Sam's whoops
    stopifnot(
      names(curr.ccsra)[1:2] == c("lat", "lon"), 
      names(curr.mursst)[1:2] == c("lon", "lat")
    )
    if (!identical(curr.ccsra[, c(2, 1)], curr.mursst[, c(1, 2)]) || 
        !identical(curr.ccsra.date, curr.mursst.date)) {
      stop("Idx ", i, " non-identical lat/lon error")
    }
    
    write.csv(
      cbind(curr.ccsra, curr.mursst[, -c(1, 2)]), row.names = FALSE, 
      file = paste0(path.out, "WEAR_3km_", curr.ccsra.date)
    )
    
    rm(curr.ccsra, curr.mursst)
  }
  rm(curr.ccsra.date, curr.mursst.date)
}; rm(i)
# Sys.time() - d

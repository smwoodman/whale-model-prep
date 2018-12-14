### Combine CCSRA and murSST (grid) extractions into one .csv file

substrRight <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}


path.ccsra <- "../whale-model-prep_data/Grid/Grid_CCSRA/"
path.mursst <- "../whale-model-prep_data/Grid/Grid_murSST/"

files.ccsra <- list.files(path.ccsra, full.names = TRUE)
files.mursst <- list.files(path.mursst, full.names = TRUE)

# d <- Sys.time()
for (i in seq_along(files.ccsra)) {
  print(i)
  curr.ccsra <- read.csv(files.ccsra[i])
  curr.mursst <- read.csv(files.mursst[i])
  
  curr.ccsra.date <- substrRight(files.ccsra[i], 14)
  curr.mursst.date <- substrRight(files.mursst[i], 14)
  
  if (!identical(curr.ccsra[, c("lat", "lon")], curr.mursst[, c("lat", "lon")]) || 
      !identical(curr.ccsra.date, curr.mursst.date)) {
    stop("Idx ", i, " non-identical error")
  }
  
  write.csv(
    cbind(curr.ccsra, curr.mursst[, -c(1:2)]), row.names = FALSE, 
    file = paste0("../whale-model-prep_data/Grid/Grid_all/WEAR_3km_", curr.ccsra.date)
  )
  rm(curr.ccsra, curr.mursst, curr.ccsra.date, curr.mursst.date)
}; rm(i)
# Sys.time() - d

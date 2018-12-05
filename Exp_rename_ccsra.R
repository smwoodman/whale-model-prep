# Rename CCSRA Excel files extracted by JVR

files.path <- "../whale-model-prep_data/Grid/Grid_CCSRA_JVR"
files.all <- list.files(files.path)

for(i in files.all) {
  file.rename(paste0(files.path, "/", i), paste0(files.path, "/", substr(i, 11, nchar(i))))
}; rm(i)

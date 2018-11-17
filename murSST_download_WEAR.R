# Code to generate urls for downloading murSST nc files for WEAR whale modeling,
#   and code to actually download files
# By Sam Woodman

###############################################################################
# NC FILES NEEDED lat/lon DIMENSIONS
# Lon range: -132, -116 (228, 244)
# Lat range: 29, 49


###############################################################################
# Example url (2016):
# http://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?
# analysed_sst[(2016-01-01T09:00:00Z):1:(2016-12-31T09:00:00Z)][(35.5):1:(39.1)][(-124):1:(-121)],
# analysis_error[(2016-01-01T09:00:00Z):1:(2016-12-31T09:00:00Z)][(35.5):1:(39.1)][(-124):1:(-121)],
# mask[(2016-01-01T09:00:00Z):1:(2016-12-31T09:00:00Z)][(35.5):1:(39.1)][(-124):1:(-121)]


###############################################################################
# Single nc file download. Testing
lonmin <- -132
lonmax <- -116
latmin <- 29
latmax <- 49
coords.txt <- paste0("(", paste(lonmin, lonmax, latmin, latmax, sep = ")-("), ")")

start.date <- as.Date("2005-01-01")
end.date   <- as.Date("2005-01-15")

days.gap <- 2

file.name.out <- "exp3.nc"
# file.name.out <- paste0(
#   "mursst_", start.date, "_", end.date, "_", 
#   days.gap, "day_", coords.txt, ".nc"
# )


string.date.loc <- paste0(
  "[(", 
  start.date, "T09:00:00Z):", days.gap, ":(", end.date, "T09:00:00Z)][(", 
  latmin, "):1:(", latmax, ")][(", 
  lonmin, "):1:(", lonmax, ")]"
)

d <- paste0(
  "http://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst", 
  string.date.loc, ",analysis_error", string.date.loc, ",mask", string.date.loc
)

download.file(
  d, destfile = paste0("../whale-model-prep_data/mursst_nc/", file.name.out), 
  method = "auto", quiet = FALSE, mode = "wb", cacheOK = TRUE
)


###############################################################################
# Exp

# library(ncdf4)
# x <- nc_open("../whale-model-prep_data/mursst_nc/exp.nc")
# y <- nc_open("../whale-model-prep_data/mursst_nc/exp2.nc")
# 
# x.t <- ncvar_get(x, "time")
# y.t <- ncvar_get(y, "time")
# 
# as.POSIXct(x.t, origin = "1970-01-01")
# as.POSIXct(y.t, origin = "1970-01-01")


###############################################################################
# Loop for downloading multiple nc files

lonmin <- -132
lonmax <- -116
latmin <- 29
latmax <- 49
coords.txt <- paste0("(", paste(lonmin, lonmax, latmin, latmax, sep = ")-("), ")")

start.date <- as.Date("2005-01-01")
end.date   <- as.Date("2005-01-15")

start.data.mult <- seq(start.date)
end.data.mult   <- seq(end.date)

date.list <- list()

days.gap <- 2



for (i in date.list) {
  string.date.loc <- paste0(
    "[(", 
    start.date, "T09:00:00Z):", days.gap, ":(", end.date, "T09:00:00Z)][(", 
    latmin, "):1:(", latmax, ")][(", 
    lonmin, "):1:(", lonmax, ")]"
  )
  
  url.name <- paste0(
    "http://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst", 
    string.date.loc, ",analysis_error", string.date.loc, ",mask", string.date.loc
  )
  file.name.out <- paste0(
    "mursst_", start.date, "_", end.date, "_",
    days.gap, "day_", coords.txt, ".nc"
  )
  
  download.file(
    url.name, destfile = paste0("../whale-model-prep_data/mursst_nc/", file.name.out), 
    method = "auto", quiet = FALSE, mode = "wb", cacheOK = TRUE
  )
}

###############################################################################

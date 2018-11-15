
# Works for -124, -123; 38, 29.1; 2015-01-01, 2015-06-30
# Doesn't work for -124, -123; 38, 29.1; 2015-01-01, 2015-12-31

lonmin <- -124
lonmax <- -123
latmin <- 38
latmax <- 39.1

start.date <- as.Date("2015-01-01")
end.date   <- as.Date("2015-6-30")


string.date.loc <- paste0(
  "[(", 
  start.date, "T09:00:00Z):1:(", end.date, "T09:00:00Z)][(", 
  latmin, "):1:(", latmax, ")][(", 
  lonmin, "):1:(", lonmax, ")]"
)

d <- paste0(
  "http://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst", 
  string.date.loc, ",analysis_error", string.date.loc, ",mask", string.date.loc
)

download.file(
  d, destfile = "../whale-model-prep_data/exp.nc", 
  method = "auto", quiet = FALSE, mode = "wb", cacheOK = TRUE
)

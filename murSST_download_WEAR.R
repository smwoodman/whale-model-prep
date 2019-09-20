# Code to generate urls for downloading murSST nc files for WEAR whale models,
#   and code to actually download files
# Needed large area (US west coast) and thus had to download one nc file
#   per day every day for 2005-2017.
#   Only downloaded analysed_sst data (not analysis_error or mark).
#   Data stored at "J:\Sam_Woodman\mursst_nc" where "J:\" is "mmdisk". 
# Lots of testing code (commented out) at top of file
#
# By Sam Woodman, November 2018

###############################################################################
###############################################################################
# NC FILES NEEDED lat/lon DIMENSIONS
# Lon range: -132, -116 (228, 244)
# Lat range: 29, 49


###############################################################################
# Example url (2016):
# http://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?
#   analysed_sst[(2016-01-01T09:00:00Z):1:(2016-12-31T09:00:00Z)][(35.5):1:(39.1)][(-124):1:(-121)],
#   analysis_error[(2016-01-01T09:00:00Z):1:(2016-12-31T09:00:00Z)][(35.5):1:(39.1)][(-124):1:(-121)],
#   mask[(2016-01-01T09:00:00Z):1:(2016-12-31T09:00:00Z)][(35.5):1:(39.1)][(-124):1:(-121)]


###############################################################################
# Single nc file download. Code is for testing purposes

# ### Download exp nc file
# lonmin <- -132
# lonmax <- -116
# latmin <- 29
# latmax <- 49
# coords.txt <- paste0("(", paste(lonmin, lonmax, latmin, latmax, sep = ")-("), ")")
# 
# start.date  <- as.Date("2013-01-01")
# end.date    <- as.Date("2013-01-01")
# 
# days.gap <- 1
# 
# string.date.loc <- paste0(
#   "[(", 
#   start.date, "T09:00:00Z):", days.gap, ":(", end.date, "T09:00:00Z)][(", 
#   latmin, "):1:(", latmax, ")][(", 
#   lonmin, "):1:(", lonmax, ")]"
# )
# file.name.out <- "exp4.nc"
# d <- paste0(
#   "http://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst", 
#   string.date.loc
#   #, ",analysis_error", string.date.loc, ",mask", string.date.loc
# )
# download.file(
#   d, destfile = paste0("../whale-model-prep_data/mursst_nc/exp/", file.name.out), 
#   method = "auto", quiet = FALSE, mode = "wb", cacheOK = TRUE
# )


# ### Open and examine exp nc file
# 
# library(ncdf4)
# x <- nc_open("../whale-model-prep_data/mursst_nc/exp/exp.nc")
# y <- nc_open("../whale-model-prep_data/mursst_nc/exp/exp2.nc")
# 
# x.t <- ncvar_get(x, "time")
# y.t <- ncvar_get(y, "time")
# as.POSIXct(x.t, origin = "1970-01-01")
# as.POSIXct(y.t, origin = "1970-01-01")
# 
# nc_close(x)
# nc_close(y)


###############################################################################
###############################################################################
# Loop for downloading multiple nc files
#   For loop checks for and skips already-downloaded files
### TODO by user: update years, extent, days.gap, or path (if necessary)
rm(list = ls())

library(lubridate)
library(ncdf4)

lonmin <- -132
lonmax <- -116
latmin <- 29
latmax <- 49
coords.txt <- paste0( #Part of url
  "(", paste(lonmin, lonmax, latmin, latmax, sep = ")-("), ")"
)
days.gap <- 1 # Number of days between downloaded files


years.todownload <- 2018:2019

## User path descriptions
# file.name.out.head: Folder to which to download murSST nc files
source("User_script_local.R", local = TRUE, echo = FALSE)
if (user == "KAF") {
  file.name.out.head  <- "" 
  
} else if (user == "EAB") {
  file.name.out.head  <- ""
  
} else if (user == "SMW") {
  file.name.out.head <- "../whale-model-prep_data/mursst_nc/"

} else {
  stop("Invalid value supplied for 'user' object")
}


### Requires that yearly folders are already created within file.name.out.head
# 3var: 751s for 36 files (2005:2008, months 1 to 3, days 1 to 3): ~21s per file
# 1var: 338s for 36 files (2005:2008, months 1 to 3, days 1 to 3): ~9.4s per file
for(i in years.todownload) {
  print(i)
  start.date <- as.Date(paste0(i, "-01-01"))
  
  
    for(j in (1:12 - 1)) { #(1:12 - 1)
    print(paste(i, "-", j + 1))
    period.month <- period(j, "month")
    curr.j <- start.date + period.month
    
    
    for(k in (1:days_in_month(curr.j) - 1)) { #(1:days_in_month(curr.j) - 1)
      curr.k <- curr.j %m+% period(k, "day")
      # Download
      string.date.loc <- paste0(
        "[(", curr.k, "T09:00:00Z):", days.gap, ":(", curr.k, "T09:00:00Z)][(",
        latmin, "):1:(", latmax, ")][(",
        lonmin, "):1:(", lonmax, ")]"
      )
      url.name <- paste0(
        "http://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst",
        string.date.loc
        #, ",analysis_error", string.date.loc, ",mask", string.date.loc
      )
      
      file.name.out <- paste0(
        file.name.out.head, year(curr.k), 
        "/mursst_", curr.k, "_", coords.txt, ".nc"
      )
      stopifnot(file.exists(paste0(file.name.out.head, year(curr.k))))
      
      if (!file.exists(file.name.out)) {
        download.file(
          url.name, destfile = file.name.out,
          method = "auto", quiet = TRUE, mode = "wb", cacheOK = TRUE
        )
      }
      rm(curr.k, string.date.loc, url.name, file.name.out)
      
    }
  }
}; rm(i, j, k)

###############################################################################
###############################################################################

# Code written for WEAR project, Nov 2018
# Based on '02d-get murSST.R' by Anita...
# Extract murSST data for segement data, "LgWhale_CCE_91_14_3km_Segs_BF0_6.csv"
# Data downloaded in murSST_download_WEAR.R 
# Data (analysed_sst, every day 2005-2017, one nc file for each day) stored at
#   "J:\Sam_Woodman\mursst_nc", where "J:\" is "mmdisk". 
# murSST only goes back to 2005, so no data for earlier cruise data
# For each segment point: get point value then 
#   sd for 1, 4 (9x9km), 12 (25x25km) pixels each way
#
# By Sam Woodman


# TODO by user: 
#   1) Change file paths if necessary
#   2) Update 'grid.dates' object, and startgrid and endgrid if necessary

###############################################################################
rm(list = ls())
source("Funcs_WEAR.R")


# install.packages(c("ncdf4", "dplyr", "purrr", "lubridate", "sf"))
library(ncdf4)
library(dplyr)
library(purrr)
library(lubridate)
library(sf)


### Paths to files
source("User_script_local.R", local = TRUE, echo = FALSE)

## User path descriptions
# nc.path:  Top-level folder for murSST nc files (this folder contains yearly folders, e.g. '2019')
# in.path:  Folder with 'Grid_Nonrectangle_3km_WEAR.csv'
# out.path: Folder to write csv filed with extracted data

if (user == "KAF") {
  nc.path  <- "" 
  in.path  <- ""
  out.path <- ""
  
} else if (user == "EAB") {
  nc.path  <- ""
  in.path  <- ""
  out.path <- ""
  
} else if (user == "SMW") {
  nc.path <- "../whale-model-prep_data/mursst_nc/" #SMW local
  # nc.path  <- "J:/Sam_Woodman/mursst_nc/" # J is mmdisk on SMW computer
  in.path  <- "../whale-model-prep_data/Grid/"
  out.path <- "../whale-model-prep_data/Grid/Grid_murSST/"
  
} else {
  stop("Invalid value supplied for 'user' object")
}


###############################################################################
### Prep
infile    <- paste0(in.path, "Grid_Nonrectangle_3km_WEAR.csv")
data.orig <- read.csv(infile, stringsAsFactors = FALSE)
varname <- 'analysed_sst'

# Number of rows in each direction from center point for which to get data
pixel.radius  <- c(4, 12)

# Generate dates for which to get data and corresponding filename
# grid.dates <- seq(as.Date("2005-01-01"), as.Date("2017-12-31"), by = 2)
grid.dates <- seq(as.Date("2019-01-02"), as.Date("2019-08-15"), by = 2)
grid.dates.df <- data.frame(
  grid_dates = grid.dates, year = year(grid.dates), 
  month = month(grid.dates), day = day(grid.dates)
)

# Generate filenames of mursst nc files
temp <- grid.dates.df %>%
  mutate(month_chr = formatC(month, width = 2, format = "d", flag = "0"),
         day_chr = formatC(day, width = 2, format = "d", flag = "0")) %>%
  select(year, month_chr, day_chr) %>%
  mutate(file_nc = pmap_chr(., paste, sep = "-")) %>%
  mutate(file_nc = paste0(nc.path, year, "/mursst_", file_nc, "_(-132)-(-116)-(29)-(49).nc"))

dates.files.nc <- temp$file_nc; rm(grid.dates.df, temp)

# Get nc file lat/lon info - this will be the same across mursst nc files
nc.temp  <- nc_open(dates.files.nc[4]) #2000; Changed from 1 to some data that is local
nc.lon   <- ncvar_get(nc.temp, "longitude")
nc.lat   <- ncvar_get(nc.temp, "latitude")
nc.nrows <- length(nc.lon)
nc.ncols <- length(nc.lat)
nc_close(nc.temp); rm(nc.temp)

# Prep grid file
grid.data.orig <- data.orig %>% select(lon = lon180, lat)


###############################################################################
#----------------------------------------------------------
grid.rad.half <- 0.027 / 2

#----------------------------------------------------------
t1 <- Sys.time()
# 30 Nov: 1.12 min for 5 days (2005 Jan 1-9)
# 03 Dec: 6.5 min for 5 days (2005 Jan 1-9) b/c of smartcheck

#----------------------------------------------------------
# Loop through each daily grid file to be created 
#   To run in smaller batches, specify start and end of grid
# grid.dates[1100]: "2011-01-08"
# grid.dates[2250]: "2017-04-26"

startgrid <- 1
endgrid   <- length(grid.dates)

for(g in startgrid:endgrid) {
  # Prep  
  grid.date <- grid.dates[g]
  print(paste(g, grid.date, sep = ": "))
  
  # Get nc file data and check that nc file only has data for 1 day
  if (file.exists(dates.files.nc[g])) {
    nc.data <- try(nc_open(dates.files.nc[g]), silent = TRUE)
    if (inherits(nc.data, "try-error")) {
      warning("File ", dates.files.nc[g], " cannot be opened")
      grid.data <- grid.data.orig
      
    } else {
      if (length(ncvar_get(nc.data, "time")) > 1) stop("nc time error")
      
      # nc_extract() is in 'Funcs_WEAR.R'
      # smartcheck type means that surrounding nc points w/in grid cell are considered
      grid.data <- nc_extract(
        grid.data.orig, nc.data, nc.lon, nc.lat, nc.nrows, nc.ncols,
        time.idx = 1, var.name = varname, calib = 0, sd.radius = pixel.radius, 
        smartcheck = TRUE, grid.rad.half = grid.rad.half, 
        na.idx = NULL, s.type.flag = "mursst"
      )
      
      nc_close(nc.data)
    }
    
  } else {
    warning("File", dates.files.nc[g], "does not exist")
    
    grid.data <- grid.data.orig
    grid.data$temp1 <- NA
    grid.data$temp2 <- NA
    grid.data$temp3 <- NA
    names(grid.data) <- c(
      head(names(grid.data), -3), "analysed_sst.mean", 'analysed_sst.SD.04', 'analysed_sst.SD.12'
    )
  }
  
  grid.datafile <- paste0(out.path, 'WEAR_mursst_3km_', grid.date, '.csv')
  write.table(grid.data, grid.datafile, sep = "," , col.names = TRUE, row.names = FALSE)
  rm(grid.data, grid.datafile)
}; rm(g); Sys.time() - t1

###############################################################################

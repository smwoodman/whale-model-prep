#--------------------------------------------------------------------------------------
# CCSRA2D__Grid_Data.R  
#
#   Extracts CCSRA data from 2D prediction grids dervived by Mike Jacox's for 31-yr 
#   reanalysis files (for 1991-2010) and the NRT (2011-2015) .nc files:
#
#    'curl','sst', 'ssh', 'sss', 'ild', 'su', 'sv', 'sustr', 'svstr' 
#
#    Modified by Karin Forney & Elizabeth Becker from the CalCOFI_CCSRA_Grid_Data.R to obtain  
#    CCSRA data for 2D prediction grids (vs. 3D)                 12/19/2016
#
#--------------------------------------------------------------------------------------
# Clear workspace 
#
rm(list = ls())
source("Funcs_WEAR.R")

# #-------------------------------SET UP FUNCTIONS---------------------------------------
# # Function to check for libararies
# #
# is.installed <- function(x){
#   is.element(x, installed.packages()[,1])
# } 
# #--------------------------------------------------------------------------------------
# #
# # If needed install and load ncdf4 packages
# 
# if (!is.installed("ncdf4")){
#   install.packages("ncdf4")
# }

# install.packages("ncdf4", "dplyr", "purrr", "lubridate")
library(ncdf4)
library(dplyr)
library(purrr)
library(lubridate)

#-------------------------------END OF FUNCTIONS---------------------------------------
#
# Open .nc file for 2011-2015 CCSRA-NRT data and read in non-changing values
# Notes re NRT predictors:  
#  SST: should be similar between the two datasets.
#  SD(SST): should be the same.
#  Salinity: will be different ? there was not very much data available to use for 
#     the near-real-time assimilation.  (KAF: maybe use Aviso for salinity for post 
#     2010 modeling).
#  SSH and SD(SSH): a calibration factor could be applied to the near-real-time data 
#     to match it to the historical reanalysis dataset (Chris offered to provide a 
#     calibration factor for our use).
#  MLD/ILD:  should be fine since largely based on the temperature signal.
#  PEA:  could be different.
#
# ---------------------------------------------------------------------------
# Set path for nc files, input grids and output files based on who 
# is running code (change user initials, all CAPS)
#
user <- "SMW"         # select "EAB" or "KAF"

if (user=="KAF") {  
  grid.path <- 'C:/KAF/PROJECTS/SERDP-CCmodels/CCE1991-2014/'
  nc.path31 <- 'C:/KAF/PROJECTS/SERDP-CCmodels/CCE1991-2014/wcra31_daily/' 
  nc.pathNRT <- 'C:/KAF/PROJECTS/SERDP-CCmodels/CCE1991-2014/wcnrt_daily/'    
  out.path <- 'C:/KAF/PROJECTS/SERDP-CCmodels/CCE1991-2014/CCSRA_pred_grids/'
  
} else if (user == "EAB") {
  nc.path31 <- 'C:/Users/EABECKER/Documents/HabModels_CCE_1991_2014/Grid_data/wcra31_daily/' 
  nc.pathNRT <- 'C:/Users/EABECKER/Documents/HabModels_CCE_1991_2014/Grid_data/wcnrt_daily/'    
  grid.path <- 'C:/Users/EABECKER/Documents/HabModels_CCE_2013/Datasets/EAB_CCE/CCE_Grid_Pred_Data/'
  out.path <- 'C:/Users/EABECKER/Documents/HabModels_CCE_1991_2014/Grid_data/CalCOFI/CCSRA_pred_grids/'
  
} else if (user == "SMW") {
  nc.path31 <- '../whale-model-prep_data/CCSRA nc files/CCSRA_wcra31_daily_2D_Jacox/' 
  nc.pathNRT <- '../whale-model-prep_data/CCSRA nc files/CCSRA_NRT2011-2017_daily_2D_Jacox/'    
  grid.path <- '../whale-model-prep_data/Grid/'
  out.path <- '../whale-model-prep_data/Grid/Grid_CCSRA/'
}
#
# 
# ---------------------------------------------------------------------------
# Set Predictor variable names, and set up array with cruise dates which grid files 
#  will be extracted from the nc files.
#
ssh.calib <- 0.154     # calibration to add to ccsNRT to make consistent with ccsra31 
Predictors <- c('sst', 'ssh', 'ild')
grid.dates <- seq(as.Date("2005-01-01"), as.Date("2017-12-31"), by = 2)
# write.csv(grid.dates, "Grid.dates.csv")  # save dates for reference

#  
# Open grid pixel file and initialize variables
#  
gridfile <- 'Grid_Nonrectangle_3km_WEAR.csv'
grid.pixelfile <- paste0(grid.path, gridfile)
grid.pixels    <- read.csv(grid.pixelfile, header = TRUE)[, c("lat", "lon180")]
names(grid.pixels) <- c('lat', 'lon')

num.pixels     <- nrow(grid.pixels)
gridlon        <- grid.pixels$lon  # For ROMS data, use -longitudes (not 360)
gridlat        <- grid.pixels$lat

#
t1 <- Sys.time()
# for() loops take 2.3 minutes per var for one day
#   using nc_extract takes ~85s
# for() take 12.64878 mins for 3 vars for 2 days
#   using nc_extract() takes 8.566133 mins

### Load in bathymetric data so that 

# Loop through each daily grid file to be created 
#   To run in smaller batched, specify start and end of grid
startgrid <- 5
endgrid   <- 100 #2374 for WEAR
grid.rad.half <- 0.027 / 2

for(g in startgrid:endgrid) {
  #
  #  Get year, month, day details for this grid file
  #
  grid.data <- grid.pixels
  grid.date <- grid.dates[g]
  print(paste(g, grid.date, sep = ": "))
  
  grid.year  <- lubridate::year(grid.date)  #as.numeric(strftime(grid.dates[g],"%Y"))
  grid.month <- lubridate::month(grid.date) #as.numeric(strftime(grid.dates[g],"%m"))
  grid.day   <- lubridate::day(grid.date)   #as.numeric(strftime(grid.dates[g],"%d"))
  grid.ymd <- paste(
    grid.year, sprintf("%02d", grid.month), sprintf("%02d", grid.day), 
    sep = '-'
  )
  
  #
  #  Now get one predictor at a time from the .nc files
  #
  for(p in Predictors) {
    #
    # Open either ccsra31 or NRT nc file to get needed date 
    #
    nc.file <- ifelse(
      grid.year < 2011, 
      paste0(nc.path31, 'wcra31_', p, '_daily_1991_2010.nc'), 
      paste0(nc.pathNRT, 'wcnrt_', p, '_daily_2011_2017.nc')
                      )
    # if (grid.year < 2011) {                             
    #   nc.file <- paste0(nc.path31, 'wcra31_', p, '_daily_1991_2010.nc')
    # } else {
    #   nc.file <- paste0(nc.pathNRT, 'wcnrt_', p, '_daily_2011_2015.nc')
    # }
    
    nc.data <- nc_open(nc.file)
    
    ROMSlat   <- ncvar_get(nc.data, 'lat')[1, ]
    ROMSlon   <- ncvar_get(nc.data, 'lon')[, 1]
    ROMSnrows <- length(ROMSlon)
    ROMSncols <- length(ROMSlat)
    
    # Find index in the ROMS file for the date of this grid file 
    ROMS.year  <- ncvar_get(nc.data, 'year')
    ROMS.month <- ncvar_get(nc.data, 'month')
    ROMS.day   <- ncvar_get(nc.data, 'day')
    ROMS.data <- data.frame(ROMS.year, ROMS.month, ROMS.day)
    day.index <- which(
      (ROMS.year == grid.year) & (ROMS.month == grid.month) & (ROMS.day == grid.day)
    )
    ROMS.ymd <- paste(ROMS.year[day.index], ROMS.month[day.index], ROMS.day[day.index])
    
    calib.val <- ifelse(p == "ssh" & grid.year >= 2011, ssh.calib, 0)

    # nc_extract() is in 'Funcs_WEAR.R'
    # Don't need to do smartcheck because 0.1 deg res of CCSRA nc is too big
    # Original for() loop code is at bottom of file
    grid.data <- nc_extract(
      grid.data, nc.data, ROMSlon, ROMSlat, ROMSnrows, ROMSncols,
      day.index, var.name = p, calib = calib.val, sd.radius = 1, 
      smartcheck = FALSE, grid.rad.half = grid.rad.half
    )

    nc_close(nc.data)
    
  } # p loop (Predictors) 
  
  grid.datafile <- paste0(out.path, 'WEAR_3km_', grid.ymd, '.csv')
  write.table(grid.data, grid.datafile, sep = "," , col.names = TRUE, row.names = FALSE)
  
}  # g loop (grids)

Sys.time() - t1


#-------------------------------------------------------------------------------------
# For spot-checking, extract one day's complete SST grid (26 June 2014)
#   NOTE:  Values all look good, including edge calculations
#
# testSST<-ncvar_get(nc.data,p,start=c(1,1,day.index),
#                    count=c(ROMSnrows,ROMSncols,1),verbose=FALSE)
# rownames(testSST) <-ROMSlon
# colnames(testSST) <-ROMSlat
# write.csv(testSST,"SST26Jun2014.csv")


#-------------------------------------------------------------------------------------


# # Determine the pixels needed for grid location, if lat/long are not NA
# grid.data$Predictor.mean   <- NA     #  Add generic columns
# grid.data$Predictor.SD     <- NA     #   for predictor
# 
# for (y in 1:num.pixels) {
#   if (!is.na(gridlat[y]) & !is.na(gridlon[y])) {
#     c.lat <- which(abs(ROMSlat - gridlat[y]) == min(abs(ROMSlat - gridlat[y])))[1] #[1] added by SMW - TBD
#     r.lon <- which(abs(ROMSlon - gridlon[y]) == min(abs(ROMSlon - gridlon[y])))[1] #[1] added by SMW - TBD
# 
#     # if (length(r.lon) > 1 | length(c.lat) > 1) browser()
#     
#     # nrows and ncols are used if we are at the edge of the data grid
#     row1    <- max(r.lon - 1, 1)
#     numrows <- min(r.lon + 1, ROMSnrows) - row1 + 1
#     col1    <- max(c.lat - 1, 1)
#     numcols <- min(c.lat + 1, ROMSncols) - col1 + 1
# 
#     # newlon <- ROMSlon[row1:(row1+numrows-1)]
#     # newlat <- ROMSlat[col1:(col1+numcols-1)]
# 
#     #  Extract 9 pixels surrounding lat/lon point for the grid date
#     #  Get center pixel value as mean and calculate SD.space from all 9 pixels
#     #  A few print statements for checking:
#     #    print(paste0('Extracting ',p,' for: '))
#     #    print(paste(ROMSlat[c.lat],ROMSlon[r.lon],ROMS.ymd))
# 
#     pred.data <- ncvar_get(
#       nc.data, p, start = c(row1, col1, day.index),
#       count = c(numrows, numcols, 1), verbose = FALSE
#     )
# 
#     if (p == "ssh" & grid.year >= 2011) {
#       pred.data <- pred.data + ssh.calib
#     }
# 
#     grid.data$Predictor.mean[y] <- pred.data[1 + (r.lon - row1), 1 + (c.lat - col1)]
#     grid.data$Predictor.SD[y]   <- sd(pred.data[,], na.rm = TRUE)
#   } # end of if there are no NAs
# }  # y loop (grid pixels)
# 
# new.Preds <- c(ncol(grid.data) - 1,  ncol(grid.data))
# names(grid.data)[new.Preds] <- c(paste0(p, '.mean'), paste0(p, '.SD'))

#-------------------------------------------------------------------------------------

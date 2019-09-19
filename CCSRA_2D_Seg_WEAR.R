#--------------------------------------------------------------------------------------
# CCSRA2D_SEG_Data.R  
#
#   Extracts CCSRA data from 2D prediction grids dervived by Mike Jacox's for 31-yr 
#   reanalysis files (for 1991-2010) and the updated NRT (2011-2017) .nc files:
#
#    'curl','sst', 'ssh', 'sss', 'ild', 'su', 'sv', 'sustr', 'svstr' 
#
#    Modified by Karin Forney & Elizabeth Becker from the CalCOFI_CCSRA_Grid_Data.R 
#    to obtain  CCSRA data for 2D prediction grids (vs. 3D)              12/19/2016
#
#    Modified for updated 2011-2017 NRT data set (2D) provided by Mike Jacox  5/9/2017
#
#    Modified by Karin Forney & Elizabeth Becker for segment extractions  04/30/2018
#
#    Modified by Karin Forney to extract data for RREAS 1997-2016 data    10/15/2018
# 
#    Modified by Sam Woodman for WEAR project   11/26/2018
#
# Notes re NRT predictors:  
#  SST: should be similar between the two datasets.
#  SD(SST): should be the same.
#  Salinity: will be different ? there was not very much data available to use for 
#     the near-real-time assimilation.  (KAF: maybe use Aviso for salinity for post 
#     2010 modeling).
#  SSH and SD(SSH): a calibration factor can be applied to the near-real-time data 
#      to match it to the historical reanalysis dataset.
#  MLD/ILD:  should be fine since largely based on the temperature signal.
#  PEA:  could be different.
#
#--------------------------------------------------------------------------------------
# Clear workspace 
#
rm(list = ls())

#-------------------------------SET UP FUNCTIONS---------------------------------------
# # Function to check for libararies
# #
# is.installed <- function(x) {
#   is.element(x, installed.packages()[,1])
# } 
# #--------------------------------------------------------------------------------------
# #
# # If needed install and load ncdf4 packages
# 
# if (!is.installed("ncdf4")){
#   install.packages("ncdf4")
# }
library(ncdf4)
library(dplyr)

##  Functions to get nc data (calib is zero for all variables except for NRT ssh)
getvarROMS <- function(nc, varname, inpts, calib){
  # inpts$dt <- as.POSIXct(inpts$dt, '%Y-%m-%d', tz = 'UTC')
  nc.data <- nc_open(nc, write = FALSE)
  lat <- ncvar_get(nc.data, 'lat')[1, ]
  lon <- ncvar_get(nc.data, 'lon')[, 1]
  yr  <- ncvar_get(nc.data, 'year')
  mth <- ncvar_get(nc.data, 'month')
  day <- ncvar_get(nc.data, 'day')
  tim <- as.POSIXct(paste(yr, mth, day, sep = '-'), tz = 'UTC')
  
  for (i in 1:nrow(inpts)) {
    # print(inpts$dt[i])
    
    if (inpts$dt[i] %in% tim) {
      xdate <- which(inpts$dt[i]==tim)
      c <- which(abs(lon - inpts$lon[i]) == min(abs(lon - inpts$lon[i])))[1]
      r <- which(abs(lat - inpts$lat[i]) == min(abs(lat - inpts$lat[i])))[1]
      data.var <- ncvar_get(
        nc.data, varname, start = c(c, r, xdate), 
        count = c(1, 1, 1), verbose = FALSE
      )
      data.var <- data.var + calib
      inpts[i, paste0(varname, '.mean')] <- data.var
      
    } else {
      inpts[i, paste0(varname, '.mean')] <- NA
    }
  }
  
  nc_close(nc.data)
  
  return(inpts)
}

getsdROMS <- function(nc, varname, inpts, pixel.radius, calib){
  inpts$dt <- as.POSIXct(inpts$dt, '%Y-%m-%d', tz = 'UTC')
  nc.data <- nc_open(nc, write = FALSE)
  lat <- ncvar_get(nc.data, 'lat')[1, ]
  lon <- ncvar_get(nc.data, 'lon')[, 1]
  nrows <- length(lat); ncols <- length(lon)
  yr <- ncvar_get(nc.data, 'year')
  mth <- ncvar_get(nc.data, 'month')
  day <- ncvar_get(nc.data, 'day')
  tim <- as.POSIXct(paste(yr, mth, day, sep = '-'), tz = 'UTC')
  
  for (i in 1:nrow(inpts)){
    # print(inpts$dt[i])
    
    if (inpts$dt[i] %in% tim){
      xdate <- which(inpts$dt[i] == tim)
      c <- which(abs(lon - inpts$lon[i]) == min(abs(lon - inpts$lon[i])))[1]
      r <- which(abs(lat - inpts$lat[i]) == min(abs(lat - inpts$lat[i])))[1]
      col1 <- max(c - pixel.radius, 1)
      numcols <- min(2 * + pixel.radius + 1, ncols - col1)
      row1 <- max(r - pixel.radius, 1)
      numrows <- min(2 * + pixel.radius + 1, nrows - row1)
      newlon <- lon[row1:(row1 + numrows - 1)]
      newlat <- lat[col1:(col1 + numcols - 1)]
      data.var  <- ncvar_get(
        nc.data, varname, start = c(col1, row1, xdate),
        count = c(numcols, numrows, 1), verbose = FALSE
      )
      data.var <- data.var + calib
      var.mat <- matrix(data.var, numrows, numcols, dimnames = list(newlon, newlat))
      inpts[i, paste0(varname, '.SDspace')] <- sd(var.mat[!is.nan(var.mat)])
      
    } else {
      inpts[i, paste0(varname, '.SDspace')] <- NA
    }
  }
  
  nc_close(nc.data)
  
  return(inpts)
}


#-------------------------------END OF FUNCTIONS---------------------------------------
# Set calibration factor for SSH
ssh.calib <- 0.035     # constant to add to ccsNRT to make consistent with ccsra31 (per M. Jacox, 5/8/17)  
pixel.radius <- 1
#   
# Set path for nc files, input grids and output files based on who 
# is running code (change user initials, all CAPS)
#

source("User_script_local.R", local = TRUE, echo = FALSE)
# user <- "SMW"         # select "EAB" or "KAF"

if (user=="KAF") {  
  seg.path <- 'C:/KAF/PROJECTS/SERDP-CCmodels/WhalePreyModels/RockfishCruiseModels/'
  nc.path31 <- 'C:/KAF/PROJECTS/SERDP-CCmodels/CCSRA/CCSRA_wcra31_daily_2D_Jacox/' 
  nc.pathNRT <- 'C:/KAF/PROJECTS/SERDP-CCmodels/CCSRA/CCSRA_NRT2011-2017_daily_2D_Jacox/'   
  etopo.path <- 'C:/KAF/PROJECTS/SERDP-CCmodels/ETOPO/' 
  
} else if (user == "EAB") {
  seg.path <- 'C:/Users/EABECKER/Documents/HabModels_CCE_2013/Datasets/CalCOFI/'
  nc.path31 <- 'C:/Users/EABECKER/Documents/HabModels_CCE_1991_2014/Grid_data/wcra31_daily/' 
  nc.pathNRT <- 'C:/Users/EABECKER/Documents/HabModels_CCE_1991_2014/Grid_data/wcnrt_daily/'   
  etopo.path <- 'C:/Users/EABECKER/Documents/HabModels_CCE_1991_2014/Grid_data/'
  
} else if (user == "SMW") { #"SMW"
  seg.path <- "../whale-model-prep_data/Segments/"
  nc.path31 <- "../whale-model-prep_data/CCSRA_nc/CCSRA_wcra31_daily_2D_Jacox/"
  nc.pathNRT <- "../whale-model-prep_data/CCSRA_nc/CCSRA_NRT2011-2017_daily_2D_Jacox/"
  
} else {
  stop("Invalid value supplied for 'user' object")
}


### Read in segment data file and assign variable names 
infile <- paste0(seg.path, "LgWhale_CCE_91_14_3km_Segs_BF0_6_Dec13_2018.csv")
outfile <- paste0(seg.path, 'WEAR_seg_CCSRA.csv')

seg.data <- read.table(infile, sep = ",", header = TRUE, stringsAsFactors = FALSE)

segs <- seg.data %>% 
  select(segnum, lon = mlon, lat = mlat, year, month, day) %>% 
  mutate(dt = as.POSIXct(paste(year, month, day, sep = "-"), "%Y-%m-%d", tz = 'UTC'))


### Separate segments for ccsra31 and ccsraNRT and get variables separately
segs2010 <- segs[segs$year < 2011, ]
segs2011 <- segs[segs$year >= 2011, ]

segs2010 <- getvarROMS(paste0(nc.path31, 'wcra31_sst_daily_1991_2010.nc'), 'sst', segs2010, 0) #11s
segs2010 <- getvarROMS(paste0(nc.path31, 'wcra31_ssh_daily_1991_2010.nc'), 'ssh', segs2010, 0) #15s
segs2010 <- getvarROMS(paste0(nc.path31, 'wcra31_ild_daily_1991_2010.nc'), 'ild', segs2010, 0)

segs2010 <- getsdROMS(paste0(nc.path31, 'wcra31_sst_daily_1991_2010.nc'), 'sst', segs2010, pixel.radius, 0) #29s
segs2010 <- getsdROMS(paste0(nc.path31, 'wcra31_ssh_daily_1991_2010.nc'), 'ssh', segs2010, pixel.radius, 0)
segs2010 <- getsdROMS(paste0(nc.path31, 'wcra31_ild_daily_1991_2010.nc'), 'ild', segs2010, pixel.radius, 0)

segs2011 <- getvarROMS(paste0(nc.pathNRT, 'wcnrt_sst_daily_20110102_20170419.nc'), 'sst', segs2011, 0)
segs2011 <- getvarROMS(paste0(nc.pathNRT, 'wcnrt_ssh_daily_20110102_20170419.nc'), 'ssh', segs2011, ssh.calib)
segs2011 <- getvarROMS(paste0(nc.pathNRT, 'wcnrt_ild_daily_20110102_20170419.nc'), 'ild', segs2011, 0)

segs2011 <- getsdROMS(paste0(nc.pathNRT, 'wcnrt_sst_daily_20110102_20170419.nc'), 'sst', segs2011, pixel.radius, 0)
segs2011 <- getsdROMS(paste0(nc.pathNRT, 'wcnrt_ssh_daily_20110102_20170419.nc'), 'ssh', segs2011, pixel.radius, ssh.calib)
segs2011 <- getsdROMS(paste0(nc.pathNRT, 'wcnrt_ild_daily_20110102_20170419.nc'), 'ild', segs2011, pixel.radius, 0)

segs <- rbind(segs2010, segs2011)


### For WEAR project, depth and sd(depth) are extracted in 'ETOPO_Extractions_WEAR.R'


### Save data
write.table(segs, outfile, sep = "," , col.names = TRUE, row.names = FALSE)

#-------------------------------------------------------------------------------------

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
rm(list=ls())

#-------------------------------SET UP FUNCTIONS---------------------------------------
# Function to check for libararies
#
is.installed <- function(x){
  is.element(x, installed.packages()[,1])
} 
#--------------------------------------------------------------------------------------
#
# If needed install and load ncdf4 packages

if (!is.installed("ncdf4")){
  install.packages("ncdf4")
}
library(ncdf4)

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
user <- "KAF"         # select "EAB" or "KAF"

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
  out.path <- '../whale-model-prep_data/Grid/'
}
#
# 
# ---------------------------------------------------------------------------
# Set Predictor variable names, and set up array with cruise dates which grid files 
#  will be extracted from the nc files.
#
ssh.calib <- 0.154     # calibration to add to ccsNRT to make consistent with ccsra31 
Predictors <- c('sst', 'ssh', 'ild')
grid.dates <- c(seq(as.Date("1991/06/26"), as.Date("1991/12/06"), by=1),
                seq(as.Date("1993/06/26"), as.Date("1993/12/06"), by=1),
                seq(as.Date("1996/06/26"), as.Date("1996/12/06"), by=1),
                seq(as.Date("2001/06/26"), as.Date("2001/12/06"), by=1),
                seq(as.Date("2005/06/26"), as.Date("2005/12/06"), by=1),
                seq(as.Date("2008/06/26"), as.Date("2008/12/06"), by=1),
                seq(as.Date("2009/06/26"), as.Date("2009/12/06"), by=1),
                seq(as.Date("2014/06/26"), as.Date("2014/12/06"), by=1))

numgrids <- length(grid.dates)
write.csv(grid.dates,"Grid.dates.csv")  # save dates for reference
#  
# Open grid pixel file and initialize variables
#  
gridfile <- 'Grid_Nonrectangle_3km_WEAR.csv'
grid.pixelfile <- paste(grid.path,gridfile,sep="")
grid.pixels    <- read.csv(grid.pixelfile, header = TRUE)[, c(1, 2)]
names(grid.pixels) <- c('lat','lon')

num.pixels     <- nrow(grid.pixels)
gridlon        <- grid.pixels$lon  # For ROMS data, use -longitudes (not 360)
gridlat        <- grid.pixels$lat

#
t1<-Sys.time()
print(t1)

# Loop through each daily grid file to be created 

startgrid <- 1148                # To run in smaller batches,
endgrid   <- 1149                # specify start and end grid

for (g in startgrid:endgrid) {
  #
  #  Get year, month, day details for this grid file
  #
  grid.data <- grid.pixels
  grid.year <-   as.numeric(strftime(grid.dates[g],"%Y"))
  grid.month <-  as.numeric(strftime(grid.dates[g],"%m"))
  grid.day <-    as.numeric(strftime(grid.dates[g],"%d"))
  grid.ymd <- paste(grid.year, sprintf("%02d",grid.month), sprintf("%02d",grid.day),sep='-')
  #
  #  Now get one predictor at a time from the .nc files
  #
  for (p in 1:length(Predictors))  {
    grid.data$Predictor.mean   <- NA     #  Add generic columns 
    grid.data$Predictor.SD     <- NA     #   for predictor 
    #
    # Open either ccsra31 or NRT nc file to get needed date 
    #
    if (grid.year < 2011) {                             
      nc.file <- paste0('wcra31_',Predictors[p],'_daily_1991_2010.nc')
      nc.file <- paste(nc.path31, nc.file,sep="")
    } else {
      nc.file <- paste0('wcnrt_',Predictors[p],'_daily_2011_2015.nc')
      nc.file <- paste(nc.pathNRT, nc.file,sep="")
    }
    
    nc.data <- nc_open(nc.file)
    ROMSlat   <- ncvar_get(nc.data,'lat')[1,]
    ROMSlon   <- ncvar_get(nc.data,'lon')[,1]
    ROMSnrows <- length(ROMSlon)
    ROMSncols <- length(ROMSlat)
    #
    # Find index in the ROMS file for the date of this grid file 
    #
    ROMS.year  <- ncvar_get(nc.data,'year')
    ROMS.month <- ncvar_get(nc.data,'month')
    ROMS.day   <- ncvar_get(nc.data,'day')
    ROMS.data <- data.frame(ROMS.year, ROMS.month, ROMS.day)
    day.index <- which((ROMS.year==grid.year) & 
                         (ROMS.month==grid.month) & (ROMS.day==grid.day))
    ROMS.ymd <- paste(ROMS.year[day.index],ROMS.month[day.index],ROMS.day[day.index])
    # 
    #  Determine the pixels needed for grid location, if lat/long are not NA
    #
    for (y in 1:num.pixels) {
      if (!is.na(gridlat[y]) & !is.na(gridlon[y])) {
        c<-which(abs(ROMSlat-gridlat[y])==min(abs(ROMSlat-gridlat[y])))
        r<-which(abs(ROMSlon-gridlon[y])==min(abs(ROMSlon-gridlon[y])))
        row1    <- max(r-1,1)
        numrows <- min(r+1,ROMSnrows)-row1+1      # nrows and ncols are used if we are at
        col1 <- max(c-1,1)                        # the edge of the data grid
        numcols <- min(c+1,ROMSncols)-col1+1
        
        newlon <- ROMSlon[row1:(row1+numrows-1)]
        newlat <- ROMSlat[col1:(col1+numcols-1)]
        #
        #  Extract 9 pixels surrounding lat/lon point for the grid date
        #  Get center pixel value as mean and calculate SD.space from all 9 pixels
        #  A few print statements for checking:
        #    print(paste0('Extracting ',Predictors[p],' for: '))
        #    print(paste(ROMSlat[c],ROMSlon[r],ROMS.ymd))
        
        pred.data    <- ncvar_get(nc.data,Predictors[p],start=c(row1,col1,day.index),
                                  count=c(numrows,numcols,1),verbose=FALSE) 
        
        if (Predictors[p]=="ssh"& grid.year >= 2011) {pred.data <- pred.data + ssh.calib}
        
        grid.data$Predictor.mean[y]   <- pred.data[1+(r-row1),1+(c-col1)]       
        grid.data$Predictor.SD[y]     <- sd(pred.data[,], na.rm=TRUE)
      } # end of if there are no NAs
    }  # y loop (grid pixels)
    
    new.Preds <- c(ncol(grid.data)-1,  ncol(grid.data))
    names(grid.data)[new.Preds] <- c(paste(Predictors[p],'.mean',sep=""),
                                     paste(Predictors[p],'.SD',sep=""))
    nc_close(nc.data)
    
  } # p loop (Predictors) 
  
  grid.datafile <- paste(out.path,'CCE_0.1deg_',grid.ymd,'.csv',sep="")
  write.table(grid.data, grid.datafile, sep = "," , col.names = TRUE, row.names = FALSE)
  
}  # g loop (grids)

t2<-Sys.time()
print(t1)
print(t2)
print (t2-t1)

#
#  Now extract rugosity data from separate NC file -- this only needs to be done
#  once for the grid pixels, and output is stored in a separate .csv file.
#
grid.data <- grid.pixels
grid.data$rugosity <- NA
nc.file <- paste0(grid.path,'ETOPO1_CCS_bathySD.nc')
nc.data <- nc_open(nc.file)
str(nc.data)

ETOPOsd.lat   <- ncvar_get(nc.data,'latitude')
ETOPOsd.lon   <- ncvar_get(nc.data,'longitude')
ETOPOsd.nrows <- length(ETOPOsd.lon)
ETOPOsd.ncols <- length(ETOPOsd.lat)

rugosity <-ncvar_get(nc.data,'layer',start=c(1,1),
                     count=c(ETOPOsd.nrows,ETOPOsd.ncols),verbose=FALSE)   
rownames(rugosity) <-ETOPOsd.lon
colnames(rugosity) <-ETOPOsd.lat

write.csv(rugosity,'ETOPO1_CCS_bathySD.csv')

for (y in 1:num.pixels) {
  if (!is.na(gridlat[y]) & !is.na(gridlon[y])) {
    c<-which(abs(ETOPOsd.lat-gridlat[y])==min(abs(ETOPOsd.lat-gridlat[y])))
    r<-which(abs(ETOPOsd.lon-gridlon[y])==min(abs(ETOPOsd.lon-gridlon[y])))
    row1    <- max(r-1,1)
    numrows <- min(r+1,ETOPOsd.nrows)-row1+1  # nrows and ncols are used if we 
    col1 <- max(c-1,1)                        # are at the edge of the data grid
    numcols <- min(c+1,ETOPOsd.ncols)-col1+1
    
    newlon <- ETOPOsd.lon[row1:(row1+numrows-1)]
    newlat <- ETOPOsd.lat[col1:(col1+numcols-1)]
    
    pred.data      <- ncvar_get(nc.data,'layer',start=c(row1,col1),
                                count=c(numrows,numcols),verbose=FALSE) 
    grid.data$rugosity[y]     <- pred.data[1+(r-row1),1+(c-col1)]    
    
  } # end of if there are no NAs
}  # y loop (grid pixels)

grid.datafile <- paste(out.path,'CCE_0.1deg_rugosity.csv',sep="")
write.table(grid.data, grid.datafile, sep = "," , col.names = TRUE, 
            row.names = FALSE)
#
#-------------------------------------------------------------------------------------
#
#For spot-checking, extract one day's complete SST grid (26 June 2014)
#  NOTE:  Values all look good, including edge calculations
#
#        testSST<-ncvar_get(nc.data,Predictors[p],start=c(1,1,day.index),
#                 count=c(ROMSnrows,ROMSncols,1),verbose=FALSE)
#        rownames(testSST) <-ROMSlon
#        colnames(testSST) <-ROMSlat
#        write.csv(testSST,"SST26Jun2014.csv")
#
#-------------------------------------------------------------------------------------



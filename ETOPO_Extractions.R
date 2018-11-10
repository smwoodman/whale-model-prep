#--------------------------------------------------------------------------------------
# ETOPO_EXTRACTIONS.R  
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
# Set path for ETOPO nc file, input segment file and output files 
# Open grid pixel file and initialize variables
#
etopo.path <- 'C:/KAF/COAST/ETOPO/'
path <-  'C:/KAF/PROJECTS/SERDP-CCmodels/WhalePreyModels/RockfishCruiseModels/'

infile     <- paste(path, 'RFwhales.csv',sep="")
outfile    <- paste(path, 'RFwhales_bathy.csv',sep="")
in.data    <- read.csv(infile, header=TRUE)   #[,c(2,4)]
num.pts <- dim(in.data)[1]
lon        <- in.data$lon    
lat        <- in.data$lat

###################################################################
#
#  Now extract rugosity and depth data from separate NC files -- this only needs to 
#  be done once for the grid pixels, and output is stored in a separate .csv file.
#
out.data <- in.data
out.data$rugosity <- NA
out.data$depth <- NA
#
# Get rugosity first
#
nc.file <- paste0(etopo.path,'ETOPO1_CCS_bathySD.nc')
nc.data <- nc_open(nc.file)

ETOPOsd.lat   <- ncvar_get(nc.data,'latitude')
ETOPOsd.lon   <- ncvar_get(nc.data,'longitude')
ETOPOsd.nrows <- length(ETOPOsd.lon)
ETOPOsd.ncols <- length(ETOPOsd.lat)

rugosity <-ncvar_get(nc.data,'layer',start=c(1,1),
                     count=c(ETOPOsd.nrows,ETOPOsd.ncols),verbose=FALSE)   
rownames(rugosity) <-ETOPOsd.lon
colnames(rugosity) <-ETOPOsd.lat

#    write.csv(rugosity,'ETOPO1_CCS_bathySD.csv')

for (y in 1:num.pts) {
  if (!is.na(lat[y]) & !is.na(lon[y])) {
    c<-which(abs(ETOPOsd.lat-lat[y])==min(abs(ETOPOsd.lat-lat[y])))
    r<-which(abs(ETOPOsd.lon-lon[y])==min(abs(ETOPOsd.lon-lon[y])))
    row1    <- max(r-1,1)
    numrows <- min(r+1,ETOPOsd.nrows)-row1+1  # nrows and ncols are used if we 
    col1 <- max(c-1,1)                        # are at the edge of the data grid
    numcols <- min(c+1,ETOPOsd.ncols)-col1+1
    
    newlon <- ETOPOsd.lon[row1:(row1+numrows-1)]
    newlat <- ETOPOsd.lat[col1:(col1+numcols-1)]
    
    pred.data      <- ncvar_get(nc.data,'layer',start=c(row1,col1),
                                count=c(numrows,numcols),verbose=FALSE) 
    out.data$rugosity[y]     <- pred.data[1+(r-row1),1+(c-col1)]    
    
  } # end of if there are no NAs
}  # y loop (lat/lon points)

#
# Now get depth
#
nc.file <- paste0(etopo.path,'etopo180_N10-60_W150-100.nc')
nc.data <- nc_open(nc.file)
ETOPO.lat   <- ncvar_get(nc.data,'latitude')
ETOPO.lon   <- ncvar_get(nc.data,'longitude')
ETOPO.nrows <- length(ETOPO.lon)
ETOPO.ncols <- length(ETOPO.lat)

depth <-ncvar_get(nc.data,'altitude',start=c(1,1),
                  count=c(ETOPO.nrows,ETOPO.ncols),verbose=FALSE)   
rownames(depth) <-ETOPO.lon
colnames(depth) <-ETOPO.lat

write.csv(depth,'ETOPO1_CCS_bathy.csv')

for (y in 1:num.pts) {
  if (!is.na(lat[y]) & !is.na(lon[y])) {
    c<-which(abs(ETOPO.lat-lat[y])==min(abs(ETOPO.lat-lat[y])))
    r<-which(abs(ETOPO.lon-lon[y])==min(abs(ETOPO.lon-lon[y])))
    row1    <- max(r-1,1)
    numrows <- min(r+1,ETOPO.nrows)-row1+1  # nrows and ncols are used if we 
    col1 <- max(c-1,1)                        # are at the edge of the data grid
    numcols <- min(c+1,ETOPO.ncols)-col1+1
    
    newlon <- ETOPO.lon[row1:(row1+numrows-1)]
    newlat <- ETOPO.lat[col1:(col1+numcols-1)]
    
    pred.data      <- ncvar_get(nc.data,'altitude',start=c(row1,col1),
                                count=c(numrows,numcols),verbose=FALSE) 
    out.data$depth[y]  <- pred.data[1+(r-row1),1+(c-col1)][1]    
    
  } # end of if there are no NAs
}  # y loop (lat/lon points)

#
# change rugosity and depth for positive depth values (=land) to NA
# (this is only needed for grids which have land)
#     
land <- which(out.data$depth>=0)
out.data$rugosity[land] <- NA
out.data$depth[land] <- NA

write.table(out.data, outfile, sep = "," , col.names = TRUE, 
            row.names = FALSE)


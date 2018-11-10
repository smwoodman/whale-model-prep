### extract murSST (daily, 1km resolution); aka ultra-high resolution data set
##  produced at JPL under the NASA MEaSUREs program (see http://mur.jpl.nasa.gov/).
## - version 4 as of April 2015.
##  This dataset is part of the Multi-sensor Improved Sea Surface Temperatures (MURSST) project, which is part of the Group for High-Resolution Sea Surface Temperature (GHRSST) project.
##  I downloaded one .nc-file per month (e.g. "mursst2010-07-01_2010-07-31.nc"), only North Sea;
## see script: murSST_extractBymonth.r
## update April 2016: a) new version available (fv04.1, jplMURSST41.nc?), I downloaded version 4.1 data for all months in the complete period 2005-2015; to use a consistent data source for any new analysis
##  b) change "seg.data$Predictor.mean" to "seg.data$Predictor.center"; "seg.data$Predictor.mean" is now holding the mean of all pixels that are crossed by a segment (Segmean) (you would need this when your predictor has a fine resolution, like murSST with 1km) 

rm(list=ls())

#### directories ####
DEP.dir <- '/Users/gilles/Dropbox/DEPONS/'
FIN.dir<-paste(DEP.dir,'final/',sep='') #this is where we kept our data from step I

### libraries ###
library(ncdf4)

# create df (ncinfo) with names of stored monthly .nc files
require(timeDate)
start <- timeSequence("2005-01-01","2013-12-31", by = "month") 
end<- timeLastDayInMonth(start)
outfile<-paste('mursst',start,'_',end,'.nc',sep='')
yyyymm<-paste(substr(outfile,7,10),substr(outfile,12,13),sep='')
ncinfo<-data.frame(cbind(yyyymm,outfile))
names(ncinfo) <- c('yyyymm','nc.name')


# Set path for nc files
nc.path <- '/Volumes/ANGELO/Anita/DEPONS/mursst/'
#nc.path<- 'C:/KAF/PROJECTS/SERDP-CCmodels/Anita/mursst/'

# load segment data & extract relevant fields
load(file=paste(FIN.dir,"DEPONS_static.rdata",sep=""))

data<-depons
data$month<-as.numeric(data$month)   #  needed if month is factor
data$day<-as.numeric(data$day)       #  needed if day is factor
#data<-data[data$year=="2010",]      # subset for testing

seglat   <-   data$seg_lat 
seglon   <-   data$seg_lon 
segyear  <-   data$year
segmonth <-   data$month
segday   <-   data$day
segyyyymm <-  paste(segyear,gsub(" ","0",format(segmonth,trim="FALSE")),sep='') # produce yyyymm to open correct (monthly) nc file
seg.ncfile.index <- match(segyyyymm,ncinfo$yyyymm)                             # match month of segment to monthly nc.file

# get time vector as 'YYYY-MM-DD'
date <- paste(segyear,segmonth,segday,sep='-')
segtime <- strptime(date, "%Y-%m-%d")

# Create output dataset and add columns for new predictor variables   
seg.data  <- data
seg.data$Predictor.center     <- NA  # value of target.pixel (center pixel); i.e. the (one) pixel where the segment's midpoint falls in; called Predictor.mean in earlier version
seg.data$Predictor.mean       <- NA  # new for "segment mean"; i.e. takes into account all neighbouring/crossed pixels for a given segment length; e.g. for a 9km segment the segment RADIUS would be "(9-1)/2=4"
seg.data$Predictor.SDspace20  <- NA 
seg.data$Predictor.SDspace10  <- NA 
seg.data$Predictor.SDspace5   <- NA 
seg.data$Predictor.SDtime     <- NA 

#####################

Predictor.name <- 'mursst'
varname <- 'analysed_sst' 
pixel.radius <- 20        # +/- pixels for SD space calcs.  1= 9 pixels, 2=25 pixels, etc.
# set pixel.radius to largest needed radius to retrieve largest chunk of "space", then define radius=10 and =5 within that block
# make sure that pixel.radius includes at least the length of your segment

t1<-Sys.time()
print(t1)

for(y in 1:nrow(seg.data)) { # Loop through the segment file 
  #y<-1
  #for(y in 1:10) {
  SegDay <- segtime[y]  
  endday <- segday[y]                  # needed below for SD.Time calc
  #Seg.Date <-as.Date(SegDay)          # use as.Date to convert character data to dates; redundant here?
  
  ncfile<-ncinfo[seg.ncfile.index[y],2]       # extract nc.name as file is listed in nc.path
  datafile <- paste(nc.path,ncfile,sep="")            
  nc.data <- nc_open(datafile)                       # Open matched nc file and process data as needed 
  lat <- ncvar_get(nc.data,'latitude')
  lon <- ncvar_get(nc.data,'longitude')
  tim <- ncvar_get(nc.data,'time')      
  day <- substr(as.POSIXlt(tim,origin='1970-01-01',tz= "GMT"),1,10) 
  
  day.index <- which(day==SegDay)                       # changed file.index to day.index; match nc date to SegDay
  if (length(day.index)==0) {
    print(paste('Not OK! Missing dates in nc file for: ',SegDay,sep="  "))
  }   
  nrows  <- length(lon)
  ncols  <- length(lat) 
  ntimes <- length(tim)  # days in month
  
  # Get value for segment mid-point ("mean") and calculate Predictor.SDspace for
  # up to xx pixels (see pixel radius) around each segment midpoint.
  
  c<-which(abs(lat-seglat[y])==min(abs(lat-seglat[y]))) # lat.index
  r<-which(abs(lon-seglon[y])==min(abs(lon-seglon[y]))) # lon.index
  row1    <- max(r-pixel.radius,1)
  numrows <- min(2*+pixel.radius+1,nrows)          # nrows and ncols are used if we are at
  col1 <- max(c-pixel.radius,1)                    # the edge of the data grid
  numcols <- min(2*+pixel.radius+1,ncols)
  time1 <- max(day.index-8+1,1)                    # if day.index(i.e.SegDay)<9: days needed from previous month; otherwise days needed from current month to go backwards to retrieve a full 8-day preceding window; 
  # 8+1 is needed because time1 is included as one of the 8 numtimes (error catched by EAB Aug 23 2016!)
  numtimes <- min(8,day.index)                     # either 8 or day.index in case day.index<8;
  newlon <- lon[row1:(row1+numrows-1)]             # defines space/block we need to extract
  newlat <- lat[col1:(col1+numcols-1)]             # defines space/block we need to extract
  #
  #  Extract only the SST data for 8-day window ending on segment day and
  #  for the selected pixel.radius around target pixel 
  #
  data.var  <-  ncvar_get(nc.data,varname,start=c(row1,col1,time1),
                          count=c(numrows,numcols,numtimes),verbose=FALSE)
  nc_close(nc.data)
  
  target.pixel <- pixel.radius + 1
  target.day <- numtimes
  
  #
  # Define segment radius; i.e. the block of which the mean should be extracted for a segment that crosses several pixels; important here since the Predictor(SST) resolution is 1 km and target segment length is about 10 km
  #
  if (seg.data$seg_length_km[y]<3){
    seg.radius <-ceiling((seg.data$seg_length_km[y]-1)/2)    #  round makes 4.5=4; ceiling makes 4.5=5; since there a few ship segments that are pretty small it is better to use ceiling here   
  } else {
    seg.radius <-round((seg.data$seg_length_km[y]-1)/2)
  }
  
  #seg.range <- c(target.pixel-seg.radius, target.pixel+seg.radius) # to shorten the otherwise long expression in: "seg.data$Predictor.mean[y]  <- mean(sst.day[c((target.pixel-seg.radius): (target.pixel+seg.radius)),c((target.pixel-seg.radius):(target.pixel+seg.radius))], na.rm=TRUE)"
  #
  #  Extract center(pixel), mean and SD space for these pixels on segment date
  # 
  if (target.day==1) {
    sst.day <- matrix(data.var[,], numrows, numcols, 
                      dimnames=list(newlon,newlat))             # data.var for exact segday and defined block/space      
  } else {
    sst.day <- matrix(data.var[,,target.day], numrows, numcols, 
                      dimnames=list(newlon,newlat))      
  }
  seg.data$Predictor.center[y]    <- sst.day[target.pixel,target.pixel]          # called seg.data$Predictor.mean in earlier version
  #seg.data$Predictor.mean[y]      <- mean(sst.day[seg.range, seg.range], na.rm=TRUE)  # Segmean of all pixels crossed by the segment and in the direct vicinity (based on seg.radius)
  seg.data$Predictor.mean[y]      <- mean(sst.day[c((target.pixel-seg.radius):(target.pixel+seg.radius)),c((target.pixel-seg.radius):(target.pixel+seg.radius))], na.rm=TRUE)
  seg.data$Predictor.SDspace20[y] <- sd(sst.day, na.rm=TRUE)
  seg.data$Predictor.SDspace10[y] <- sd(sst.day[11:31,11:31], na.rm=TRUE)   # define block for pixel radius =10
  seg.data$Predictor.SDspace5[y]  <- sd(sst.day[16:26,16:26], na.rm=TRUE)   # define block for pixel radius =5
  #
  #
  # Extract SSTs for preceding 8 days to get SDtime. If day of month is
  # 7 or less, we need to get additional SSTs from previous monthly .nc file. 
  #
  if (target.day==1) {
    sst.8day  <- data.var[target.pixel,target.pixel]    
  } else {
    sst.8day  <- data.var[target.pixel,target.pixel,]    
  }
  
  if(endday<8) {                                     # if we need SSTs from previous month
    prevdays <- 8-endday
    prev.ncfile.index <- seg.ncfile.index[y]-1
    ncfile.prev<-ncinfo[prev.ncfile.index,2]
    datafile <- paste(nc.path,ncfile.prev,sep="") 
    nc.data <- nc_open(datafile)                     # open previous month .nc file
    
    lastday <- length(ncvar_get(nc.data,'time'))     
    startday <- lastday-prevdays+1
    prev.sst  <-  ncvar_get(nc.data,varname,start=c(r,c,startday),
                            count=c(1,1,prevdays),verbose=FALSE)
    nc_close(nc.data) 
    
    sst.8day<-c(sst.8day,prev.sst)        # final data values for this pixel for endday and preceding xx days
    
  } # end of if endday<8
  
  seg.data$Predictor.SDtime[y]<-sd(sst.8day, na.rm=TRUE)
  
}   # end  for(y in 1:nrow(seg.data))   

#  
# Find data closest to each segment midpoint and add to data.frame
#
lastcol<-ncol(seg.data)
new.Preds <- c((lastcol-5): lastcol)
names(seg.data)[new.Preds] <- c(paste(Predictor.name,'.center',sep=""), # called '.mean' before
                                paste(Predictor.name,'.mean',sep=""),   # called '.Segmean' before
                                paste(Predictor.name,'.SDspace20',sep=""),
                                paste(Predictor.name,'.SDspace10',sep=""),
                                paste(Predictor.name,'.SDspace5',sep=""),
                                paste(Predictor.name,'.SDtime',sep=""))

t2<-Sys.time()
print(t2-t1)

depons<-seg.data

# Print data
#write.table(depons, file = "depons_murSST.csv", sep = ",", col.names = TRUE, row.names = FALSE)
save(depons,file=paste(FIN.dir,"depons_env_final.rdata",sep=""),compress='gzip')


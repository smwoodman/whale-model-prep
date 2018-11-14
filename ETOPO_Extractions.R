#--------------------------------------------------------------------------------------
# ETOPO_EXTRACTIONS.R  
#
#--------------------------------------------------------------------------------------
# Clear workspace 
#
rm(list = ls())

#-------------------------------SET UP FUNCTIONS---------------------------------------
# If needed install and load ncdf4 packages

if (!require(ncdf4)) install.packages("ncdf4")
require(ncdf4)

#-------------------------------END OF FUNCTIONS---------------------------------------
# Set path for ETOPO nc file, input segment file and output files 
# Open grid pixel file and initialize variables

# etopo.path <- 'C:/KAF/COAST/ETOPO/'
# path <-  'C:/KAF/PROJECTS/SERDP-CCmodels/WhalePreyModels/RockfishCruiseModels/'

etopo.path <- "../whale-model-prep_data/etopo180_N10-60_W150-100/"
path <- "../whale-model-prep_data/Outputs/"

infile     <- paste0(path, 'Grid_Non-rectangle_3km.csv')
outfile    <- paste0(path, 'Grid_Non-rectangle_3km_bathy.csv')
in.data    <- read.csv(infile)
num.pts    <- nrow(in.data)
lon        <- in.data$lon180
lat        <- in.data$lat

out.data <- in.data
# out.data$rugosity <- NA
# out.data$depth <- NA


###############################################################################
#  Now extract rugosity and depth data from separate NC files -- this only needs to 
#  be done once for the grid pixels, and output is stored in a separate .csv file.

#------------------------------------------------------------------------------
# # Get rugosity first
# nc.file <- paste0(etopo.path,'ETOPO1_CCS_bathySD.nc')
# nc.data <- nc_open(nc.file)
# 
# ETOPOsd.lat   <- ncvar_get(nc.data,'latitude')
# ETOPOsd.lon   <- ncvar_get(nc.data,'longitude')
# ETOPOsd.nrows <- length(ETOPOsd.lon)
# ETOPOsd.ncols <- length(ETOPOsd.lat)
# 
# rugosity <-ncvar_get(nc.data,'layer',start=c(1,1),
#                      count=c(ETOPOsd.nrows,ETOPOsd.ncols),verbose=FALSE)   
# rownames(rugosity) <-ETOPOsd.lon
# colnames(rugosity) <-ETOPOsd.lat
# 
# #    write.csv(rugosity,'ETOPO1_CCS_bathySD.csv')
# 
# for (y in 1:num.pts) {
#   if (!is.na(lat[y]) & !is.na(lon[y])) {
#     c<-which(abs(ETOPOsd.lat-lat[y])==min(abs(ETOPOsd.lat-lat[y])))
#     r<-which(abs(ETOPOsd.lon-lon[y])==min(abs(ETOPOsd.lon-lon[y])))
#     row1    <- max(r-1,1)
#     numrows <- min(r+1,ETOPOsd.nrows)-row1+1  # nrows and ncols are used if we 
#     col1 <- max(c-1,1)                        # are at the edge of the data grid
#     numcols <- min(c+1,ETOPOsd.ncols)-col1+1
#     
#     newlon <- ETOPOsd.lon[row1:(row1+numrows-1)]
#     newlat <- ETOPOsd.lat[col1:(col1+numcols-1)]
#     
#     pred.data      <- ncvar_get(nc.data,'layer',start=c(row1,col1),
#                                 count=c(numrows,numcols),verbose=FALSE) 
#     out.data$rugosity[y]     <- pred.data[1+(r-row1),1+(c-col1)]    
#     
#   } # end of if there are no NAs
# }   # y loop (lat/lon points)
# 
# nc_close(nc.data)


#------------------------------------------------------------------------------
# Now get depth
nc.file <- paste0(etopo.path,'etopo180_N10-60_W150-100.nc')
nc.data <- nc_open(nc.file)
ETOPO.lat   <- ncvar_get(nc.data, 'latitude')
ETOPO.lon   <- ncvar_get(nc.data, 'longitude')
ETOPO.nrows <- length(ETOPO.lon)
ETOPO.ncols <- length(ETOPO.lat)

depth <- ncvar_get(
  nc.data, 'altitude', start = c(1, 1),
  count = c(ETOPO.nrows, ETOPO.ncols), verbose = FALSE
)   
rownames(depth) <- ETOPO.lon
colnames(depth) <- ETOPO.lat
write.csv(depth, paste0(etopo.path, 'ETOPO1_CCS_bathy.csv'))

for (y in 1:num.pts) { #~5 sec for() loop
  if (!is.na(lat[y]) & !is.na(lon[y])) {
    r.lon <- which.min(abs(ETOPO.lon - lon[y]))
    c.lat <- which.min(abs(ETOPO.lat - lat[y]))

    # nrows and ncols are used if we are at the edge of the data grid
    row1    <- max(r.lon - 1, 1)
    numrows <- min(r.lon + 1, ETOPO.nrows) - row1 + 1
    col1    <- max(c.lat - 1, 1)
    numcols <- min(c.lat + 1, ETOPO.ncols) - col1 + 1

    newlon <- ETOPO.lon[row1:(row1 + numrows - 1)]
    newlat <- ETOPO.lat[col1:(col1 + numcols - 1)]

    pred.data <- ncvar_get(
      nc.data, 'altitude', start = c(row1, col1),
      count = c(numrows, numcols), verbose = FALSE
    )
    out.data$depth[y]  <- pred.data[1 + (r.lon - row1), 1 + (c.lat - col1)][1]

  } # end of if there are no NAs
}   # y loop (lat/lon points)


#######
out.data$depth <- apply(cbind(lon, lat), 1, function(i) {
  if (anyNA(i)) {
    NA
    
  } else {
    r.lon <- which.min(abs(ETOPO.lon - i[1]))
    c.lat <- which.min(abs(ETOPO.lat - i[2]))
    
    # nrows and ncols are used if we are at the edge of the data grid
    row1    <- max(r.lon - 1, 1)
    numrows <- min(r.lon + 1, ETOPO.nrows) - row1 + 1  
    col1    <- max(c.lat - 1, 1)                        
    numcols <- min(c.lat + 1, ETOPO.ncols) - col1 + 1
    
    # newlon <- ETOPO.lon[row1:(row1 + numrows - 1)]
    # newlat <- ETOPO.lat[col1:(col1 + numcols - 1)]
    
    pred.data <- ncvar_get(
      nc.data, 'altitude', start = c(row1, col1),
      count = c(numrows, numcols), verbose = FALSE
    ) 
    # TODO: should this be mean of negative values?
    pred.data[1 + (r.lon - row1), 1 + (c.lat - col1)][1]
  }
})

#######

nc_close(nc.data)


###############################################################################
# Change rugosity and depth for positive depth values (i.e. land) to NA
# (this is only needed for grids which have land)
#     
land <- which(out.data$depth >= 0)
# out.data$rugosity[land] <- NA
out.data$depth[land] <- NA

write.table(out.data, outfile, sep = "," , col.names = TRUE, row.names = FALSE)

###############################################################################

#--------------------------------------------------------------------------------------
# ETOPO_EXTRACTIONS.R 
# Adapted for WEAR project by Sam Woodman Nov 2018
#
#--------------------------------------------------------------------------------------
# Clear workspace 
#
rm(list = ls())

#-------------------------------SET UP FUNCTIONS---------------------------------------
# If needed install and load ncdf4 and dplyr packages
list.of.packages <- c("ncdf4", "dplyr", "purrr")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages) #install packages
lapply(new.packages, require, character.only = TRUE)    #load packages


#-------------------------------END OF FUNCTIONS---------------------------------------

###############################################################################
# Set path for ETOPO nc file, input segment file and output files 
# Open grid pixel file and initialize variables

# etopo.path <- 'C:/KAF/COAST/ETOPO/'
# path <-  'C:/KAF/PROJECTS/SERDP-CCmodels/WhalePreyModels/RockfishCruiseModels/'

etopo.path <- "../whale-model-prep_data/etopo180_N10-60_W150-100/"
path       <- "../whale-model-prep_data/Outputs/"

infile     <- paste0(path, 'Grid_Non-rectangle_3km.csv')
outfile    <- paste0(path, 'Grid_Non-rectangle_3km_bathy.csv')
in.data    <- read.csv(infile)
num.pts    <- nrow(in.data)
lon        <- in.data$lon180
lat        <- in.data$lat

out.data <- in.data


###############################################################################
# Now extract depth data (point value and SD) -- this only needs to be done
#   once for the grid pixels, and output is stored in a separate .csv file.
# No rugosity nc file; just using SD of depth


#------------------------------------------------------------------------------
# Now get depth
nc.file <- paste0(etopo.path,'etopo180_N10-60_W150-100.nc')
nc.data <- nc_open(nc.file)
ETOPO.lat   <- ncvar_get(nc.data, 'latitude')
ETOPO.lon   <- ncvar_get(nc.data, 'longitude')
ETOPO.nrows <- length(ETOPO.lon)
ETOPO.ncols <- length(ETOPO.lat)

# depth <- ncvar_get(
#   nc.data, 'altitude', start = c(1, 1),
#   count = c(ETOPO.nrows, ETOPO.ncols), verbose = FALSE
# )   
# rownames(depth) <- ETOPO.lon
# colnames(depth) <- ETOPO.lat
# write.csv(depth, paste0(etopo.path, 'ETOPO1_CCS_bathy.csv'))

# for() loop moved to bottom of file
z <- 0
out.data$depth_lc <- apply(cbind(lon, lat), 1, function(i) {
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
    
    if (pred.data[1 + (r.lon - row1), 1 + (c.lat - col1)][1] > 0) {
      z <<- z + any(pred.data >= 0)
    }
    
    list(
      pred.data[1 + (r.lon - row1), 1 + (c.lat - col1)][1],
      sd(pred.data[pred.data < 0])
    )
  }
})
out.data <- out.data %>% 
  mutate(depth = purrr::map_dbl(depth_lc, function(i) i[[1]]), 
         depth_sd = purrr::map_dbl(depth_lc, function(i) i[[2]])) %>% 
  select(lat, lon180, lon360, area_km, depth, depth_sd)

nc_close(nc.data)


###############################################################################
# Change depth and depth_sd for positive depth values (i.e. land) to NA
# (this is only needed for grids which have land)

land <- which(out.data$depth >= 0)

out.data$depth[land] <- NA
out.data$depth_sd[land] <- NA

write.table(out.data, outfile, sep = "," , col.names = TRUE, row.names = FALSE)

###############################################################################
###############################################################################

# For a grid file of 30,877 coordinates, apply() is ~6s and for() is ~8s

# out.data <- in.data
# out.data$depth <- NA
# out.data$depth_sd <- NA
# 
# d <- Sys.time()
# for (y in 1:num.pts) { #~5 sec for() loop
#   if (!is.na(lat[y]) & !is.na(lon[y])) {
#     r.lon <- which.min(abs(ETOPO.lon - lon[y]))
#     c.lat <- which.min(abs(ETOPO.lat - lat[y]))
# 
#     # nrows and ncols are used if we are at the edge of the data grid
#     row1    <- max(r.lon - 1, 1)
#     numrows <- min(r.lon + 1, ETOPO.nrows) - row1 + 1
#     col1    <- max(c.lat - 1, 1)
#     numcols <- min(c.lat + 1, ETOPO.ncols) - col1 + 1
# 
#     # newlon <- ETOPO.lon[row1:(row1 + numrows - 1)]
#     # newlat <- ETOPO.lat[col1:(col1 + numcols - 1)]
# 
#     pred.data <- ncvar_get(
#       nc.data, 'altitude', start = c(row1, col1),
#       count = c(numrows, numcols), verbose = FALSE
#     )
#     out.data$depth[y]  <- pred.data[1 + (r.lon - row1), 1 + (c.lat - col1)][1]
#     out.data$depth_sd[y]  <- sd(pred.data[pred.data < 0])
#     
#   } # end of if there are no NAs
# }   # y loop (lat/lon points)
# Sys.time() - d


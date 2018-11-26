#--------------------------------------------------------------------------------------
# ETOPO_EXTRACTIONS.R 
# Adapted for WEAR project by Sam Woodman Nov 2018
# Code to extract ETOPO data for both grid and segment points
#
#--------------------------------------------------------------------------------------
# Clear workspace 

rm(list = ls())

#-------------------------------SET UP FUNCTIONS---------------------------------------
# Install (if needed) and then load packages
list.of.packages <- c("ncdf4", "dplyr", "purrr", "sf")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages) #install packages
rm(list.of.packages, new.packages)

library(ncdf4)
library(dplyr)
library(purrr)
library(sf)


#-------------------------------END OF FUNCTIONS---------------------------------------

###############################################################################
# Set path for ETOPO nc file, input segment file and output files 
# Open grid pixel file and initialize variables

#----------------------------------------------------------
### FOR KAF
# etopo.path <- 'C:/KAF/COAST/ETOPO/'
# path <-  'C:/KAF/PROJECTS/SERDP-CCmodels/WhalePreyModels/RockfishCruiseModels/'


#----------------------------------------------------------
### For SMW grid
# etopo.path <- "../whale-model-prep_data/etopo180_N10-60_W150-100/"
# path       <- "../whale-model-prep_data/Outputs/"
# 
# infile     <- paste0(path, 'Grid_Non-rectangle_3km.csv')
# outfile    <- paste0(path, 'Grid_Non-rectangle_3km_bathy.csv')
# in.data    <- read.csv(infile)
# num.pts    <- nrow(in.data)
# lon        <- in.data$lon180
# lat        <- in.data$lat
# 
# out.data <- in.data


#----------------------------------------------------------
### For SMW segments
etopo.path <- "../whale-model-prep_data/etopo180_N10-60_W150-100/"
path       <- "../whale-model-prep_data/Segments/"

infile     <- paste0(path, 'LgWhale_CCE_91_14_3km_Segs_BF0_6.csv')
outfile    <- paste0(path, 'WEAR_seg_bathy.csv')
in.data    <- read.csv(infile, stringsAsFactors = FALSE)
num.pts    <- nrow(in.data)
lon        <- in.data$mlon
lat        <- in.data$mlat

out.data <- in.data


###############################################################################
# Now extract depth data (point value and SD) -- this only needs to be done
#   once for the grid pixels, and output is stored in a separate .csv file.
# No rugosity nc file; just using SD of depth for WEAR


#------------------------------------------------------------------------------
### Depth prep work
nc.file <- paste0(etopo.path,'etopo180_N10-60_W150-100.nc')
nc.data <- nc_open(nc.file)
ETOPO.lat   <- ncvar_get(nc.data, 'latitude')
ETOPO.lon   <- ncvar_get(nc.data, 'longitude')
ETOPO.nrows <- length(ETOPO.lon)
ETOPO.ncols <- length(ETOPO.lat)


# # Write nc file depth matrix to csv file
# depth <- ncvar_get(
#   nc.data, 'altitude', start = c(1, 1),
#   count = c(ETOPO.nrows, ETOPO.ncols), verbose = FALSE
# )   
# rownames(depth) <- ETOPO.lon
# colnames(depth) <- ETOPO.lat
# write.csv(depth, paste0(etopo.path, 'ETOPO1_CCS_bathy.csv'))

# # Notes
# For a grid file of 30,877 coordinates, apply() is ~6s and for() is ~8s
# ~5.8 seconds without special stuff
# ~11.5 seconds with special stuff


#------------------------------------------------------------------------------
### Create list-column with both depth and SD(depth) values
# Sam note: nc file is indexed in dim order (X, Y, Z, T) and ncvar_get() output
#   reflects that (i.e. rows correspond to lon and cols to lat of pred.data)
out.data$depth_lc <- apply(cbind(lon, lat), 1, function(i) {
  if (anyNA(i)) {
    warning("A longitude or latitude was NA")
    NA
    
  } else {
    # Get ETOPO row and column number of ETOPO point closest to grid centroid
    # This assumes ETOPO is finer-scale than the grid and spans entire grid
    r.lon <- which.min(abs(ETOPO.lon - i[1]))
    c.lat <- which.min(abs(ETOPO.lat - i[2]))
    
    # nrows and ncols are used if we are at the edge of the ETOPO grid
    row1    <- max(r.lon - 1, 1)
    numrows <- min(r.lon + 1, ETOPO.nrows) - row1 + 1  
    col1    <- max(c.lat - 1, 1)                        
    numcols <- min(c.lat + 1, ETOPO.ncols) - col1 + 1
    if (!(numcols == 3 && numrows == 3)) warning("At edge of ETOPO grid")
    
    # Get depth value of closest ETOPO point and 9 surrounding ETOPO points
    pred.data <- ncvar_get(
      nc.data, 'altitude', start = c(row1, col1),
      count = c(numrows, numcols), verbose = FALSE
    )
    pred.cent <- pred.data[1 + (r.lon - row1), 1 + (c.lat - col1)][1]
    
    # If the centroid is closest to a land ETOPO point (>=0) but 
    #   at least one of the 9 surrounding ETOPO points are ocean (<0), 
    #   then get the value of the closest ocean ETOPO point
    #   that is still within the grid cell
    if (pred.cent >= 0 && any(pred.data < 0)) {
      print("hi")
      ## Create sf objects of ETOPO points, grid cell centroid, and grid cell
      # ETOPO points
      depth.coords <- expand.grid(
        as.numeric(ETOPO.lon[row1:(row1 + numrows - 1)]), 
        as.numeric(ETOPO.lat[col1:(col1 + numcols - 1)])
      )
      depth.all <- as.vector(pred.data)
      
      depth.sf <- depth.coords %>% 
        mutate(depth = depth.all) %>%  #as.vector() combines by column
        st_as_sf(coords = c(1, 2), crs = 4326)
      
      # Grid cell centroid
      cent.sfc <- st_sfc(st_point(i), crs = 4326)
      
      # Grid cell (polygon)
      j <- 0.0225 # Half of grid cell length/width
      poly.sfc <- st_sfc(st_polygon(list(matrix(
        c(i[1] + j, i[1] - j, i[1] - j, i[1] + j, i[1] + j,
          i[2] + j, i[2] + j, i[2] - j, i[2] - j, i[2] + j),
        ncol = 2
      ))), crs = 4326); rm(j)
      
      ## Determine which of the points that meet the depth (negative) and 
      ##   polygon (within grid cell) requirements
      poly.depth.int <- suppressMessages(st_intersects(poly.sfc, depth.sf)[[1]])
      depth.which <- which(
        (1:(numcols * numrows) %in% poly.depth.int) & (depth.all < 0)
      )
      
      if (length(depth.which) > 0) {
        ## Which of those points is closest to the centroid
        cent.depth.dist <- as.numeric(st_distance(cent.sfc, depth.sf))
        names(cent.depth.dist) <- 1:length(depth.all)
        
        cent.depth.dist2 <- cent.depth.dist[depth.which]
        cent.depth.min.name <- as.numeric(
          names(cent.depth.dist2)[which.min(cent.depth.dist2)]
        )
        
        pred.cent <- depth.all[cent.depth.min.name]
      }
    }
    
    list(pred.cent, sd(pred.data[pred.data < 0]))
  }
})


#------------------------------------------------------------------------------
# Extract depth and SD(depth) from list-column and save to their own columns
out.data <- out.data %>% 
  mutate(depth = purrr::map_dbl(depth_lc, function(j) j[[1]]), 
         depth_sd = purrr::map_dbl(depth_lc, function(j) j[[2]])) %>% 
  # select(lat, lon180, lon360, area_km, depth, depth_sd) #grid
  select(-depth_lc) #seg

nc_close(nc.data)


#------------------------------------------------------------------------------
# Change depth and depth_sd for positive depth values (i.e. land) to NA
# This is only needed for grids which have land

land <- which(out.data$depth >= 0)

out.data$depth[land] <- NA
out.data$depth_sd[land] <- NA

write.table(out.data, outfile, sep = "," , col.names = TRUE, row.names = FALSE)


# x <- read.csv("../whale-model-prep_data/Outputs/Grid_Non-rectangle_3km_bathy.csv")
# y <- read.csv("../whale-model-prep_data/Outputs/Grid_Non-rectangle_3km_bathy_noextra.csv")
# sum(is.na(x$depth)) #10954
# sum(is.na(y$depth)) #11344

###############################################################################

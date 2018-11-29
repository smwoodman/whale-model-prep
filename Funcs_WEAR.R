# Functions used in whale-model-prep repository for WEAR project
# Sam Woodman, Nov 2018

###############################################################################
# Extract data from nc file for a grid for a given time and given variable
#   nc.data... vars are passed so they don't have to be calculated each time???
nc_extract <- function(grid.df, nc.data, nc.lon, nc.lat, nc.nrows, nc.ncols, 
                       time.idx, var.name, calib, sd.radius, smartcheck, 
                       grid.rad.half) {
  ### Inputs
  # grid.df: data frame with lon and lat coords of grid cell centroids;
  #   MUST have columns with names 'lon' and 'lat'
  # nc.data: current nc file
  # nc.data.lon: longitude values of current nc file
  # nc.data.lat: latitude values of current nc file
  # nc.data.nrows: number of rows in nc file, i.e. length(nc.data.lon)
  # nc.data.ncols: number of columns in nc file, i.e. length(nc.data.lat)
  # time.idx: index of time for which to extract data from nc file
  # var.name: name of prediction variable
  # calib: calibration value to add to prediction value
  # sd.rad: number of pixels to go in each direction from center nc file point
  #   when calculating SD. Can be a vector for calculating multiple SD's 
  #   (e.g. mursst)
  # 
  
  
  grid.df$pred_lc <- apply(grid.df[, c("lon", "lat")], 1, function(i) {
    if (anyNA(i)) {
      NA
      
    } else {
      r.lon <- which.min(abs(nc.lon - i[1]))
      c.lat <- which.min(abs(nc.lat - i[2]))
      
      # nrows and ncols are used if we are at the edge of the nc file extent
      row1    <- max(r.lon - max(sd.radius), 1)
      numrows <- min(r.lon + max(sd.radius), nc.nrows) - row1 + 1  
      col1    <- max(c.lat - max(sd.radius), 1)                        
      numcols <- min(c.lat + max(sd.radius), nc.ncols) - col1 + 1
      
      # Extract pixels surrounding lat/lon point for the grid date
      #   Surrouding pixels have radius sd.radius
      #   Get center pixel value as mean and calculate SD.space from surrounding pixels
      pred.data <- ncvar_get(
        nc.data, var.name, start = c(row1, col1, time.idx),
        count = c(numrows, numcols, 1), verbose = FALSE
      )
      pred.data <- pred.data + calib
      
      idx.cent <- c(1 + (r.lon - row1), 1 + (c.lat - col1))
      pred.cent <- pred.data[idx.cent[1], idx.cent[2]]
      
      if (smartcheck) {
        stopifnot(exists("grid.rad.half"))
        # If the center pixel value is NA but at least one of the 
        #   surrounding nc file points are non-NaN, then get the value of 
        #   the closest valid nc file point that is still within the grid cell
        # Note is.na() catches NaN's
        if (is.na(pred.cent) & any(!is.na(pred.data))) {
          pred.cent <- nc_extract_smartcheck(
            nc.lon[row1:(row1 + numrows - 1)], 
            nc.lat[col1:(col1 + numcols - 1)], 
            pred.data, pred.cent, i, grid.rad.half, 1
          )
        }
      }
      
      pred.data.sd <- lapply(sd.radius, function(j) {
        d <- list(
          max(idx.cent[1] - j, 1):min(idx.cent[1] + j, numrows), 
          max(idx.cent[2] - j, 1):min(idx.cent[2] + j, numcols)
        )
        sd(pred.data[d[[1]], d[[2]]], na.rm = TRUE)
      })
      
      c(pred.cent, as.list(pred.data.sd))
    }
  })
  
  if (identical(sd.radius, 1)) {
    names.d <- c(paste0(var.name, ".mean"), paste0(var.name, ".SD"))
    grid.df %>% 
      mutate(pred = purrr::map_dbl(pred_lc, function(j) j[[1]]), 
             pred_sd = purrr::map_dbl(pred_lc, function(j) j[[2]])) %>% 
      select(-pred_lc) %>% 
      set_names(head(names(grid.df), -1), names.d)
    
  } else {
    names.d <- c(paste0(var.name, ".mean"), paste0(var.name, ".SD.", sd.radius))
    stop("nc_extract() not ready for multiple sd.radius values yet")
  }
}



###############################################################################
# If current variable value is NA (i.e. positive for depth) then look around
#   for closest valid value within the grid cell
nc_extract_smartcheck <- function(lon, lat, pred.data, pred.cent, pt.grid,
                                  grid.rad.half, na.flag) {
  ### Inputs
  # lon: vector of longitudes for matrix, sorted from smallest to largest
  # lat: vector of latitudes for matrix, sorted from smallest to largest
  # pred.data: matrix of prediction values extracted from nc file.
  # pred.cent: center value of pred.data
  # pt.grid: coordinates of current grid cell centroid
  # grid.rad.half: half of grid cell width or length
  # na.flag: 1 if 'invlaid' value is NA; 2 if 'invalid value is > 0
  
  
  #--------------------------------------------------------
  ## Create needed objects
  # nc file points
  nc.coords <- expand.grid(
    as.numeric(lon), as.numeric(lat)
  )
  pred.all <- as.vector(pred.data)
  
  pred.sf <- nc.coords %>%
    mutate(pred = pred.all) %>%  #as.vector() combines by column
    st_as_sf(coords = c(1, 2), crs = 4326)
  
  # Grid cell centroid
  cent.sfc <- st_sfc(st_point(pt.grid), crs = 4326)
  
  # Grid cell (polygon)
  i <- pt.grid
  j <- grid.rad.half
  poly.sfc <- st_sfc(st_polygon(list(matrix(
    c(i[1] + j, i[1] - j, i[1] - j, i[1] + j, i[1] + j,
      i[2] + j, i[2] + j, i[2] - j, i[2] - j, i[2] + j),
    ncol = 2
  ))), crs = 4326); rm(i, j)
  
  #--------------------------------------------------------
  ## Determine which of the points both have a valid value and
  ##   are within the grid cell
  numcols <- length(lon)
  numrows <- length(lat)
  
  poly.pred.int <- suppressMessages(st_intersects(poly.sfc, pred.sf)[[1]])
  if (na.flag == 1) {
    pred.which <- which(
      (1:(numcols * numrows) %in% poly.pred.int) & (!is.na(pred.all))
    )
    
  } else if (na.flag == 2) {
    pred.which <- which(
      (1:(numcols * numrows) %in% poly.pred.int) & (pred.all < 0)
    )
    
  } else {
    stop("Invalid 'na.flag' value")
  }
  
  #--------------------------------------------------------
  ## If any points meet the requirements, which is closest to the centroid?
  if (length(pred.which) > 0) {
    cent.pred.dist <- as.numeric(st_distance(cent.sfc, pred.sf))
    names(cent.pred.dist) <- 1:length(pred.all)
    
    cent.pred.dist2 <- cent.pred.dist[pred.which]
    cent.pred.min.name <- as.numeric(
      names(cent.pred.dist2)[which.min(cent.pred.dist2)]
    )
    
    pred.all[cent.pred.min.name]
    
  } else {
    pred.cent
  }
}

###############################################################################

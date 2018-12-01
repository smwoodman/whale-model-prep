# Functions used in whale-model-prep repository for WEAR project
# Sam Woodman, Nov 2018

###############################################################################
# Extract data from nc file for a grid for a given time and given variable
nc_extract <- function(grid.df, nc.data, nc.lon, nc.lat, nc.nrows, nc.ncols, 
                       time.idx, var.name, calib, sd.radius, smartcheck, 
                       grid.rad.half, na.idx = NULL, s.type.flag) {
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
  # smartcheck: logical flag for whether or not to use nc_extract_smartcheck()
  # grid.rad.half: half of grid cell length; passed to nc_extract_smartcheck()
  # na.idx: indices of rows in grid.df where the extracted value will be NA
  # s.invalid.type.flag: passed to invalid.type.flag of nc_extract_smartcheck() 
  # s.within.poly.check: passed to within.poly.check of nc_extract_smartcheck() 
  
  
  # If any indices have already been identified as going to be NA, 
  #   set their lat/long to NA so that the apply() statement doesn't
  #   both to get data for them
  grid.df.xy <- grid.df[, c("lon", "lat")]
  
  if (!is.null(na.idx)) {
    stopifnot(length(na.idx) == nrow(grid.df))
    grid.df.xy[na.idx, ] <- NA
  }
  
  # It is faster to get matrix out now and then pull from nc file each time
  pred.data.all <- ncvar_get(
    nc.data, var.name, start = c(1, 1, time.idx),
    count = c(nc.nrows, nc.ncols, 1), verbose = FALSE
  )
  
  # Apply across rows of lat/long coordinates
  grid.df$pred_lc <- apply(grid.df.xy, 1, function(i) {
    if (anyNA(i)) {
      list(NA, NA)
      
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
      # pred.data <- ncvar_get(
      #   nc.data, var.name, start = c(row1, col1, time.idx),
      #   count = c(numrows, numcols, 1), verbose = FALSE
      # )
      pred.data <- pred.data.all[row1:(row1 + numrows - 1), 
                                 col1:(col1 + numcols - 1)]
      pred.data <- pred.data + calib
      
      # Determine the index of the center pixel
      idx.cent <- c(1 + (r.lon - row1), 1 + (c.lat - col1))
      pred.cent <- pred.data[idx.cent[1], idx.cent[2]]
      
      # Perform smartcheck if desired
      if (smartcheck) {
        stopifnot(exists("grid.rad.half"))
        # If the center pixel value is NA but at least one of the 
        #   surrounding nc file points are non-NaN, then get the value of 
        #   the closest valid nc file point that is still within the grid cell
        # Note that is.na() catches NaN's
        if (is.na(pred.cent) & any(!is.na(pred.data))) {
          pred.cent <- nc_extract_smartcheck(
            lon = nc.lon[row1:(row1 + numrows - 1)], 
            lat = nc.lat[col1:(col1 + numcols - 1)], 
            pred.data, pred.cent, pt.grid = i, grid.rad.half, 
            type.flag = s.type.flag
          )
        }
      }
      
      # Calculate desired standard deviation(s)
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
  
  # Process list-column returned by apply()
  if (identical(sd.radius, 1)) { #CCSRA/basic
    names.d <- c(paste0(var.name, ".mean"), paste0(var.name, ".SD"))
    grid.df %>% 
      mutate(pred = purrr::map_dbl(pred_lc, function(j) j[[1]]), 
             pred_sd = purrr::map_dbl(pred_lc, function(j) j[[2]])) %>% 
      select(-pred_lc) %>% 
      set_names(head(names(grid.df), -1), names.d)
    
  } else if (identical(sd.radius, c(4, 12))) { #murSST
    names.d <- c(
      paste0(var.name, ".mean"), 
      paste0(var.name, ".SD.", sprintf("%02.0f", sd.radius))
    )
    d <- grid.df %>% 
      mutate(pred = purrr::map_dbl(pred_lc, function(j) j[[1]]), 
             pred_sd1 = purrr::map_dbl(pred_lc, function(j) j[[2]]), 
             pred_sd2 = purrr::map_dbl(pred_lc, function(j) j[[3]])) %>% 
      select(-pred_lc) %>% 
      set_names(head(names(grid.df), -1), names.d)
    
  } else {
    stop("nc_extract() not ready for these sd.radius values yet")
  }
}


###############################################################################
# If current variable value is invalid (i.e. NA or positive for depth) 
#   then look around for closest valid value, maybe within the grid cell
nc_extract_smartcheck <- function(lon, lat, pred.data, pred.cent, pt.grid,
                                  grid.rad.half, type.flag) {
  # invalid.type.flag, within.poly.check = TRUE
  ### Inputs
  # lon: vector of longitudes for matrix, sorted from smallest to largest
  # lat: vector of latitudes for matrix, sorted from smallest to largest
  # pred.data: matrix of prediction values extracted from nc file.
  # pred.cent: center value of pred.data
  # pt.grid: coordinates of current grid cell centroid
  # grid.rad.half: half of grid cell width or length
  # invalid.type.flag: 1 if 'invlaid' value is NA; 2 if 'invalid value is > 0
  # within.poly.check: Logical flag for if nc file point with valid value
  #   needs to be within grid cell
  
  
  #--------------------------------------------------------
  ### Create needed objects
  # nc file points
  nc.coords <- expand.grid(as.numeric(lon), as.numeric(lat))
  pred.all <- as.vector(pred.data) #as.vector() combines by column
  
  pred.sf <- nc.coords %>%
    mutate(pred = pred.all) %>%  
    st_as_sf(coords = c(1, 2), crs = 4326)
  
  # Grid cell centroid
  cent.sfc <- st_sfc(st_point(pt.grid), crs = 4326)
  
  # Grid cell (polygon)
  if (type.flag == "etopo") {
    i <- pt.grid
    j <- grid.rad.half
    poly.sfc <- st_sfc(st_polygon(list(matrix(
      c(i[1] + j, i[1] - j, i[1] - j, i[1] + j, i[1] + j,
        i[2] + j, i[2] + j, i[2] - j, i[2] - j, i[2] + j),
      ncol = 2
    ))), crs = 4326); rm(i, j)
  }
  
  
  #--------------------------------------------------------
  ### Determine which of the points meet smartcheck requirements
  if (type.flag == "etopo") {
    poly.pred.int <- suppressMessages(st_intersects(poly.sfc, pred.sf)[[1]])
    pred.which <- which(
      (1:(length(lon) * length(lat)) %in% poly.pred.int) & (pred.all < 0)
    )
    
  } else if (type.flag == "ccsra") {
    pred.which <- which(!is.na(pred.all))
    
  } else {
    stop("Invalid 'type.flag' value")
  }
  
  # if (invalid.type.flag == 1) {
  #   # Invalid value is NA value
  #   if (within.poly.check) {
  #     # nc file point with valid value must be within grid cell
  #     poly.pred.int <- suppressMessages(st_intersects(poly.sfc, pred.sf)[[1]])
  #     pred.which <- which(
  #       (1:(numcols * numrows) %in% poly.pred.int) & (!is.na(pred.all))
  #     )
  #   } else {
  #     # nc file point with valid value doesn't have to be within grid cell
  #     pred.which <- which(!is.na(pred.all))
  #   }
  #   
  # } else if (invalid.type.flag == 2) {
  #   # Invalid value is value >= 0
  #   if (within.poly.check) {
  #     # nc file point with valid value must be within grid cell
  #     poly.pred.int <- suppressMessages(st_intersects(poly.sfc, pred.sf)[[1]])
  #     pred.which <- which(
  #       (1:(numcols * numrows) %in% poly.pred.int) & (pred.all < 0)
  #     )
  #   } else {
  #     # nc file point with valid value doesn't have to be within grid cell
  #     pred.which <- which(pred.all < 0)
  #   }
  #   
  # } else {
  #   stop("Invalid 'invalid.type.flag' value")
  # }
  
  
  #--------------------------------------------------------
  ### If any points meet the requirements, which is closest to the centroid?
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

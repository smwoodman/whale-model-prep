# Functions used in whale-model-prep repository for WEAR project
# Sam Woodman, Nov 2018


function(lon, lat, pred.data, pt.cent, j) {
  ### Inputs
  # lon: vector of longitudes for matrix
  
  
  # nc file points
  nc.coords <- expand.grid(
    as.numeric(lon), as.numeric(lat)
  )
  nc.pred <- as.vector(pred.data)
  
  nc.pred.sf <- nc.coords %>% 
    mutate(pred = nc.pred) %>%  #as.vector() combines by column
    st_as_sf(coords = c(1, 2), crs = 4326)
  
  # Grid cell centroid
  cent.sfc <- st_sfc(st_point(pt.cent), crs = 4326)
 
  # Grid cell (polygon)
  poly.sfc <- st_sfc(st_polygon(list(matrix(
    c(i[1] + j, i[1] - j, i[1] - j, i[1] + j, i[1] + j,
      i[2] + j, i[2] + j, i[2] - j, i[2] - j, i[2] + j),
    ncol = 2
  ))), crs = 4326)
}


if (pred.cent >= 0 && any(pred.data < 0)) {
  ## Create sf objects of ETOPO points, grid cell centroid, and grid cell
  # ETOPO points
  # depth.coords <- expand.grid(
  #   as.numeric(ETOPO.lon[row1:(row1 + numrows - 1)]), 
  #   as.numeric(ETOPO.lat[col1:(col1 + numcols - 1)])
  # )
  # depth.all <- as.vector(pred.data)
  # 
  # depth.sf <- depth.coords %>% 
  #   mutate(depth = depth.all) %>%  #as.vector() combines by column
  #   st_as_sf(coords = c(1, 2), crs = 4326)
  # 
  # # Grid cell centroid
  # cent.sfc <- st_sfc(st_point(i), crs = 4326)
  # 
  # # Grid cell (polygon)
  # j <- grid.rad.half # Half of grid cell length/width
  # poly.sfc <- st_sfc(st_polygon(list(matrix(
  #   c(i[1] + j, i[1] - j, i[1] - j, i[1] + j, i[1] + j,
  #     i[2] + j, i[2] + j, i[2] - j, i[2] - j, i[2] + j),
  #   ncol = 2
  # ))), crs = 4326); rm(j)
  
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
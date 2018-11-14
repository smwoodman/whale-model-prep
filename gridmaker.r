# gridmaker.r --- creates uniform grid of decimal lat/longs based on 
#   input boundaries and pixel size (in degrees).
# Calculates area size of each pixel in sq.km

# Original code by Karin Forney and Elizabeth Becker
# Modified by Sam Woodman Nov 2018 to use sf package and so grid does 
#   not have to be rectangular


###############################################################################
library(sf)


###############################################################################
# VARIABLES SET BY USER

#----------------------------------------------------------
### 1) Create map object to use while visualizing study area and/or grid

### EITHER ###
# Use this section if using longitude range [0, 360]
map.base <- st_as_sfc(maps::map('world2', plot = FALSE, fill = TRUE))

### OR ###
# Use this section if using longitude range [-180, 180]
# map.base <- st_as_sfc(maps::map('world', plot = FALSE, fill = TRUE))


#----------------------------------------------------------
### 2) Define the study area (rectangular or otherwise)
# The centroids of the grid cells (NOT grid cell edges) will start at the bottom-left corner of the study area,
#   and thus proceed along the left and bottom edges of the study area.
#   All grid cells whose centroids (NOT grid cell edges) are within the study area will be exported to the .csv file.

### EITHER ###
# Use this section for creating a rectangular grid defined by lon/lat min/max

# latmin <- 32.5
# latmax <- 42.0
# lonmin <- 360 - 125.0
# lonmax <- 360 - 118.0
# poly.df <- data.frame(
#   X1 = c(lonmax, lonmin, lonmin, lonmax, lonmax), 
#   X2 = c(latmin, latmin, latmax, latmax, latmin)
# )

###  OR  ###
# Use this section to provide coordinates of a non-rectangular study area
# Coordinates mus be enterds as "c(lon, lat)" where all longitudes are in 
#   range [0, 360] or [-180, 180]. The first and the last set of coordinates
#   must be the same.

list.vertices <- list(
  c(243, 32),
  c(238, 33),
  c(234, 38),
  c(234, 49),
  c(237, 49),
  c(237, 40),
  c(240, 36),
  c(243, 36),
  c(243, 32)
)
poly.df <- data.frame(do.call(rbind, list.vertices))


#----------------------------------------------------------
### 3) Pixel size: the total length and width of each grid cell. 
# Thus, the shortest disatance from a grid cell edge to the grid cell centroid will be 'pixel' / 2.

# pixel <- .225                 # 25km
# pixel <- .090                 # 10km
# pixel <- 0.10                 # 0.1 degree for SeaGrant modeling project
pixel <- .045                 # 5km
# pixel <- .018                 # 2km


#----------------------------------------------------------
### 4) Path and filename for .csv file with grid cell centroid coordinates and areas

outfile <- paste0(
  "Outputs/", 
  ifelse(
    exists("latmin"), 
    paste0("Grid_Lat", latmin, "to", latmax, "_Lon", lonmin, "to", lonmax), 
    "Grid_Non-rectangle"
  ),
  "_Step", pixel, "withArea.csv"
)


###############################################################################
# CODE RUN BY USER - NO CHANGES NEEDED

### Create and visualize study area polygon
poly.bound <- st_sfc(st_polygon(list(as.matrix(poly.df))), crs = 4326) #4326 is WGS 84 coords
plot(poly.bound, axes = TRUE, border = "red")
plot(map.base, add = TRUE, col = "tan")
# plot(poly.bound, add = TRUE, border = "red")

### Create grid (for areas) and get *centroids* of polygons.
# TODO: Should points be lower left vertices or centroids?
grid <- st_make_grid(
  poly.bound, cellsize = pixel, #what = "centers",
  offset = st_bbox(poly.bound)[c("xmin", "ymin")] - pixel / 2
)
grid.cent <- st_set_precision(st_centroid(grid), 1e+10)

### Get the centroids that are within the study area
grid.cent.which <- unlist(st_intersects(poly.bound, grid.cent))

### Create data frame of centroid coordinates and area value
grid.cent.coords <- do.call(rbind, st_geometry(grid.cent)[grid.cent.which])
lon <- grid.cent.coords[, 1]
grid.cent.df <- data.frame(
  lat = grid.cent.coords[, 2], 
  lon360 = lon,
  lon180 = ifelse(lon > 180, lon - 360, lon),
  area_km = as.numeric(st_area(grid[grid.cent.which])) / 1e+06
); rm(lon)

### Write to .csv
write.csv(grid.cent.df, file = outfile, row.names = FALSE)


### Visualize grid is desired
plot(grid[grid.cent.which], axes = TRUE)
plot(poly.bound, add = TRUE, border = "red")
plot(map.base, add = TRUE, col = "tan")

###############################################################################

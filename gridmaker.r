# gridmaker.r --- creates uniform grid of decimal lat/longs based on 
# input boundaries and pixel size (in degrees).
# Calculates area size of each pixel in sq.km

# Originally by Karin Forney and Elizabeth Becker
# Modified by Sam Woodman Nov 2018 so grid does not have to be rectangular


###############################################################################
# New strategy: 
# 1) st_sfc(st_polygon(list(as.matrix(x))), crs = crs.prov)
# 2) Use st_grid_maker()
# 3) Clip to 'study area' using st_intersection()

library(sf)
map.base <- st_as_sfc(maps::map('world2', plot = FALSE, fill = TRUE))


#------------------------------------------------------------------------------
# VARIABLES SET BY USER

#----------------------------------------------------------
### Extent of grid (study area). Boundary marks...

### EITHER ###

# latmin <- 32.5
# latmax <- 42.0
# lonmin <- 360 - 125.0
# lonmax <- 360 - 118.0
# poly.df <- data.frame(
#   X1 = c(lonmax, lonmin, lonmin, lonmax, lonmax), 
#   X2 = c(latmin, latmin, latmax, latmax, latmin)
# )

###  OR  ###

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
### Path for .csv file with coordinates and areas
outfile <- paste0(
  "Outputs/", 
  "Grid_Lat", latmin, "to", latmax, "_Lon", lonmin, "to", lonmax, 
  "_Step", pixel, "withArea.csv"
)

#----------------------------------------------------------
### Pixel size
pixel <- .045

# 0.225: 25km
# 0.090: 10km
# 0.100: 0.1 degree (for SeaGrant modeling project)
# 0.045: 5km
# 0.018: 2km



#------------------------------------------------------------------------------
# Create and visualize study area polygon
poly.bound <- st_sfc(st_polygon(list(as.matrix(poly.df))), crs = 4326) #4326 is WGS 84 coords
plot(poly.bound, axes = TRUE, border = "red")
plot(map.base, add = TRUE, col = "tan")

# Create grid (for areas) and get *centroids* of polygons.
# TODO: Should points be lower left vertices or centroids?
system.time(grid <- st_make_grid(
  poly.bound, cellsize = pixel, #what = "centers",
  offset = st_bbox(poly.bound)[c("xmin", "ymin")] - pixel / 2
))
grid.cent <- st_set_precision(st_centroid(grid), 1e+10)

# Get the centroids that are within the study area
grid.cent.which <- unlist(st_intersects(poly.bound, grid.cent))

# Create data frame of centroid coordinates and area value
grid.cent.coords <- do.call(rbind, st_geometry(grid.cent)[grid.cent.which])
grid.cent.df <- data.frame(
  lat = grid.cent.coords[, 2], 
  lon360 = grid.cent.coords[, 1],
  lon180 = grid.cent.coords[, 1] - 360,
  area_km = as.numeric(st_area(grid)[grid.cent.which]) / 1e+06
)

write.csv(grid.cent.coords, file = outfile, row.names = FALSE)


### Visualize grid is desired
plot(poly.bound, axes = TRUE, border = "red")
plot(grid, add = TRUE)
plot(map.base, add = TRUE, col = "tan")


###############################################################################
###############################################################################
# Old

# rm(list=ls())                          # clears workspace
# library(geosphere)
# pixel.mat <- matrix(NA,5,2)
# colnames(pixel.mat) <- c("x","y") 
# eq.radius <- 6378.137
# 
# latmin <- 32.5
# latmax <- 42.0
# lonmin <- 360 - 125.0
# lonmax <- 360 - 118.0
# # pixel <- .225                 # 25km
# # pixel <- .090                 # 10km
# # pixel <- 0.10                 # 0.1 degree for SeaGrant modeling project
# pixel <- .045                 # 5km
# # pixel <- .018                 # 2km  (not run)
# 
# grid <- data.frame(lat=NULL,lon=NULL)
# y <- Sys.time()
# for (lat in seq(latmin,latmax, by=pixel)) {
#   for (lon in seq(lonmin,lonmax, by=pixel)) {
#     lon180 <- lon-360
#     if (lat<latmax & lon<lonmax) {
#       pixel.mat[1,]=c(lon180,lat)
#       pixel.mat[2,]=c(lon180+pixel,lat)
#       pixel.mat[3,]=c(lon180+pixel,lat+pixel)
#       pixel.mat[4,]=c(lon180,lat+pixel)
#       pixel.mat[5,]=pixel.mat[1,]
#     }
#     pixel.area <- areaPolygon(pixel.mat,eq.radius)
#     grid <- rbind(grid, data.frame(lat=lat, lon360=lon, lon180=lon180, pixelkm2=pixel.area))       
#   }
# }
# Sys.time() - y #~37s
# 
# outfil = paste0(
#   "Outputs/",
#   "Grid_Lat",latmin,"to",latmax,"_Lon",lonmin,"to",lonmax,"_Step",pixel,"withArea.csv"
#   )
# write.csv(grid, file = outfil)
# 
# dim(grid)
# 
# 


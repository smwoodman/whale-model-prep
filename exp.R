library(sf)

###############################################################################
### Read in and clip world land file
x <- c(-130, -110, 30, 50)
range_poly_func <- function(x, poly.crs, buffer) {
  stopifnot(length(x) == 4)
  
  poly.x <- x[c(1, 1, 2, 2, 1)]
  poly.y <- x[c(3, 4, 4, 3, 3)]
  
  st_sfc(
    st_polygon(list(cbind(poly.x, poly.y))), crs = poly.crs
  )
}

st_erase <- function(x, y) st_difference(x, st_union(st_combine(y)))

bound <- range_poly_func(x, 4326)
map.world <- st_geometry(st_read("../whale-model-prep_data/shapefiles/World_countries.shp"))
map.world.bound <- st_intersection(map.world, bound)
ocean <- st_erase(bound, map.world.bound)

plot(ocean, col = "blue", axes = T)
plot(map.world.bound, col = "tan", axes = T)
st_write(ocean, "../whale-model-prep_data/shapefiles/US_west_coast_ocean.shp", )
st_write(map.world.bound, "../whale-model-prep_data/shapefiles/US_west_coast_land.shp")


###############################################################################
### Compare how many grid centroids vs any grid cell falls on land
grid.df <- read.csv("../whale-model-prep_data/Outputs/Grid_Non-rectangle_3km.csv")
grid.cent <- st_as_sf(grid.df, coords = c("lon180", "lat"), crs = 4326)
grid.poly <- eSDM::pts_to_sfc_centroids(grid.df[, c(2, 1)], 0.045/2, 4326)

plot(grid.poly, axes = TRUE)
plot(st_geometry(grid.cent), add = TRUE, col = "red")
plot(st_geometry(map.world.bound), add = TRUE, col = "tan")


d.poly <- st_intersects(st_intersection(grid.poly, bound), ocean)
d.cent <- st_intersects(st_intersection(grid.cent, bound), ocean)

sum(sapply(d.poly, length) > 0) #20,182. Land int: 11918. Ocean w/48 lat max: 
sum(sapply(d.cent, length) > 0) #19,503. Land int: 11374. Ocean w/48 lat max: 
sum(sapply(d.poly, length) > 0) - sum(sapply(d.cent, length) > 0) #679. Land int: 544


###############################################################################
### Check that no segment midpoints fall on land
segs <- read.csv("../whale-model-prep_data/LgWhale_CCE_91_14_3km_Segs_BF0_6.csv")
segs.sfc <- st_geometry(st_as_sf(segs, coords = c("mlon", "mlat"), crs = 4326))

d.segs <- st_intersects(segs.sfc, map.world.bound)
sum(sapply(d.segs, length) > 0) #0

###############################################################################

st_write(grid.cent, "../whale-model-prep_data/shapefiles/grid_cent.shp")
st_write(grid.poly, "../whale-model-prep_data/shapefiles/grid_poly.shp")

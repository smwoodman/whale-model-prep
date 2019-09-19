library(dplyr)
library(sf)
library(maps)
library(mapdata)

grid.3km <- read.csv("../whale-model-prep_data/Grid/Grid_Nonrectangle_3km_WEAR.csv") %>% 
  select(lon180, lat, area_km) %>% 
  eSDM::pts2poly_centroids(0.027 / 2, crs = 4326) %>% 
  mutate(base_idx = 1:length(geometry))

map.base <- st_geometry(st_as_sf(maps::map('world', plot = FALSE, fill = TRUE)))
map.base.zb <- st_crop(map.base, z.buf)
map.base2 <- st_geometry(st_as_sf(maps::map('worldHires', plot = FALSE, fill = TRUE))) %>% 
  st_crop(map.base)
map.base2.zb <- st_crop(map.base2, z.buf)


map.world <- st_geometry(st_read("../../Ensemble Case Study/GIS_Work/Shapefiles/World_countries_trunc.shp"))
map.world.zb <- st_intersection(map.world, z.buf)


z <- eSDM::pts2poly_vertices(
  data.frame(x = c(-130, -130, -115, -115, -130), y = c(30, 40, 40, 30, 30)), 
  crs = 4326
)
list.vertices <- list(
  c(243, 32),
  c(238, 33),
  c(234, 38),
  c(234, 48.12),
  c(235.210364011, 48.5375173285),
  c(235.494102374, 48.3877105965),
  c(237, 48),
  c(237, 40),
  c(240, 36),
  c(243, 36),
  c(243, 32)
)
z <- eSDM::pts2poly_vertices(
  data.frame(do.call(rbind, list.vertices)), 
  crs = 4326
) %>% 
  st_wrap_dateline()
z.buf <- st_buffer(z, 1)

st_erase <- function(x, y) st_difference(x, st_union(st_combine(y)))


###############################################################################
plot(st_geometry(map.world.zb), axes = TRUE)
plot(z, add = TRUE, border = "red")

system.time(grid.3km.e <- st_erase(grid.3km, map.base.zb)) #~54s
grid.3km.e2 <- grid.3km.e %>% 
  mutate(area_km2 = as.numeric(units::set_units(st_area(geometry), km^2)), 
         area_diff = area_km - area_km2)


summary(grid.3km.e2$area_km)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 5.988   6.591   7.092   6.981   7.388   7.636 

summary(grid.3km.e2$area_diff)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00000 0.00000 0.00000 0.09308 0.00000 7.56348 


###############################################################################
rgdal::ogrListLayers("C:/Users/sam.woodman/Downloads/NOAA Shoreline/CUSP_Western_geo.shp")
x <- st_read("C:/Users/sam.woodman/Downloads/NOAA Shoreline/CUSP_Western_geo.shp")



###############################################################################

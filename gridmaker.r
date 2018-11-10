#  gridmaker.r --- creates uniform grid of decimal lat/longs based on 
#  input boundaries and pixel size (in degrees).  Calculates area size 
#  of each pixel in sq.km

rm(list=ls())                          # clears workspace
library(geosphere)
pixel.mat <- matrix(NA,5,2)
colnames(pixel.mat) <- c("x","y") 
eq.radius <- 6378.137

latmin <- 32.5
latmax <- 42.0
lonmin <- 360 - 125.0
lonmax <- 360 - 118.0
# pixel <- .225                 # 25km
# pixel <- .090                 # 10km
# pixel <- 0.10                 # 0.1 degree for SeaGrant modeling project
pixel <- .045                 # 5km
# pixel <- .018                 # 2km  (not run)

grid <- data.frame(lat=NULL,lon=NULL)
for (lat in seq(latmin,latmax, by=pixel)) {
  for (lon in seq(lonmin,lonmax, by=pixel)) {
    lon180 <- lon-360
    if (lat<latmax & lon<lonmax) {
      pixel.mat[1,]=c(lon180,lat)
      pixel.mat[2,]=c(lon180+pixel,lat)
      pixel.mat[3,]=c(lon180+pixel,lat+pixel)
      pixel.mat[4,]=c(lon180,lat+pixel)
      pixel.mat[5,]=pixel.mat[1,]
    }
    pixel.area <- areaPolygon(pixel.mat,eq.radius)
    grid <- rbind(grid, data.frame(lat=lat, lon360=lon, lon180=lon180, pixelkm2=pixel.area))       
  }
}

outfil = paste("Grid_Lat",latmin,"to",latmax,"_Lon",lonmin,"to",lonmax,"_Step",pixel,"withArea.csv",sep="")
write.csv(grid, outfil)

dim(grid)




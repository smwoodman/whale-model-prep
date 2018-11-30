# Code for exploring if the CCSRA nc file indices that have NAs have NAs for
# all variables and dates.
# Result: yes they do


###############################################################################
x1 <- read.csv("../whale-model-prep_data/Grid/Grid_CCSRA/WEAR_3km_2005-01-01.csv")

# x1.out <- x1 %>% 
#   mutate(na_flag = as.numeric(is.na(sst.mean))) %>% 
#   select(lat, lon, na_flag)
# write.csv(
#   x1.out, row.names = FALSE,
#   file = "../whale-model-prep_data/Grid/Grid_CCSRA_na_WEAR.csv"
# )

# y <- read.csv("../whale-model-prep_data/Grid/Grid_Nonrectangle_3km_WEAR_bathy.csv")
# 
# sum(!(which(is.na(x1$sst.mean)) %in% which(is.na(y$depth))))
# 
# nrow(x1) #85,869
# sum(is.na(x1$sst.mean))
# sum(is.na(x1$sst.mean)) - sum(is.na(x1$sst.SD)) #same for ssh and ild
# 
# x1.sst <- which(is.na(x1$sst.mean))
# x1.ssh <- which(is.na(x1$ssh.mean))
# x1.ild <- which(is.na(x1$ild.mean))
# 
# identical(x1.sst, x1.ssh)
# identical(x1.sst, x1.ild)


# x2 <- read.csv("../whale-model-prep_data/Grid/Grid_CCSRA_pre/WEAR_3km_2005-01-03.csv")
# x2.sst <- which(is.na(x2$sst.mean))
# x2.ssh <- which(is.na(x2$ssh.mean))
# x2.ild <- which(is.na(x2$ild.mean))
# 
# identical(x1.sst, x2.sst)
# identical(x1.sst, x2.ssh)
# identical(x1.sst, x2.ild)


###############################################################################
# For all lon/lat coords in rows x1.sst, extract values for grid.dates from applicable nc files

### Determine the nc file lat/long index that each grid point is closest to
# load("../whale-model-prep_data/Grid/Grid_CCSRA_idx.RDATA") #Created in "CCSRA_2D_Grid_WEAR.R"
# Assumes that indices are constant accross CCSRA nc files
nc.data <- nc_open("../whale-model-prep_data/CCSRA_nc/CCSRA_wcra31_daily_2D_Jacox/wcra31_sst_daily_1991_2010.nc")
nc.lon <- ncvar_get(nc.data, 'lon')[, 1]
nc.lat <- ncvar_get(nc.data, 'lat')[1, ]
nc_close(nc.data)



x1$ccsra.idx <- apply(x1[, c("lon", "lat")], 1, function(i) {
  r.lon <- which.min(abs(nc.lon - i[1]))
  c.lat <- which.min(abs(nc.lat - i[2]))
  paste(r.lon, c.lat, sep = "-")
})
x1.na <- x1 %>% 
  mutate(ccsra.lon.idx = map_dbl(strsplit(ccsra.idx, "-"), function(i) as.numeric(i[1])),
         ccsra.lat.idx = map_dbl(strsplit(ccsra.idx, "-"), function(i) as.numeric(i[2]))) %>% 
dplyr::filter(is.na(sst.mean)) %>% 
  dplyr::select(lon, lat, ccsra.lon.idx, ccsra.lat.idx, ccsra.idx)


# Remove duplicates lon/lat indices for sake of speed
i <- x1.na$ccsra.lon.idx[!duplicated(x1.na$ccsra.idx)]
j <- x1.na$ccsra.lat.idx[!duplicated(x1.na$ccsra.idx)]

### Load nc files and get lengths of their time dimensions
nc.ccsra1.sst <- nc_open("../whale-model-prep_data/CCSRA_nc/CCSRA_wcra31_daily_2D_Jacox/wcra31_sst_daily_1991_2010.nc")
nc.ccsra2.sst <- nc_open("../whale-model-prep_data/CCSRA_nc/CCSRA_NRT2011-2017_daily_2D_Jacox/wcnrt_sst_daily_20110102_20170419.nc")
nc.ccsra3.sst <- nc_open("../whale-model-prep_data/CCSRA_nc/CCSRA_NRT2011-2017_daily_2D_Jacox/wcnrt_sst_daily_20170420_20180430.nc")

nc.ccsra1.ssh <- nc_open("../whale-model-prep_data/CCSRA_nc/CCSRA_wcra31_daily_2D_Jacox/wcra31_ssh_daily_1991_2010.nc")
nc.ccsra2.ssh <- nc_open("../whale-model-prep_data/CCSRA_nc/CCSRA_NRT2011-2017_daily_2D_Jacox/wcnrt_ssh_daily_20110102_20170419.nc")
nc.ccsra3.ssh <- nc_open("../whale-model-prep_data/CCSRA_nc/CCSRA_NRT2011-2017_daily_2D_Jacox/wcnrt_ssh_daily_20170420_20180430.nc")

nc.ccsra1.ild <- nc_open("../whale-model-prep_data/CCSRA_nc/CCSRA_wcra31_daily_2D_Jacox/wcra31_ild_daily_1991_2010.nc")
nc.ccsra2.ild <- nc_open("../whale-model-prep_data/CCSRA_nc/CCSRA_NRT2011-2017_daily_2D_Jacox/wcnrt_ild_daily_20110102_20170419.nc")
nc.ccsra3.ild <- nc_open("../whale-model-prep_data/CCSRA_nc/CCSRA_NRT2011-2017_daily_2D_Jacox/wcnrt_ild_daily_20170420_20180430.nc")

nc.ccsra1.time <- length(ncvar_get(nc.ccsra1.sst, "year"))
nc.ccsra2.time <- length(ncvar_get(nc.ccsra2.sst, "year"))
nc.ccsra3.time <- length(ncvar_get(nc.ccsra3.sst, "year"))

# Sanity check
all(
  identical(nc.ccsra1.time, length(ncvar_get(nc.ccsra1.ssh, "year"))), 
  identical(nc.ccsra1.time, length(ncvar_get(nc.ccsra1.ild, "year"))), 
  identical(nc.ccsra2.time, length(ncvar_get(nc.ccsra2.ssh, "year"))), 
  identical(nc.ccsra2.time, length(ncvar_get(nc.ccsra2.ild, "year"))), 
  identical(nc.ccsra3.time, length(ncvar_get(nc.ccsra3.ssh, "year"))), 
  identical(nc.ccsra3.time, length(ncvar_get(nc.ccsra3.ild, "year")))
)

### Test that nc files have NA values for all time points for all lon/lat indices ID'd in x1
# # One time
# nc.ccsra1.sst.1 <- ncvar_get(nc.ccsra1.sst, "sst", start = c(i[1], j[1], 1), count = c(1, 1, 7300))
# sum(is.na(nc.ccsra1.sst.1))

# Run test for all values for all files
d.test <- function(nc.data, time.len, idx.cbind, var) {
  apply(idx.cbind, 1, function(k) {
    temp <- ncvar_get(nc.data, var, start = c(k[1], k[2], 1), count = c(1, 1, time.len))
    sum(!is.na(temp)) == 0
  })
}


all(d.test(nc.ccsra1.sst, nc.ccsra1.time, cbind(i, j), "sst"))
all(d.test(nc.ccsra2.sst, nc.ccsra2.time, cbind(i, j), "sst"))
all(d.test(nc.ccsra3.sst, nc.ccsra3.time, cbind(i, j), "sst"))

all(d.test(nc.ccsra1.ssh, nc.ccsra1.time, cbind(i, j), "ssh"))
all(d.test(nc.ccsra2.ssh, nc.ccsra2.time, cbind(i, j), "ssh"))
all(d.test(nc.ccsra3.ssh, nc.ccsra3.time, cbind(i, j), "ssh"))

all(d.test(nc.ccsra1.ild, nc.ccsra1.time, cbind(i, j), "ild"))
all(d.test(nc.ccsra2.ild, nc.ccsra2.time, cbind(i, j), "ild"))
all(d.test(nc.ccsra3.ild, nc.ccsra3.time, cbind(i, j), "ild"))


# Clean up
nc_close(nc.ccsra1.sst)
nc_close(nc.ccsra2.sst)
nc_close(nc.ccsra3.sst)
nc_close(nc.ccsra1.ssh)
nc_close(nc.ccsra2.ssh)
nc_close(nc.ccsra3.ssh)
nc_close(nc.ccsra1.ild)
nc_close(nc.ccsra2.ild)
nc_close(nc.ccsra3.ild)


###############################################################################
# Export grid as a shapefile with a column indicating NA's
library(sf)
# No smart
x0 <- read.csv("../whale-model-prep_data/Grid/Grid_CCSRA_pre/WEAR_3km_2005-01-01_nosmart")
x0.shp <- x0 %>% 
  st_sf(geometry = eSDM::pts_to_sfc_centroids(x0[, c(2, 1)], 0.027 / 2, 4326), crs = 4326) %>% 
  dplyr::mutate(na_flag = as.numeric(is.na(sst.mean)))
plot(x0.shp["na_flag"], axes = TRUE, border = NA)

st_write(x0.shp, "../whale-model-prep_data/shapefiles/grid_ccsra_naflag_nosmart.shp")


# Smartcheck
x1 <- read.csv("../whale-model-prep_data/Grid/Grid_CCSRA/WEAR_3km_2005-01-01.csv")
x1.shp <- x1 %>% 
  st_sf(geometry = eSDM::pts_to_sfc_centroids(x1[, c(2, 1)], 0.027 / 2, 4326), crs = 4326) %>% 
  dplyr::mutate(na_flag = as.numeric(is.na(sst.mean)))
plot(x1.shp["na_flag"], axes = TRUE, border = NA)

st_write(x1.shp, "../whale-model-prep_data/shapefiles/grid_ccsra_naflag_smart.shp")


###############################################################################
# Export CCSRA nc file point locations to shapefile
nc.data <- nc_open("../whale-model-prep_data/CCSRA_nc/CCSRA_wcra31_daily_2D_Jacox/wcra31_sst_daily_1991_2010.nc")
x <- ncvar_get(nc.data, "lon")[, 1]
y <- ncvar_get(nc.data, "lat")[1, ]
xy <- data.frame(expand.grid(x, y))
xy.sfc <- st_geometry(st_as_sf(xy, coords = c(1, 2), crs = 4326))
st_write(xy.sfc, "../whale-model-prep_data/shapefiles/nc_points_sfc.shp")

###############################################################################

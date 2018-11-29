###############################################################################
x1 <- read.csv("../whale-model-prep_data/Grid/Grid_CCSRA_pre/WEAR_3km_2005-01-01.csv")
x2 <- read.csv("../whale-model-prep_data/Grid/Grid_CCSRA_pre/WEAR_3km_2005-01-03.csv")

# x1.sst <- which(is.na(x1$sst.mean))
# x1.ssh <- which(is.na(x1$ssh.mean))
# x1.ild <- which(is.na(x1$ild.mean))
# 
# identical(x1.sst, x1.ssh)
# identical(x1.sst, x1.ild)
# 
# 
# x2.sst <- which(is.na(x2$sst.mean))
# x2.ssh <- which(is.na(x2$ssh.mean))
# x2.ild <- which(is.na(x2$ild.mean))
# 
# identical(x1.sst, x2.sst)
# identical(x1.sst, x2.ssh)
# identical(x1.sst, x2.ild)


###############################################################################
# For all lon/lat coords in rows x1.sst, extract values for grid.dates from applicable nc files

### Load and prep
load("../whale-model-prep_data/Grid/Grid_CCSRA_idx.RDATA")
x1.na <- x1 %>% 
  mutate(ccsra.lon.idx = z.lon.idx, ccsra.lat.idx = z.lat.idx, 
         ccsra.idx = paste(z.lon.idx, z.lat.idx, sep = "-")) %>% 
  dplyr::filter(is.na(sst.mean))

# Remove duplicates lon/lat indices for sake of speed
length(unique(x1$ccsra.idx))
i <- x1.na$ccsra.lon.idx[!duplicated(x1.na$ccsra.idx)] #z.lon.idx.unique
j <- x1.na$ccsra.lat.idx[!duplicated(x1.na$ccsra.idx)] #z.lon.idx.unique

# Load nc files and get lengths of their time dimensions
nc.ccsra1.sst <- nc_open("../whale-model-prep_data/CCSRA nc files/CCSRA_wcra31_daily_2D_Jacox/wcra31_sst_daily_1991_2010.nc")
nc.ccsra2.sst <- nc_open("../whale-model-prep_data/CCSRA nc files/CCSRA_NRT2011-2017_daily_2D_Jacox/wcnrt_sst_daily_20110102_20170419.nc")
nc.ccsra3.sst <- nc_open("../whale-model-prep_data/CCSRA nc files/CCSRA_NRT2011-2017_daily_2D_Jacox/wcnrt_sst_daily_20170420_20180430.nc")

nc.ccsra1.ssh <- nc_open("../whale-model-prep_data/CCSRA nc files/CCSRA_wcra31_daily_2D_Jacox/wcra31_ssh_daily_1991_2010.nc")
nc.ccsra2.ssh <- nc_open("../whale-model-prep_data/CCSRA nc files/CCSRA_NRT2011-2017_daily_2D_Jacox/wcnrt_ssh_daily_20110102_20170419.nc")
nc.ccsra3.ssh <- nc_open("../whale-model-prep_data/CCSRA nc files/CCSRA_NRT2011-2017_daily_2D_Jacox/wcnrt_ssh_daily_20170420_20180430.nc")

nc.ccsra1.ild <- nc_open("../whale-model-prep_data/CCSRA nc files/CCSRA_wcra31_daily_2D_Jacox/wcra31_ild_daily_1991_2010.nc")
nc.ccsra2.ild <- nc_open("../whale-model-prep_data/CCSRA nc files/CCSRA_NRT2011-2017_daily_2D_Jacox/wcnrt_ild_daily_20110102_20170419.nc")
nc.ccsra3.ild <- nc_open("../whale-model-prep_data/CCSRA nc files/CCSRA_NRT2011-2017_daily_2D_Jacox/wcnrt_ild_daily_20170420_20180430.nc")

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

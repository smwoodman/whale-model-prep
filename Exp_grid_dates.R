d1 <- nc_open("../whale-model-prep_data/CCSRA nc files/CCSRA_NRT2011-2015_daily_2D_Jacox/wcnrt_daily/wcnrt_sst_daily_2011_2015.nc")

d2 <- nc_open("../whale-model-prep_data/CCSRA nc files/CCSRA_NRT2011-2017_daily_2D_Jacox/wcnrt_sst_daily_20110102_20170419.nc")
d22 <- nc_open("../whale-model-prep_data/CCSRA nc files/CCSRA_NRT2011-2017_daily_2D_Jacox/wcnrt_sst_daily_20170420_20180430.nc")

d3 <- nc_open("../whale-model-prep_data/CCSRA nc files/CCSRA_wcra31_daily_2D_Jacox/wcra31_sst_daily_1991_2010.nc")

d1.year  <- ncvar_get(d1, 'year')
d1.month <- ncvar_get(d1, 'month')
d1.day   <- ncvar_get(d1, 'day')

d2.year  <- ncvar_get(d2, 'year')
d2.month <- ncvar_get(d2, 'month')
d2.day   <- ncvar_get(d2, 'day')

d22.ymd <- as.Date(paste(ncvar_get(d22, 'year'), ncvar_get(d22, 'month'), ncvar_get(d22, 'day'), sep = "-"))

d3.year  <- ncvar_get(d3, 'year')
d3.month <- ncvar_get(d3, 'month')
d3.day   <- ncvar_get(d3, 'day')
d3.ymd <- as.Date(paste(ncvar_get(d3, 'year'), ncvar_get(d3, 'month'), ncvar_get(d3, 'day'), sep = "-"))


x <- grid.dates[grid.dates > as.Date("2011-01-01")]
y <- seq(as.Date("2011-01-01"), as.Date("2017-12-31"), by = 1)
z <- grid.dates[grid.dates < as.Date("2011-01-01")]

x[which(!(x %in% d2.ymd))]

sum(!(x %in% c(d2.ymd, d22.ymd)))
sum(!(y %in% c(d2.ymd, d22.ymd)))


tail(as.Date(paste(ncvar_get(d3, 'year'), ncvar_get(d3, 'month'), ncvar_get(d3, 'day'), sep = "-")))
z[(!z %in% d3.ymd)]

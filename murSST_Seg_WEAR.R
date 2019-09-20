# Code written for WEAR project, Nov 2018
# Based on '02d-get murSST.R' by Anita...
# Extract murSST data for segement data, "LgWhale_CCE_91_14_3km_Segs_BF0_6.csv"
# Data downloaded in murSST_download_WEAR.R 
# Data (analysed_sst, every day 2005-2017, one nc file for each day) stored at
#   "J:\Sam_Woodman\mursst_nc", where "J:\" is "mmdisk". 
# murSST only goes back to 2005, so no data for earlier cruise data
# For each segment point: get point value then 
#   sd for 1, 4 (9x9km), 12 (25x25km) pixels each way
#
# By Sam Woodman

###############################################################################
library(dplyr)
library(ncdf4)
library(purrr)

### Paths to files
source("User_script_local.R", local = TRUE, echo = FALSE)

## User path descriptions
# nc.path:  Top-level folder for murSST nc files (this folder contains yearly folders, e.g. '2019')
# seg.path: Folder with segment file

if (user == "KAF") {
  nc.path  <- "" 
  seg.path <- ""
  
} else if (user == "EAB") {
  nc.path  <- ""
  seg.path <- ""
  
} else if (user == "SMW") {
  # nc.path <- "../whale-model-prep_data/mursst_nc/"
  nc.path <- "J:/Sam_Woodman/mursst_nc/" # J is mmdisk on SMW computer
  seg.path <- "../whale-model-prep_data/Segments/"
  
} else {
  stop("Invalid value supplied for 'user' object")
}


###############################################################################
### Prep
infile        <- paste0(seg.path, "LgWhale_CCE_91_14_3km_Segs_BF0_6_Dec13_2018.csv")
outfile       <- paste0(seg.path, "WEAR_seg_mursst.csv")
seg.data.orig <- read.csv(infile, stringsAsFactors = FALSE)
seg.data.out  <- seg.data.orig
varname <- 'analysed_sst' 


# Number of rows in each direction from center point for which to get data
pixel.radius  <- 12 


# Generate filenames of mursst nc files
temp <- seg.data.orig %>% 
  mutate(month_chr = formatC(month, width = 2, format = "d", flag = "0"), 
         day_chr = formatC(day, width = 2, format = "d", flag = "0")) %>% 
  select(year, month_chr, day_chr) %>% 
  mutate(file_nc = pmap_chr(., paste, sep = "-")) %>% 
  mutate(file_nc = paste0(nc.path, year, "/mursst_", file_nc, "_(-132)-(-116)-(29)-(49).nc"))

seg.data <- seg.data.orig %>% 
  mutate(file_nc = temp$file_nc) %>% 
  filter(year >= 2005) %>% 
  select(mlon, mlat, year, month, day, file_nc)
rm(temp)


# Get nc file lat/lon info - this will be the same across mursst nc files
nc.temp  <- nc_open(seg.data$file_nc[1])
nc.lon   <- ncvar_get(nc.temp, "longitude")
nc.lat   <- ncvar_get(nc.temp, "latitude")
nc.nrows <- length(nc.lon)
nc.ncols <- length(nc.lat)
nc_close(nc.temp); rm(nc.temp)

###############################################################################
### For each segment point, open applicable nc file and get needed data
# 88 sec for segment file (13,923 points)
# 95-100 sec when using for() loop
# Note: for segments, there were 0 points were center point had NA value
#   but one of surrounding had non-NA value
seg.data.out$temp_nc <- c(
  rep(NA, (nrow(seg.data.out) - nrow(seg.data))), 
  apply(seg.data, 1, function(i) {
    if (anyNA(i)) {
      warning("A longitude or latitude was NA")
      NA
      
    } else {
      # Open nc file and get data. Note that nc file is only for 1 day
      nc.data  <- nc_open(i["file_nc"])
      
      lonlat <- c(as.numeric(c(i["mlon"], i["mlat"])))
      r.lon <- which.min(abs(nc.lon - lonlat[1])) #which.min selects first element by default
      c.lat <- which.min(abs(nc.lat - lonlat[2])) #which.min selects first element by default
      
      # nrows and ncols are used if we are at the edge of the nc file grid
      row1    <- max(r.lon - pixel.radius, 1)
      numrows <- min(r.lon + pixel.radius, nc.nrows) - row1 + 1  
      col1    <- max(c.lat - pixel.radius, 1)                        
      numcols <- min(c.lat + pixel.radius, nc.ncols) - col1 + 1
      if (!(numcols == 25 && numrows == 25)) {
        warning("At edge of nc file grid: ", lonlat)
      }
      
      # Check that nc file only has data for 1 day
      if (length(ncvar_get(nc.data, "time")) > 1) warning("nc time error")
      
      # Get values of closest nc file point and pixel.radius surrounding points
      pred.data <- ncvar_get(
        nc.data, varname, start = c(row1, col1, 1),
        count = c(numrows, numcols, 1), verbose = FALSE
      )
      nc_close(nc.data)
      
      idx.cent <- c(1 + (r.lon - row1), 1 + (c.lat - col1))
      idx.04 <- list(
        max(idx.cent[1] - 4, 1):min(idx.cent[1] + 4, numrows), 
        max(idx.cent[2] - 4, 1):min(idx.cent[2] + 4, numcols)
      )
      
      if (is.na(pred.data[idx.cent[1], idx.cent[2]][1])) {
        warning("Variable value was NA")
      }
      
      list(
        pred.data[idx.cent[1], idx.cent[2]][1], 
        sd(pred.data[idx.04[[1]], idx.04[[2]]], na.rm = TRUE), 
        sd(pred.data, na.rm = TRUE)
      )
    }
  })
)


# Get data out of list-column
seg.data.out <- seg.data.out %>% 
  mutate(mursst = purrr::map_dbl(temp_nc, function(j) ifelse(anyNA(j), NA, j[[1]])), 
         mursst_sd4 = purrr::map_dbl(temp_nc, function(j) ifelse(anyNA(j), NA, j[[2]])), 
         mursst_sd12 = purrr::map_dbl(temp_nc, function(j) ifelse(anyNA(j), NA, j[[3]]))) %>% 
  select(-temp_nc)

sum(is.na(seg.data.out$mursst))
sum(is.na(seg.data.out$mursst_sd4))
sum(is.na(seg.data.out$mursst_sd12))


# Save to file
write.table(
  seg.data.out, outfile, sep = "," , col.names = TRUE, row.names = FALSE
)

###############################################################################

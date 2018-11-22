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
library(ncdf4)

# Paths to files
nc.path <- "../whale-model-prep_data/mursst_nc/"
# nc.path <- "J:/Sam_Woodman/mursst_nc" # J is mmdisk on SMW computer

seg.path <- "../whale-model-prep_data/Segments/"

###############################################################################
# Prep
infile     <- paste0(seg.path, "LgWhale_CCE_91_14_3km_Segs_BF0_6.csv")
outfile    <- paste0(seg.path, "WEAR_seg_mursst.csv")
in.data    <- read.csv(infile)
num.pts    <- nrow(in.data)
lon        <- in.data$lon180
lat        <- in.data$lat

out.data <- in.data

###############################################################################

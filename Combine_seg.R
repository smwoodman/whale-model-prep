# Code for getting variables from various segment .csv files and
#   saving them to single file
# Segment data is located at 'C:\SMW\WEAR\whale-model-prep_data\Segments'
# By Sam Woodman, Nov 2018, WEAR project


###############################################################################
rm(list = ls())

source("User_script_local.R", local = TRUE, echo = FALSE)
if (user == "KAF") {
  path <- ""
  outfile <- ""
  
} else if (user == "EAB") {
  path <- ""
  outfile <- ""
  
} else if (user == "SMW") {
  path <- "../whale-model-prep_data/Segments/"
  outfile <- "../whale-model-prep_data/Segments/WEAR_seg_all.csv"
  
} else {
  stop("Invalid value supplied for 'user' object")
}


###############################################################################
### Read in all data
segs.orig   <- read.csv(paste0(path, "LgWhale_CCE_91_14_3km_Segs_BF0_6_Dec13_2018.csv"), stringsAsFactors = FALSE)
segs.bathy  <- read.csv(paste0(path, "WEAR_seg_bathy.csv"), stringsAsFactors = FALSE)
segs.mursst <- read.csv(paste0(path, "WEAR_seg_mursst.csv"), stringsAsFactors = FALSE)
segs.roms   <- read.csv(paste0(path, "WEAR_seg_CCSRA.csv"), stringsAsFactors = FALSE)

# ### Sanity check
# identical(seg.orig, seg.bathy[, 1:40])
# identical(seg.orig, seg.mursst[, 1:40])
# identical(seg.orig[, 1], seg.roms[, 1])

### Combine and save data
segs.out <- segs.orig %>% 
  bind_cols(segs.bathy[, c("depth", "depth_sd")], 
            segs.mursst[, c("mursst", "mursst_sd4", "mursst_sd12")], 
            segs.roms[, c("sst.mean", "sst.SDspace", "ssh.mean", "ssh.SDspace", "ild.mean", "ild.SDspace")])

# sum(is.na(segs.out[, 41:51]))
# sum(is.na(segs.out$mursst))
# sum(is.na(segs.out$sst.mean))
# sum(is.na(segs.out$sst.SDspace))

write.table(segs.out, outfile, sep = "," , col.names = TRUE, row.names = FALSE)

###############################################################################

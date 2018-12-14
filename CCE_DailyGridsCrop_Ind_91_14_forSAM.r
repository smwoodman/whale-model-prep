############################# CCE_DailyGridsCrop_Ind_91_14.r ###############################################
#
#  This code was modified from CalCOFI_WeeklyGrids_Crop_Ind_ROMS.r (for CalCOFI w/s modeling project)
#  to create daily grid-based predictions for the CCE study area.  Selected models are set up in
#  separate files (e.g.,"CCE_Build91_14_Ind.r") which are run using a 'source' command.  
#  Note that this program clears the workspace to make sure no pre-existing objects cause problems.
#
# In addition to the source file,the program requires the user to specify two input files: 
#    1) bathydata:  a file with grid lat/lons and corresponding static predictors
#    2) vardatafil:  a set of corresponding files with daily dynamic ROMS predictors:
#                    these files should be in a separate folder, specified in the main loop
#
#   The program creates one main output object, writen to the file species.pred.file, with the 
#   grid pixel lat/lons and daily columns for density.
#   The first eight columns in the output file are: # days with non-NA values (n), average 
#   density, stdev density, lower and upper 90% conf limit using stdev, SE(density), and
#   lower and upper 90% using SE.
#
#   Last revised (EAB): 02/24/2017
#
#
### Dec 2018 - Sam Woodman
# Modified to make code more general/flexible, i.e. able to handle predicting on different years
#
####################################################################################################
# Clear workspace
rm(list = ls())
library(lubridate)
library(sf)
library(splancs)

### Run build file
source(paste0("CCE_Build91_14_Ind_forSAM.r"))

# Check results to verify
summary(ind.gam.CCE)

# plot(ind.gam.CCE,scale=0,residuals=TRUE)
#  par(mfrow=c(2,2))
#  plot(ind.gam.CCE,scale=0, shade=TRUE)


### Species codes (SpNum):
#
# 13 = Striped dolphin (Sc)
# 16 = Long-beaked common dolphin (Dc)
# 17 = Short-beaked common dolphin (Dd)
# 18 = Common bottlenose dolphin (Tt)
# 21 = Risso's dolphin (Gg)
# 22 = Pacific white-sided dolphin (Lo) 
# 27 = northern right whale dolphin (Lb)
# 44 = Dall's porpoise (Pd)
# 46 = sperm whale(Pm)
# 51 = Mesoplodon spp. (sb) (Note that for small beaked whale guild, "51" is inclusive of 
#                             codes 51, 52, 57, 59, 61, 81, 83, 109 - computed below.)
# 63 = Baird's beaked whale (Bb)
# 74 = fin whale (Bp)
# 75 = blue whale (Bm)
# 76 = humpback whale (Mn)


### Set up output file location; 'SpNum' specified in Build code and is integer with sp abbr as name
# species.pred.file <- paste0("C:/Users/EABECKER/Documents/R_grid_preds_ROMS/CCE_91_14_preds/", SpNum ,"_dailyPreds_1991_2014.csv")
species.pred.path <- paste0("../Exp_CCE_Ind/Outputs/")
species.pred.file <- paste0(species.pred.path, SpNum ,"_dailyPreds_exp.csv")


### Determine whether densities should be cropped to study area (plus buffer)
# (This will take a few minutes extra to run, before entering main prediction loop).   
# For 10-km 49729 segments, takes 3-4 minutes. SMW update: much faster using sf package
Crop.Area = TRUE
Crop.Buffer = 0.10/2   #Buffer in degrees -- allow at least 1/2 pixel size to avoid gaps at edges
Crop.areafil <- "../Exp_CCE_Ind/CCE_StudyArea_noIslands_noBays.csv"


### Specify folder for needed files with grid midpoints and static predictors 
# gridpath <-"C:/Users/EABECKER/Documents/HabModels_CCE_1991_2014/Grid_data/"
gridpath <-"../Exp_CCE_Ind/Grid/"
gridfile <- paste0(gridpath, "BathyVarsCCEGrid.csv")


### Specify dates for which to predict and the years for which to summarize data
dataweeks <- c(as.Date("2001/06/26"), as.Date("2001/06/27"), 
               as.Date("2005/06/28"), as.Date("2005/06/29"))
years2summ <- c(2001, 2005) # SMW question: Should this just be any year in 'dataweeks'?
if (!all(lubridate::year(dataweeks) %in% years2summ)) {
  warning("You are not summarizing all the years for which you are predicting")
}
# write.csv(dataweeks, "Grid.dates.csv")  # save dates for reference


#  OPTIONAL TASKS:  Change day.start, day.end, and dynamic variable details in loop below
#     Also, update column names as needed (used for selecting columns to use)
#
############################ END USER TASKS #############################################
#
# Define predict.gam functions (defined in source code; not needed here)
#
#  Define function to calculate variance and stdev for rows of a data.frame
#  (KAF found these using a Google search on Splus rowStdevs R)  7 Nov 2013
#
rowVars <- function(x, na.rm=FALSE, dims=1, unbiased=TRUE, SumSquares=FALSE,
                    twopass=TRUE) {
  if (SumSquares) return(rowSums(x^2, na.rm, dims))
  N <- rowSums(!is.na(x), FALSE, dims)
  Nm1 <- if (unbiased) N-1 else N
  if (twopass) {
    x <- if (dims==0) x - mean(x, na.rm=na.rm) else sweep(x, 1:dims, rowMeans(x,na.rm,dims))
  }
  (rowSums(x^2, na.rm, dims) - rowSums(x, na.rm, dims)^2/N) / Nm1
}
rowStdevs <- function(x, ...) sqrt(rowVars(x, ...))
#
#########################################################################################
#  Now read / create prediction datasets. Longitudes are included on 0-360 scale

#----------------------------------------------------------
fixeddata <- read.table(gridfile, colClasses = "numeric", sep = ",", header = T, stringsAsFactors = F)
# fixeddata <- fixeddata[, c(1,2,3,5,6)]    ### Be sure to specify lon 360 column here
# names(fixeddata) <- c("pixel","mlat","mlon","depth","rugosity")
# fixeddata$effort <- rep(1,nrow(fixeddata))

fixeddata <- fixeddata %>% #Be sure to select lon 360 column here
  dplyr::select(pixel = LineID, mlat = lat, mlon = lon360, 
                depth = Depth_ETOPO1, rugosity = rugosity) %>% 
  dplyr::mutate(effort = 1)
species.data <- fixeddata %>% select(pixel, mlat, mlon)


#----------------------------------------------------------
#  If desired (Crop.Area == TRUE above), find and set CropIt=TRUE for pixels that are out of 
#  the study area.  These are then excluded below when setting up data4thisrun.  Default is
#  CropIt=FALSE
#
# CropIt <- rep(FALSE, nrow(fixeddata))
# if (Crop.Area == TRUE) {
#   incr <- Crop.Buffer
#   CropArea <- read.csv(Crop.areafil, header=TRUE)
#   
#   for (pix in 1:nrow(fixeddata)) { 
#     x1 <- fixeddata$mlon[pix]
#     y1 <- fixeddata$mlat[pix]
#     pixel.pts <- data.frame(x=c(x1-incr, x1-incr, x1+incr, x1+incr, x1-incr),
#                             y=c(y1-incr, y1+incr, y1+incr, y1-incr, y1-incr))
#     if (sum(inout(pixel.pts, CropArea, bound=TRUE, quiet=TRUE))==0)  {CropIt[pix]=TRUE}  
#   } 
# }  
# 

# sf way to generate CropIt; takes ~0.29 second for 33,666 points
if (Crop.Area == TRUE) {
  keep.df  <- read.csv(Crop.areafil, header = TRUE)
  keep.sfc <- st_sfc(st_polygon(list(as.matrix(keep.df))), crs = 4326)
  keep.sfc <- suppressWarnings(suppressMessages(st_buffer(keep.sfc, Crop.Buffer)))
  # plot(keep.sfc, axes = TRUE)
  
  pts.sfc  <- st_geometry(st_as_sf(fixeddata, coords = c("mlon", "mlat"), crs = 4326))
  
  keep.pts.int <- suppressMessages(st_intersects(keep.sfc, pts.sfc))[[1]]
  CropIt <- seq_along(pts.sfc) %in% keep.pts.int
  
  rm(keep.df, keep.sfc, pts.sfc, keep.pts.int)
}


#----------------------------------------------------------
### Change column names from the ROMS daily files to match the model-building variable names
# Names of the columns in the grid files
vardata.cols <- c("lat", "lon", "sst.mean", "sst.SD", "ssh.mean", "ssh.SD", "ild.mean", "ild.SD")

# What to rename the columns in the grid files
oceocols <- c("lat", "lon", "SST", "SSTsd", "SSH", "SSHsd", "MLD", "MLDsd")
stopifnot(length(vardata.cols) == length(oceocols))

# Variables needed for model
model.vars <- c("SST", "SSTsd", "SSH", "SSHsd", "MLD", "yearCoVar")

#----------------------------------------------------------
# Set up grid of ROMS input data with appropriate column
#
# dataweeks <- c(seq(as.Date("1991/06/26"), as.Date("1991/12/06"), by=1),
#                seq(as.Date("1993/06/26"), as.Date("1993/12/06"), by=1),
#                seq(as.Date("1996/06/26"), as.Date("1996/12/06"), by=1),
#                seq(as.Date("2001/06/26"), as.Date("2001/12/06"), by=1),
#                seq(as.Date("2005/06/26"), as.Date("2005/12/06"), by=1),
#                seq(as.Date("2008/06/26"), as.Date("2008/12/06"), by=1),
#                seq(as.Date("2009/06/26"), as.Date("2009/12/06"), by=1),
#                seq(as.Date("2014/06/26"), as.Date("2014/12/06"), by=1))

## For selected days
# dataweeks <- c(seq(as.Date("2001/06/26"), as.Date("2001/06/28"), by=1))
# dataweeks <- c(as.Date("2001/06/26"), as.Date("2001/06/27"), 
#                as.Date("2005/06/28"), as.Date("2005/06/29"))

# dataweeks.yrs <- lubridate::year(dataweeks)
# years2summ <- c(2001, 2005)
# if (!all(dataweeks.yrs %in% years2summ)) {
#   warning("You are not summarizing all the years for which you are pulling data")
# }
# write.csv(dataweeks,"Grid.dates.csv")  # save dates for reference

dataweeks.yrs <- lubridate::year(dataweeks)
yrweek <- gsub("-", ".", dataweeks) #Replaces - with . to allow creation of column names below

################################################################################################
# Specify starting and ending days here.  
# Note that some yrs/days do not have some ROMS variables
# SMW note: 2-2.3s per file -> ~80 minutes for 2374 

week.start <- 1               # Specify starting and ending DAY (index) for grid predictions
week.end   <- length(dataweeks) #

denscol <- (week.end - week.start) + 1
alldens <- matrix(nrow = nrow(fixeddata), ncol = denscol)   # to be used for average densities below
colnames(alldens) <- paste0("Dens", yrweek[week.start:week.end])

t1 <- Sys.time()
print(t1)

for (i in week.start:week.end) {        # specify weeks above
  vardatafil <- paste0(gridpath, "CCSRA_pred_grids/CCE_0.1deg_", dataweeks[i], ".csv")  # specify ROMS filenames
  vardata <- read.table(vardatafil, sep = ",", header = T)
  #   vardata <- read.table(vardatafil, colClasses="numeric", sep = ",",header=T, stringsAsFactors=F)
  
  # Check to ensure that required column names are in opened file
  if (!all(vardata.cols %in% names(vardata))) { 
    stop("File ", "'CCE_0.1deg_", dataweeks[i], ".csv' ", 
         "does not contain the required column names")
  }
  
  vardata <- vardata[, vardata.cols]
  names(vardata) <- oceocols
  
  #  Add year covariate (needed for some species with GAMs including year as a covariate to capture trends)
  # gridYR <- substr(dataweeks[i], 1, 4)
  vardata$yearCoVar <- dataweeks.yrs[i]
  # vardata$yearCoVar <- (vardata$year - 2000)
  
  
  #  Combine predictors 
  alldata <- cbind(fixeddata, vardata[, model.vars])  # specify variables you need for model
  nID <- nrow(alldata)
  # OnLand <- rep(F, nID)
  # OnLand[which(alldata$depth > 0)] <- T
  OnLand <- alldata$depth > 0
  TFna <- fn.get.na(alldata)    #Find rows with NA values in relevant columns 
  ok <- which(TFna == F & OnLand == F & CropIt == F)    #  fn.get.na is defined in the BUILD code sourced above
  data4thisrun <- alldata[ok, ] 
  
  
  # run predictions and store them somewhere
  p.ind.gam <- predict.gam.null.ck.log.off(ind.gam.CCE, data4thisrun, type = "response") #Takes ~1.85s
  p.ind.gam.all.lines <- rep(NA, nID)
  p.ind.gam.all.lines[ok] <- p.ind.gam
  
  dens <- p.ind.gam.all.lines  
  
  # Create output data.frame for this species
  new.names <- c(names(species.data), paste0(SpNum, ".dens.", yrweek[i]))
  
  species.data <- cbind(species.data, dens)
  names(species.data) <- new.names
  
  alldens[, i-week.start+1] <- dens
  
}; rm(i)  # end of for i loop

t2 <- Sys.time()
print(Sys.time() - t1)

#
# Remove objects that are no longer needed to free up memory
#
rm(wc8yrs, wc8yrs.nona, wc8yrs.nona.no0, t1, t2)
rm(alldata, vardata, fixeddata, data4thisrun, dens, TFna, ok)


################################################################################################
# When entire species.data frame created, calculate three additional columns 
# with mean, SD and n for monthly densites.  Write to output csv file 
# (need to copy headers to the first row so we can use dimnames.write=F to  
# get rid of the stupid rownames)

zval <- qnorm(0.95)

nrow(alldens)
avgdens <- rowMeans(alldens, na.rm=T)
sumdens <- rowSums(alldens, na.rm=T)
ndens <- sumdens/avgdens

# Calculate standard deviation of the monthly densities
stdevdens <- rowStdevs(alldens, na.rm=T)
CVstddens <- stdevdens/avgdens
L90std <- avgdens/exp(zval*sqrt(log(1+(CVstddens^2))))   # qnorm(0.95) gives z for 
U90std <- avgdens*exp(zval*sqrt(log(1+(CVstddens^2))))   #  2-tailed 90% C.I. 

# Calculate standard error of the mean of the monthly densities
SEdens <- stdevdens/sqrt(ndens)
CVSEdens <- SEdens/avgdens
L90SE <- avgdens/exp(zval*sqrt(log(1+(CVSEdens^2))))   # qnorm(0.95) gives z for 
U90SE <- avgdens*exp(zval*sqrt(log(1+(CVSEdens^2))))   #  2-tailed 90% C.I. 


#------------------------------------------------------------------------------
### Create yearly averages for (Surfer) plotting
# years2summ variable set up by dataweeks

year_avg_dens <- function(alldens.f, dataweeks.f, years2summ.f, species.data.f) {
  avgdens.list <- lapply(years2summ.f, function(i) {
    curr <- alldens.f[, lubridate::year(dataweeks.f) == i]
    rowMeans(curr, na.rm = TRUE)
  })
  stopifnot(length(avgdens.list) == length(years2summ.f))
  
  yeardens <- cbind(species.data.f[, 1:3], avgdens.list)
  colnames(yeardens) <- c("pixel", "mlat", "mlon", years2summ.f)
  
  yeardens
}

yeardens <- year_avg_dens(alldens, dataweeks, years2summ, species.data)
write.table(
  yeardens, sep = ",", row.names = FALSE, 
  file = paste0(species.pred.path, names(SpNum), "_yearlyDens_ROMSdaily_XY_YR_MAXDF_15.csv")
)

# # Create yearly averages for Surfer plotting
# weeks1991 <- alldens[,1:164]
# weeks1993 <- alldens[,165:328]
# weeks1996 <- alldens[,329:492]
# weeks2001 <- alldens[,493:656]
# weeks2005 <- alldens[,657:820]
# weeks2008 <- alldens[,821:984]
# weeks2009 <- alldens[,985:1148]
# weeks2014 <- alldens[,1149:1312]
# 
# avgdens.1991 <- rowMeans(weeks1991, na.rm=T)
# avgdens.1993 <- rowMeans(weeks1993, na.rm=T)
# avgdens.1996 <- rowMeans(weeks1996, na.rm=T)
# avgdens.2001 <- rowMeans(weeks2001, na.rm=T)
# avgdens.2005 <- rowMeans(weeks2005, na.rm=T)
# avgdens.2008 <- rowMeans(weeks2008, na.rm=T)
# avgdens.2009 <- rowMeans(weeks2009, na.rm=T)
# avgdens.2014 <- rowMeans(weeks2014, na.rm=T)
# 
# # Create yearly average files for Surfer plotting
# # Change species and model type as appropriate
# 
# yeardens <- cbind(
#   species.data[, 1:3], avgdens.1991, avgdens.1993, avgdens.1996, 
#   avgdens.2001, avgdens.2005, avgdens.2008, avgdens.2009, avgdens.2014
# ) 
# colnames(yeardens) <- c("pixel","mlat","mlon","1991","1993","1996","2001","2005","2008","2009","2014")
# write.table(yeardens, file="Bp_yearlyDens_ROMSdaily_XY_YR_MAXDF_15.csv", sep=",", row.names=FALSE)


#------------------------------------------------------------------------------
# Create yearly input files for yearly AbundCheck
#  Will also allow plotting yearly uncertainty if desired
year_summ <- function(alldens.f, dataweeks.f, years2summ.f, species.data.f, 
                      zval.f, species.pred.path.f, sp.info.f) {
  #--------------------------------------------------------
  ### Generate list of densities summarized by year
  summ.list <- lapply(years2summ.f, function(i) {
    curr <- alldens.f[, lubridate::year(dataweeks.f) == i]
    
    avgdens <- rowMeans(curr, na.rm = TRUE)
    ndens <- rowSums(curr, na.rm = TRUE) / avgdens
    
    # qnorm(0.95) gives z for 2-tailed 90% C.I.
    stdevdens <- rowStdevs(curr, na.rm = TRUE)
    CVstddens <- stdevdens / avgdens
    L90std <- avgdens / exp(zval.f * sqrt(log(1 + (CVstddens ^ 2))))
    U90std <- avgdens * exp(zval.f * sqrt(log(1 + (CVstddens ^ 2))))
    
    # qnorm(0.95) gives z for 2-tailed 90% C.I.
    SEdens <- stdevdens / sqrt(ndens)
    CVSEdens <- SEdens / avgdens
    L90SE <-avgdens / exp(zval.f * sqrt(log(1 + (CVSEdens ^ 2))))
    U90SE <-avgdens * exp(zval.f * sqrt(log(1 + (CVSEdens ^ 2))))
    
    dens.curr <- cbind(
      species.data[, 1:3], ndens, avgdens, stdevdens, L90std, U90std, 
      SEdens, L90SE, U90SE
    )
    colnames(dens.curr) <- c(
      "pixel","mlat","mlon", paste0("nDens.", i),
      paste0(i, c("Avg.Dens", "STD.Dens", "L90STD.dens", "U90STD.dens", 
                  "SE.Dens", "L90SE.dens", "U90SE.dens"))
    )
    
    dens.curr
  })
  
  #--------------------------------------------------------
  ### Write .csv file of density summaries for each year
  for(j in seq_along(summ.list)) {
    dens.curr <- summ.list[[j]]
    yr.curr <- years2summ.f[j]
    
    write.table(
      dens.curr, sep = ",", row.names = FALSE, 
      file = paste0(species.pred.path.f, sp.info.f, "_", yr.curr, "Dens_ROMSdaily_XY_YR_MAXDF_15.csv")
    )
  }
  
  #--------------------------------------------------------
  TRUE
}

year_summ(alldens, dataweeks, years2summ, species.data, zval, 
          species.pred.path, sp.info.f = names(SpNum))

# sumdens.1991 <- rowSums(weeks1991, na.rm=T)
# ndens.1991 <- sumdens.1991/avgdens.1991
# sumdens.1993 <- rowSums(weeks1993, na.rm=T)
# ndens.1993 <- sumdens.1993/avgdens.1993
# sumdens.1996 <- rowSums(weeks1996, na.rm=T)
# ndens.1996 <- sumdens.1996/avgdens.1996
# sumdens.2001 <- rowSums(weeks2001, na.rm=T)
# ndens.2001 <- sumdens.2001/avgdens.2001
# sumdens.2005 <- rowSums(weeks2005, na.rm=T)
# ndens.2005 <- sumdens.2005/avgdens.2005
# sumdens.2008 <- rowSums(weeks2008, na.rm=T)
# ndens.2008 <- sumdens.2008/avgdens.2008
# sumdens.2009 <- rowSums(weeks2009, na.rm=T)
# ndens.2009 <- sumdens.2009/avgdens.2009
# sumdens.2014 <- rowSums(weeks2014, na.rm=T)
# ndens.2014 <- sumdens.2014/avgdens.2014
# 
# stdevdens.1991 <-rowStdevs(weeks1991, na.rm=T)
# stdevdens.1993 <-rowStdevs(weeks1993, na.rm=T)
# stdevdens.1996 <-rowStdevs(weeks1996, na.rm=T)
# stdevdens.2001 <-rowStdevs(weeks2001, na.rm=T)
# stdevdens.2005 <-rowStdevs(weeks2005, na.rm=T)
# stdevdens.2008 <-rowStdevs(weeks2008, na.rm=T)
# stdevdens.2009 <-rowStdevs(weeks2009, na.rm=T)
# stdevdens.2014 <-rowStdevs(weeks2014, na.rm=T)
# 
# 
# CVstddens.1991 <- stdevdens.1991/avgdens.1991
# L90std.1991 <-avgdens.1991/exp(zval*sqrt(log(1+(CVstddens.1991^2))))   # qnorm(0.95) gives z for 
# U90std.1991 <-avgdens.1991*exp(zval*sqrt(log(1+(CVstddens.1991^2))))   #  2-tailed 90% C.I. 
# CVstddens.1993 <- stdevdens.1993/avgdens.1993
# L90std.1993 <-avgdens.1993/exp(zval*sqrt(log(1+(CVstddens.1993^2))))   
# U90std.1993 <-avgdens.1993*exp(zval*sqrt(log(1+(CVstddens.1993^2))))   
# CVstddens.1996 <- stdevdens.1996/avgdens.1996
# L90std.1996 <-avgdens.1996/exp(zval*sqrt(log(1+(CVstddens.1996^2))))   
# U90std.1996 <-avgdens.1996*exp(zval*sqrt(log(1+(CVstddens.1996^2))))   
# CVstddens.2001 <- stdevdens.2001/avgdens.2001
# L90std.2001 <-avgdens.2001/exp(zval*sqrt(log(1+(CVstddens.2001^2))))   
# U90std.2001 <-avgdens.2001*exp(zval*sqrt(log(1+(CVstddens.2001^2))))   
# CVstddens.2005 <- stdevdens.2005/avgdens.2005
# L90std.2005 <-avgdens.2005/exp(zval*sqrt(log(1+(CVstddens.2005^2))))   
# U90std.2005 <-avgdens.2005*exp(zval*sqrt(log(1+(CVstddens.2005^2))))   
# CVstddens.2008 <- stdevdens.2008/avgdens.2008
# L90std.2008 <-avgdens.2008/exp(zval*sqrt(log(1+(CVstddens.2008^2))))   
# U90std.2008 <-avgdens.2008*exp(zval*sqrt(log(1+(CVstddens.2008^2))))   
# CVstddens.2009 <- stdevdens.2009/avgdens.2009
# L90std.2009 <-avgdens.2009/exp(zval*sqrt(log(1+(CVstddens.2009^2))))   
# U90std.2009 <-avgdens.2009*exp(zval*sqrt(log(1+(CVstddens.2009^2)))) 
# CVstddens.2014 <- stdevdens.2014/avgdens.2014
# L90std.2014 <-avgdens.2014/exp(zval*sqrt(log(1+(CVstddens.2014^2))))   
# U90std.2014 <-avgdens.2014*exp(zval*sqrt(log(1+(CVstddens.2014^2))))     
# 
# SEdens.1991 <- stdevdens.1991/sqrt(ndens.1991)
# CVSEdens.1991 <- SEdens.1991/avgdens.1991
# L90SE.1991 <-avgdens.1991/exp(zval*sqrt(log(1+(CVSEdens.1991^2))))   # qnorm(0.95) gives z for 
# U90SE.1991 <-avgdens.1991*exp(zval*sqrt(log(1+(CVSEdens.1991^2))))   #  2-tailed 90% C.I. 
# SEdens.1993 <- stdevdens.1993/sqrt(ndens.1993)
# CVSEdens.1993 <- SEdens.1993/avgdens.1993
# L90SE.1993 <-avgdens.1993/exp(zval*sqrt(log(1+(CVSEdens.1993^2))))   
# U90SE.1993 <-avgdens.1993*exp(zval*sqrt(log(1+(CVSEdens.1993^2))))  
# SEdens.1996 <- stdevdens.1996/sqrt(ndens.1996)
# CVSEdens.1996 <- SEdens.1996/avgdens.1996
# L90SE.1996 <-avgdens.1996/exp(zval*sqrt(log(1+(CVSEdens.1996^2))))   
# U90SE.1996 <-avgdens.1996*exp(zval*sqrt(log(1+(CVSEdens.1996^2)))) 
# SEdens.2001 <- stdevdens.2001/sqrt(ndens.2001)
# CVSEdens.2001 <- SEdens.2001/avgdens.2001
# L90SE.2001 <-avgdens.2001/exp(zval*sqrt(log(1+(CVSEdens.2001^2))))   
# U90SE.2001 <-avgdens.2001*exp(zval*sqrt(log(1+(CVSEdens.2001^2)))) 
# SEdens.2005 <- stdevdens.2005/sqrt(ndens.2005)
# CVSEdens.2005 <- SEdens.2005/avgdens.2005
# L90SE.2005 <-avgdens.2005/exp(zval*sqrt(log(1+(CVSEdens.2005^2))))   
# U90SE.2005 <-avgdens.2005*exp(zval*sqrt(log(1+(CVSEdens.2005^2)))) 
# SEdens.2008 <- stdevdens.2008/sqrt(ndens.2008)
# CVSEdens.2008 <- SEdens.2008/avgdens.2008
# L90SE.2008 <-avgdens.2008/exp(zval*sqrt(log(1+(CVSEdens.2008^2))))   
# U90SE.2008 <-avgdens.2008*exp(zval*sqrt(log(1+(CVSEdens.2008^2)))) 
# SEdens.2009 <- stdevdens.2009/sqrt(ndens.2009)
# CVSEdens.2009 <- SEdens.2009/avgdens.2009
# L90SE.2009 <-avgdens.2009/exp(zval*sqrt(log(1+(CVSEdens.2009^2))))   
# U90SE.2009 <-avgdens.2009*exp(zval*sqrt(log(1+(CVSEdens.2009^2)))) 
# SEdens.2014 <- stdevdens.2014/sqrt(ndens.2014)
# CVSEdens.2014 <- SEdens.2014/avgdens.2014
# L90SE.2014 <-avgdens.2014/exp(zval*sqrt(log(1+(CVSEdens.2014^2))))   
# U90SE.2014 <-avgdens.2014*exp(zval*sqrt(log(1+(CVSEdens.2014^2))))
# 
# # Yearly files for input into AbundCheckYearly
# #  Change species code
# 
# dens1991 <- cbind(species.data[,1:3],ndens.1991,avgdens.1991,stdevdens.1991,L90std.1991,U90std.1991,SEdens.1991,
#                   L90SE.1991,U90SE.1991)
# colnames(dens1991) <- c("pixel","mlat","mlon","nDens.1991","1991Avg.Dens","1991STD.Dens","1991L90STD.dens",
#                         "1991U90STD.dens","1991SE.Dens","1991L90SE.dens","1991U90SE.dens")
# write.table(dens1991,file="Bp_1991Dens_ROMSdaily_XY_YR_MAXDF_15.csv",sep=",",row.names=FALSE)
# 
# dens1993 <- cbind(species.data[,1:3],ndens.1993,avgdens.1993,stdevdens.1993,L90std.1993,U90std.1993,SEdens.1993,
#                   L90SE.1993,U90SE.1993)
# colnames(dens1993) <- c("pixel","mlat","mlon","nDens.1993","1993Avg.Dens","1993STD.Dens", 
#                         "1993L90STD.dens","1993U90STD.dens","1993SE.Dens","1993L90SE.dens","1993U90SE.dens")
# write.table(dens1993,file="Bp_1993Dens_ROMSdaily_XY_YR_MAXDF_15.csv",sep=",",row.names=FALSE)
# 
# dens1996 <- cbind(species.data[,1:3],ndens.1996,avgdens.1996,stdevdens.1996,L90std.1996,U90std.1996,SEdens.1996,
#                   L90SE.1996,U90SE.1996)
# colnames(dens1996) <- c("pixel","mlat","mlon","nDens.1996","1996Avg.Dens","1996STD.Dens", 
#                         "1996L90STD.dens","1996U90STD.dens","1996SE.Dens","1996L90SE.dens","1996U90SE.dens")
# write.table(dens1996,file="Bp_1996Dens_ROMSdaily_XY_YR_MAXDF_15.csv",sep=",",row.names=FALSE)
# 
# dens2001 <- cbind(species.data[,1:3],ndens.2001,avgdens.2001,stdevdens.2001,L90std.2001,U90std.2001,SEdens.2001,
#                   L90SE.2001,U90SE.2001)
# colnames(dens2001) <- c("pixel","mlat","mlon","nDens.2001","2001Avg.Dens","2001STD.Dens", 
#                         "2001L90STD.dens","2001U90STD.dens","2001SE.Dens","2001L90SE.dens","2001U90SE.dens")
# write.table(dens2001,file="Bp_2001Dens_ROMSdaily_XY_YR_MAXDF_15.csv",sep=",",row.names=FALSE)
# 
# dens2005 <- cbind(species.data[,1:3],ndens.2005,avgdens.2005,stdevdens.2005,L90std.2005,U90std.2005,SEdens.2005,
#                   L90SE.2005,U90SE.2005)
# colnames(dens2005) <- c("pixel","mlat","mlon","nDens.2005","2005Avg.Dens","2005STD.Dens", 
#                         "2005L90STD.dens","2005U90STD.dens","2005SE.Dens","2005L90SE.dens","2005U90SE.dens")
# write.table(dens2005,file="Bp_2005Dens_ROMSdaily_XY_YR_MAXDF_15.csv",sep=",",row.names=FALSE)
# 
# dens2008 <- cbind(species.data[,1:3],ndens.2008,avgdens.2008,stdevdens.2008,L90std.2008,U90std.2008,SEdens.2008,
#                   L90SE.2008,U90SE.2008)
# colnames(dens2008) <- c("pixel","mlat","mlon","nDens.2008","2008Avg.Dens","2008STD.Dens", 
#                         "2008L90STD.dens","2008U90STD.dens","2008SE.Dens","2008L90SE.dens","2008U90SE.dens")
# write.table(dens2008,file="Bp_2008Dens_ROMSdaily_XY_YR_MAXDF_15.csv",sep=",",row.names=FALSE)
# 
# dens2009 <- cbind(species.data[,1:3],ndens.2009,avgdens.2009,stdevdens.2009,L90std.2009,U90std.2009,SEdens.2009,
#                   L90SE.2009,U90SE.2009)
# colnames(dens2009) <- c("pixel","mlat","mlon","nDens.2009","2009Avg.Dens","2009STD.Dens", 
#                         "2009L90STD.dens","2009U90STD.dens","2009SE.Dens","2009L90SE.dens","2009U90SE.dens")
# write.table(dens2009,file="Bp_2009Dens_ROMSdaily_XY_YR_MAXDF_15.csv",sep=",",row.names=FALSE)
# 
# dens2014 <- cbind(species.data[,1:3],ndens.2014,avgdens.2014,stdevdens.2014,L90std.2014,U90std.2014,SEdens.2014,
#                   L90SE.2014,U90SE.2014)
# colnames(dens2014) <- c("pixel","mlat","mlon","nDens.2014","2014Avg.Dens","2014STD.Dens", 
#                         "2014L90STD.dens","2014U90STD.dens","2014SE.Dens","2014L90SE.dens","2014U90SE.dens")
# write.table(dens2014,file="Bp_2014Dens_ROMSdaily_XY_YR_MAXDF_15.csv",sep=",",row.names=FALSE)


###########################################################################################


# Delete dens files to free up memory
rm(alldens)
# rm(avgdens.1991,avgdens.1993,avgdens.1996,avgdens.2001,avgdens.2005,avgdens.2008, 
#    avgdens.2009,avgdens.2014,weeks1991,weeks1993,weeks1996,weeks2001,weeks2005,weeks2008,weeks2009,weeks2014)
#
#
# Create final output data.frame
species.data.out <- cbind(species.data[, 1:3], n.Dens=ndens, Avg.Dens=avgdens, 
                          STD.Dens=stdevdens, L90STD.dens=L90std, U90STD.dens=U90std,
                          SE.Dens=SEdens, L90SE.dens=L90SE, U90SE.dens=U90SE, 
                          species.data[, 4:ncol(species.data)])
hdr <- matrix(names(species.data.out), 1, length(species.data.out))

write.table(hdr, species.pred.file, col.names=FALSE, row.names=FALSE, na="NA", sep=",")
write.table(species.data.out, species.pred.file, append=TRUE, 
            col.names=FALSE, row.names=FALSE, na="NA", sep=",")

# Create subset of large table for Surfer plotting
# species.data.Surf <- species.data[ ,1:11]
species.data.Surf <- data.frame(species.data.out[, 1:11])
names(species.data.Surf) <- c("pixel", "mlat", "mlon", "n.Dens", "Avg.Dens", "STD.Dens", 
                              "L90STD.dens", "U90STD.dens", "SE.Dens", "L90SE.dens", "U90SE.dens")
write.table(
  species.data.Surf, sep=",", row.names=FALSE,
  file = paste0(species.pred.path, names(SpNum), "_AveDens_ROMSdaily_XY_YR_MAXDF_15.csv")
)


######################### END OF PROGRAM #######################################################

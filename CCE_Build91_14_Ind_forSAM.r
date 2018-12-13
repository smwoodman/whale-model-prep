##############################################################################################
# Script CCE_Build91_14_Ind
#
### March 2, 2017  -- Elizabeth Becker
#
# Script develops GAMs in mcgv using BF 0-5 segments for entire CCE Study Area
# using all 7 years of CCE SWFSC shipboard data (1991, 1993, 1996, 2001, 2005, 2008, 2014)
# AND the 2009 SOCAL survey data.

# In this script, the number of individuals with effort as an offset is modeled as 
# the response using a Tweedie distribution where the Tweedie parameter is estimated as part
# of the fitting process.
# In this script, the "best" models are built based on model development and analysis
# using the "CCE_FULL_Build_91_14_SingleResponse.r" version of this program.
#
# NOTE: The "FULL Build" version includes sample size summaries, correlation tests, 
# observed/predicted ratios, etc.
# This r code is streamlined for sourcing the final model from the daily grid pred code.
#  
# This script was built to work with EAB's extracted cetacean and rs data.
# Sample size = ~5km on-effort segments.
# The list of predictors includes ROMS dynamic and static predictors.  
#
#
### Dec 2018 - Sam Woodman
# Adapted for WEAR project (modeling...)
#
##############################################################################################
# Clear work space
rm(list = ls())

# Load packages
library(dplyr)
library(mgcv)

#....................USER TASKS.............................#

### Read in segment file
# infile <- "C:/Users/EABECKER/Documents/HabModels_CCE_1991_2014/Segment_data/ModelingSegs_BF0_5_CCE_1991-2014_5km.csv"
infile <- "../Exp_CCE_Ind/ModelingSegs_BF0_5_CCE_1991-2014_5km.csv"
wc8yrs <- read.table(infile, sep = ",", header = TRUE, stringsAsFactors = FALSE)

### Data filters
# Set filters
filter.beaufort <- 5       # Max allowed Beaufort value
filter.effort.type <- "F"  # Effort type to remove; e.g. 'filter.effort.type <- "F"' will remove Fine effort

# Apply filters
wc8yrs <- wc8yrs %>% 
  dplyr::filter(aveBF <= filter.beaufort, efftyp != filter.effort.type)
rm(filter.beaufort, filter.effort.type)

### SELECT SPECIES. 
# Pick a species (SpNum) by specifying a number:
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

SpecCod  <- c("Sc","Dc","Dd","Tt","Gg","Lo","Lb", "Pd", "Pm", "sb", "Bb", "Bp", "Bm", "Mn")
SpecNums <- c(13, 16, 17, 18, 21, 22, 27, 44, 46, 51, 63, 74, 75, 76)
SpNum    <- 75
stopifnot(length(SpecCod) == length(SpecNums))

ANI.col <-  paste("ANI", SpNum, sep = ".")
nSI.col <-  paste("nSI", SpNum, sep = ".")
ESW.col <-  paste("ESW", SpNum, sep = ".")
gZERO.col <-  paste("gZERO", SpNum, sep = ".")

# SELECT SPLINE AND DEGREES OF FREEDOM FOR SMOOTHERS
# 'cs' uses cubic splines (like Splus), 
# 'ts'= thin plate splines ("shrinkage approach" applies additional smoothing penalty)

spline2use <- "ts"  ## "tp" is default for s()
maxdf <- 10         ## k=10 is default for s()


# ---ROMS daily dynamic predictors----# 
wc8yrs$SST   <- wc8yrs$sst  
wc8yrs$SSTsd <- wc8yrs$sst_sd
wc8yrs$MLD   <- wc8yrs$ild 
wc8yrs$SSH   <- wc8yrs$ssh
wc8yrs$SSHsd <- wc8yrs$ssh_sd

#  wc8yrs$curl <- wc8yrs$curl

# ---Static predictors----# 
wc8yrs$depth <- wc8yrs$depth # ETOPO1  
wc8yrs$rugosity <- wc8yrs$rugosity   # 

#  convert longitude to 360
wc8yrs$mlon <- (wc8yrs$mlon + 360)

# For species with known or suspected trends, include a year covariate
# Set up below with year 2000 = average (dataset 1991 - 2009)

wc8yrs$yearCoVar <- wc8yrs$year 

# for (i in 1:dim(wc8yrs)[1])   {
#   wc8yrs$yearCoVar[i] <- (wc8yrs$yearCoVar[i] - 2002)  # Subtract mean
# }

# Get any rows that have NA in predictors
fn.get.na <- function(data) { 
  c(is.na(data$SST)
    #         | is.na(data$SSTsd) 
    | is.na(data$MLD)
    | is.na(data$SSH)
    #	  | is.na(data$SSHsd)
    #	  | is.na(data$curl)
  )
}
noNA <- which(!fn.get.na(wc8yrs))  # rows without NAs in columns needed for model.
wc8yrs.nona <- wc8yrs[noNA, ] 


## EXTRACT SPECIES DATA FOR MODELING
# Mesoplodon spp. includes 51, 52, 57, 59, 61, 81, 83, 109 (but no #57 sightings and ESW for only #s 51 & 61 but ok per Jay)
wc8yrs.nona$nSI.51 <- (wc8yrs.nona$nSI.51 + wc8yrs.nona$nSI.52 + wc8yrs.nona$nSI.59 +
                         wc8yrs.nona$nSI.61 +  wc8yrs.nona$nSI.81 + wc8yrs.nona$nSI.83 + wc8yrs.nona$nSI.109)

wc8yrs.nona$ANI.51 <- (wc8yrs.nona$ANI.51 + wc8yrs.nona$ANI.52 + wc8yrs.nona$ANI.59 +
                         wc8yrs.nona$ANI.61 +  wc8yrs.nona$ANI.81 + wc8yrs.nona$ANI.83 + wc8yrs.nona$ANI.109)

wc8yrs.nona$ESW.51 <- ((wc8yrs.nona$ESW.51 + wc8yrs.nona$ESW.61)/2) 


avegs <- (wc8yrs.nona[[ANI.col]] / wc8yrs.nona[[nSI.col]]) #average group size
avegs[is.na(avegs)] <- 0
ind <- wc8yrs.nona[[nSI.col]] * avegs   # Number of individuals (response variable)
effort <- (wc8yrs.nona[[ESW.col]] * wc8yrs.nona[[gZERO.col]] * wc8yrs.nona$dist * 2)   
# ^ To account for searching on both sides multiply by 2; i.e. effactive area search

wc8yrs.nona <- cbind.data.frame(wc8yrs.nona, ind, avegs, effort)
# wc8yrs.nona <- wc8yrs.nona[wc8yrs.nona$efftyp != "F", ]   ##  REMOVE fine lines from 2005; SMW: filter moved to top
rm(ind, avegs, effort)

wc8yrs.nona.no0 <- wc8yrs.nona[wc8yrs.nona$avegs > 0, ] #For group size model, build only using 


############################################################################
#                 DEFINE PREDICT.GAM FUNCTIONS                             #
############################################################################
#
# ...predict.gam.null.ck.ident.nooff: identity link without offsets
predict.gam.null.ck.ident.nooff <- function(x, p.data, ...) {
  if(length(names(x$coefficients)) == 1) {
    p.resp <- rep(as.numeric(x$coefficients[[1]]), nrow(p.data))
  } else {
    p.resp <- predict.gam(x, p.data,...)
  }
}

#...predict.gam.null.ck.log.off: log link with offsets
predict.gam.null.ck.log.off <- function(x, p.data, ...) {
  if(length(names(x$coefficients)) == 1) {
    p.resp <- exp(as.numeric(x$coefficients[[1]]) + log(p.data$effort))
  } else {
    p.resp <- predict.gam(x, p.data,...)
  }
}

############################################################################
# BUILD MODELS using 91, 93, 96, 01, 05, and 08 data
############################################################################

# # Individuals GAM
ind.gam.CCE <- gam(formula = ind ~
                     offset(log(effort)) +
                     #			s(SST, bs=spline2use, k=maxdf) +
                     #			s(SSTsd, bs=spline2use, k=maxdf) +
                     s(SSH, bs=spline2use, k=maxdf) +
                     #			s(SSHsd, bs=spline2use, k=maxdf) +
                     s(MLD, bs=spline2use, k=maxdf) +
                     te(SST,mlat,  bs=spline2use, k=6) +
                     s(depth, bs=spline2use, k=maxdf),
                   #    s(rugosity, bs=spline2use, k=maxdf),
                   #    s(yearCoVar, bs= "cs", k = 8) +
                   #      te(mlon, mlat, bs=spline2use, k=7) ,   # df = [k*k] - 1),
                   family = tw(),
                   method = "REML", 
                   data = wc8yrs.nona)

##############################################################
## MODEL SUMMARIES AND PLOTS   

# summary(ind.gam.CCE)

# plot(ind.gam.CCE,scale=0,residuals=TRUE)
# par(mfrow=c(2,2))
# plot(ind.gam.CCE,scale=0, shade=TRUE)



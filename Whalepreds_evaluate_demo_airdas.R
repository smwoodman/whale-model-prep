### Demo script for using whalepreds_evaluate() to calculate
###   evaluation metrics using different temporal aggreagations of predictions


###############################################################################
library(dplyr)
library(eSDM)
library(lubridate)
library(purrr)
library(readr)
library(sf)


source("Whalepreds_aggregate.R")
source("Whalepreds_aggregate_dates.R")
source("Whalepreds_evaluate.R")


# User identification
# path.prefix: User-specifc; e.g., where Sam stores RDS files
# filename.preds: Filename (including path) of csv file with whale prediction data
# filename.validation: Filename (including path) of csv file with validation data
# path.out: Path to folder where evaluation outputs (plots, csv files) will be saved
#   This folder currently MUST contain 'monthly, '4day', and '7day' folders

source("User_script_local.R")
if (user == "EAB") {
  
} else if (user == "KAF") {
  
} else if (user == "SMW") {
  path.prefix <- "../RAIMBOW/" # What Sam uses for RDS files
  filename.preds <- "../RAIMBOW/Data/Humpback 3km models/Model1_PredictionGrids/WEAR3km_76_2005-01-01to2019-08-14_daily_dens.csv"
  filename.validation <- "../../DAS/airdas/RDATA_files/airdas_run.RDATA"
  path.out <- "../RAIMBOW/Plots/Whalepreds_evaluate_airdas/"
} else {
  stop("User not recognized")
}

  
###############################################################################
# File reading and prep

### Read in bidaily predictions
x.curr <- readr::read_csv(filename.preds) %>% 
  select(pixel, mlat, mlon, mlon360, areakm, starts_with("76.dens."))

# saveRDS(x, file = paste0(path.prefix, "RDATA_files/3km_daily.rds"))
# x.curr <- readRDS(paste0(path.prefix, "RDATA_files/3km_daily.rds")) %>% 
#   select(pixel, mlat, mlon, mlon360, areakm, starts_with("76.dens."))
# ^ Sam's time-saver: save data frame as RDS file for faster reading

### Predictions aggregated below

### Read in validation data
y.rock <- read_csv(
  "../RAIMBOW/Data/Humpback 3km models/Validation/RREAS_1995-2016/RFwhales2_prey.csv", 
  col_types = cols(
    .default = col_double(),
    GIS_KEY = col_character(),
    dt = col_date(format = "%m/%d/%Y"),
    sightcat = col_character()
  )
)

load(filename.validation)
save.image("eval_airdas.RDATA")

load("eval_airdas.RDATA")
y <- segdata # %>% mutate(mdate = as.Date(mtime))



###############################################################################
# Runtime: ~7s for every date interval that has both preds and 
#   validation data, i.e. the ## in 'Number of dates: ##' print statement
c.y.cols <- c("mlon", "mlat", "mtime", "mn_nSI")
c.plot.xlim <- c(-127, -119)
c.plot.ylim <- c(34, 49)
c.col.breaks <- c(0, 0.01, 0.02, 0.03, 0.05, 0.07, 0.09)
c.col.pal <- NULL
c.plot.main <- "Humpback"


### Monthly (note different x.col.idx)
x.mon <- whalepreds_aggregate( 
  x = x.curr, x.cols = 6:ncol(x.curr), x.col.idx = 9:18,
  aggr.level = "monthly", range.dates = NULL, se.calc = FALSE
)
metrics.df.mon <- whalepreds_evaluate(
  x = x.mon, y = y, x.cols = NULL, x.col.idx = 13:22, y.cols = c.y.cols, 
  grid.rad = 0.027/2, 
  csv.filename = paste0(path.out, "monthly/metrics_monthly.csv"), 
  plot.path = paste0(path.out, "monthly/"), 
  plot.xlim = c.plot.xlim, plot.ylim = c.plot.ylim, 
  col.breaks = c.col.breaks, col.pal = c.col.pal, plot.main = c.plot.main
)

### 14 days; ~3.5 min
x.14d <- whalepreds_aggregate(
  x = x.curr, x.cols = 6:ncol(x.curr), x.col.idx = 9:18,
  aggr.level = "14day", range.dates = NULL, se.calc = FALSE
)
metrics.df.14d <- whalepreds_evaluate(
  x = x.14d, y = y, x.cols = NULL, x.col.idx = 11:20, y.cols = c.y.cols, 
  grid.rad = 0.027/2, 
  csv.filename = paste0(path.out, "14day/metrics_14day.csv"), 
  plot.path = paste0(path.out, "14day/"), 
  plot.xlim = c.plot.xlim, plot.ylim = c.plot.ylim, 
  col.breaks = c.col.breaks, col.pal = c.col.pal, plot.main = c.plot.main
)

### 7 days; ~5.5min
x.7d <- whalepreds_aggregate( 
  x = x.curr, x.cols = 6:ncol(x.curr), x.col.idx = 9:18,
  aggr.level = "7day", range.dates = NULL, se.calc = FALSE
)
metrics.df.7d <- whalepreds_evaluate(
  x = x.7d, y = y, x.cols = NULL, x.col.idx = 10:19, y.cols = c.y.cols, 
  grid.rad = 0.027/2, 
  csv.filename = paste0(path.out, "7day/metrics_7day.csv"), 
  plot.path = paste0(path.out, "7day/"), 
  plot.xlim = c.plot.xlim, plot.ylim = c.plot.ylim, 
  col.breaks = c.col.breaks, col.pal = c.col.pal, plot.main = c.plot.main
)


###############################################################################
# Examine histograms of AUC and TSS values

### Read in data from csv files if necessary
# metrics.df.mon <- read_csv("Plots/Whalepreds_evaluate/monthly/metrics_monthly.csv")
# metrics.df.14d <- read_csv("Plots/Whalepreds_evaluate/14day/metrics_14day.csv")
# metrics.df.7d  <- read_csv("Plots/Whalepreds_evaluate/7day/metrics_7day.csv")

### Histograms of AUC values
png(paste0(path.out, "Histogram_AUC.png"), width = 6, height = 6, units = "in", res = 450)
opar <- par(mfrow = c(2, 2))
hist(metrics.df.mon$AUC, breaks = seq(0, 1, by = 0.1))
hist(metrics.df.14d$AUC, breaks = seq(0, 1, by = 0.1))
hist(metrics.df.7d$AUC, breaks = seq(0, 1, by = 0.1))
par(opar)
dev.off()

### Histogram of TSS values
png(paste0(path.out, "Histogram_TSS.png"), width = 6, height = 6, units = "in", res = 450)
opar <- par(mfrow = c(2, 2))
hist(metrics.df.mon$TSS, breaks = seq(0, 1, by = 0.1))
hist(metrics.df.14d$TSS, breaks = seq(0, 1, by = 0.1))
hist(metrics.df.7d$TSS, breaks = seq(0, 1, by = 0.1))
par(opar)
dev.off()
###############################################################################

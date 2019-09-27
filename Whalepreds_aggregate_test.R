# Code for testing whalepreds_aggregate()

###############################################################################
### Karin model 1, preliminary predictions. For testing
# devtools::install_github("smwoodman/eSDM")
library(dplyr)
library(eSDM)
library(lubridate)
library(readr)
library(purrr)
library(sf)
# library(parallel)

# x <- read_csv("../RAIMBOW/Data/Whale_preds/WEAR3km_76_2005-01-01to2018-07-30_BiDaily_dens.csv")
# saveRDS(x, file = "../RAIMBOW/RDATA_files/WEAR3km_76_2005-01-01to2018-07-30_BiDaily_dens.rds")
x <- readRDS("../RAIMBOW/RDATA_files/WEAR3km_76_2005-01-01to2018-07-30_BiDaily_dens.rds")
attr(x, "spec") <- NULL
x.cols <- 6:2485
x.col.start <- 10:19
aggr.level <- "monthly" #"biweekly"

source("Whalepreds_aggregate.R")


###############################################################################
### Original
system.time(
  d <- whalepreds_aggregate(
    x, x.cols, x.col.start, aggr.level, 
    # range.dates = seq(from = as.Date("2010-10-01"), to = as.Date("2016-07-01"), by = "months"), 
    se.calc = FALSE
  )
)

### Use range.dates instead of aggr.level
system.time(
  d2 <- whalepreds_aggregate(
    x, x.cols, x.col.start, aggr.level, 
    range.dates = seq(from = as.Date("2005-01-01"), to = as.Date("2018-08-01"), by = "months"),
    se.calc = FALSE
  )
)
names(d2) <- gsub("user", "monthly", names(d2))
all.equal(d, d2)

### range.dates extends past date range of data
system.time(
  d3 <- whalepreds_aggregate(
    x, x.cols, x.col.start, aggr.level, 
    range.dates = seq(from = as.Date("2003-01-01"), to = as.Date("2019-01-01"), by = "months"),
    se.calc = FALSE
  )
)
names(d3) <- gsub("user", "monthly", names(d3))
all.equal(d, d3)


### range.dates does not cover whole date range of data
system.time(
  d4 <- whalepreds_aggregate(
    x, x.cols, x.col.start, aggr.level, 
    range.dates = seq(from = as.Date("2005-01-01"), to = as.Date("2014-01-01"), by = "months"),
    se.calc = FALSE
  )
)
names(d4) <- gsub("user", "monthly", names(d4))
all.equal(d[, 1:ncol(d4)], d4)


###############################################################################
all.equal(d, d2)
all.equal(d, d3)
all.equal(d[, 1:ncol(d4)], d4)

###############################################################################

### Demo using whale prediction aggergation function
# Sam Woodman, Sept 2019

library(dplyr)
library(lubridate)
library(purrr)
library(readr)

source("Preprocessing_whalepredictions.R")

# Read in bidaily predictions
x <- readr::read_csv("Data/Humpback 3km models/Model1_PredictionGrids/WEAR3km_76_2005-01-01to2019-08-14_daily_dens.csv")

# # For sake of time on Sam's computer
# saveRDS(x, file = "RDATA_files/3km_daily.rds")
# x <- readRDS("RDATA_files/3km_daily.rds")


### Prep and use of function
# Get 2018 data only
x.curr <- x %>% 
  select(pixel, mlat, mlon, mlon360, areakm, starts_with("76.dens.2018"))

# Generate date windows for range.dates argument
#   Need extra few days to get an interval that contains Dec 31
range.dates <- seq(
  from = as.Date("2018-01-01"), to = as.Date("2019-01-07"), by = "7 days"
)


# Aggregate predictions. See 'Preprocessing_whalepredictions.R' for argument descriptions
#   The two functions demonstrate the use of aggr.level vs range.dates;
#   they give equivalent outputs
y <- raimbow_pre_whalepreds_aggregate(
  x = x.curr, x.cols = 6:ncol(x.curr), x.col.idx = 9:18, 
  aggr.level = NULL, range.dates = range.dates, se.calc = FALSE
)  

y2 <- raimbow_pre_whalepreds_aggregate(
  x = x.curr, x.cols = 6:ncol(x.curr), x.col.idx = 9:18, 
  aggr.level = "7day", range.dates = NULL, se.calc = FALSE
)  


### Testing
# Ensure y and y2 get same values. Using aggr.
identical(y, set_names(y2, names(y)))

# Testing that aggregating/averaging is working as expected, i.e. intervals are [, )
all.equal(
  y2$Avg_7day_2018_01_01[1], 
  mean(c(x$`76.dens.2018.01.01`[1], x$`76.dens.2018.01.03`[1], 
         x$`76.dens.2018.01.05`[1], x$`76.dens.2018.01.07`[1]))
)

# Fancier (whole column) interval testing
tmp <- data.frame(
  x$`76.dens.2018.01.01`, x$`76.dens.2018.01.03`, 
  x$`76.dens.2018.01.05`, x$`76.dens.2018.01.07`
)
all.equal(y2$Avg_7day_2018_01_01, apply(tmp, 1, mean)); rm(tmp)

all.equal(
  y2$Avg_7day_2018_01_01, 
  pmap_dbl(
    list(x$`76.dens.2018.01.01`, x$`76.dens.2018.01.03`, 
         x$`76.dens.2018.01.05`, x$`76.dens.2018.01.07`), 
    function(z1, z2, z3, z4) mean(c(z1, z2, z3, z4))
  )
)

all.equal(
  y2$Avg_7day_2018_01_08, 
  pmap_dbl(
    list(x$`76.dens.2018.01.09`, x$`76.dens.2018.01.11`, 
         x$`76.dens.2018.01.13`), 
    function(z1, z2, z3) mean(c(z1, z2, z3))
  )
)

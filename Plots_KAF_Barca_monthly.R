# Plot biweekly predictions from 2009-2018 with same color scheme for Karin 
#   for Barcelona 2019 presentation. Also create gif of plots


###############################################################################
library(dplyr)
library(eSDM)
library(lubridate)
library(purrr)
library(readr)
library(rnaturalearth)
library(sf)

source("Whalepreds_aggregate.R")
source("Whalepreds_aggregate_dates.R")


###############################################################################
### Read and process bidaily predictions
x.curr <- read_csv("../raimbow-local/Data/Humpback 3km models/Model1_PredictionGrids/WEAR3km_76_2005-01-01to2019-08-14_daily_dens.csv") %>% 
  select(pixel, mlat, mlon, mlon360, areakm, starts_with("76.dens."))

# x.14d <- whalepreds_aggregate(
#   x = x.curr, x.cols = 6:ncol(x.curr), x.col.idx = 9:18,
#   aggr.level = "14day", range.dates = NULL, se.calc = FALSE
# )

x.mon <- whalepreds_aggregate(
  x = x.curr, x.cols = 6:ncol(x.curr), x.col.idx = 9:18,
  aggr.level = "monthly", range.dates = NULL, se.calc = FALSE
)

x.sf <- x.mon %>% 
  select(mlon, mlat, everything()) %>% 
  pts2poly_centroids(0.027 / 2, crs = 4326)

# # Check that equal to Karin's 14d averages
# x2 <- read_csv("../raimbow-local/Data/Humpback 3km models/Model1_PredictionGrids/WEAR3km_76_2005-01-01to2019-08-14_14d_Dens.csv")
# d <- (x.14d[, 1:386] - x2)
# table(round(unlist(d), 2))

rmap.base <- st_geometry(ne_states(country = "United States of America", returnclass = "sf"))
rmap.base2 <- ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>% 
  filter(admin %in% c("Canada", "Mexico")) %>% 
  st_geometry()

# save.image("../raimbow-local/RDATA_files/Plots_KAF_Barca.rdata")
# load("../raimbow-local/RDATA_files/Plots_KAF_Barca.rdata")

# range(x.mon %>% select(starts_with("Avg_monthly")), na.rm = TRUE)

### Plot MONTHLY maps
for(i in names(x.sf)[grepl("Avg_monthly", names(x.sf))]) {
  print(i)
  i.date <- as.Date(substr(i, 11, 20), format = "%Y_%m_%d")
  # i.date.end <- i.date + days(14)
  png(paste0("../raimbow-local/Plots/Plots_KAF/Plots_KAF_Barca_monthly_continuous/", i,".png"), 
      width = 4, height = 6, units = 'in', res = 450)
  
  plot(x.sf[i], main = NA, axes = TRUE, 
       border = NA, breaks = c(seq(0, 0.080, by = 0.0001)), #c(seq(0, 0.075, by = 0.005)), 
       xlim = c(-127, -116.5), ylim = c(32.5, 48.5), xaxt = "n", yaxt = "n", 
       asp = 0, at = seq(0.0, 0.08, by = 0.01), 
       key.length = 1, key.pos = 4, reset = FALSE)
  plot(rmap.base, add = TRUE, col = "tan", border = "black")
  plot(rmap.base2, add = TRUE, col = "tan", border = "black")
  
  # txt.lab <- paste0(
  #   # "Biweekly\n", 
  #   paste(day(i.date), month(i.date, label = TRUE), year(i.date)), " -\n", #"\nto\n", 
  #   paste(day(i.date.end), month(i.date.end, label = TRUE), year(i.date.end))
  # )
  txt.lab <- paste(month(as.numeric(substr(i, 18, 19)), label = TRUE), substr(i, 13, 16))
  text(-127, 33, labels = txt.lab, pos = 4)
  text(-120, 47.5, "Washington", cex = 0.8)
  text(-120.5, 44, "Oregon", cex = 0.8)
  text(-119.5, 37, "California", cex = 0.8)
  
  graphics::box()
  sf::.degAxis(1, at = c(-125, -120, -115))
  sf::.degAxis(2, at = seq(from = 30, to = 50, by = 5))
  
  dev.off()
}


###############################################################################
# ### Make gif
# library(magick)
# library(purrr)
# 
# plot.files <- list.files("../raimbow-local/Plots/Plots_KAF_Barca_continuous/", full.names = TRUE)
# plot.files[c(80, 224)]
# 
# plot.files[80:224] %>%  #3.5 hours for 140 (continuous color scheme) images
#   purrr::map(image_read) %>% 
#   image_join() %>% 
#   image_animate(fps = 2) %>% 
#   image_write("../raimbow-local/Plots/Plots_KAF_Barca_continuous/Mn_biweekly_barca.gif")

###############################################################################

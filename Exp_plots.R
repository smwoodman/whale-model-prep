# Code for fancy plots of data outputted from 'CCE_DailyGridsCrop_Ind...R'
# Relies on 'Funcs_plot_WEAR.R'. Packages called in here.
# By Sam Woodman, Dec 2018


###############################################################################
rm(list = ls())


###############################################################################
#------------------------------------------------------------------------------
### USER TASKS: 
# 1) Specify file path/name and dimensions
# 2) Specify layout
#   layout: specify number of rows and columns and how plots will be orgnaized
#   Can use layout.show(n = #) to get preview

source("Funcs_plot_WEAR.R")
# png("../Exp_CCE_Ind/Plots/exp_12x6.png", width = 12, height = 6, units = "in", res = 300)
png("../Exp_CCE_Ind/Plots/exp_6x3.png", width = 6, height = 3, units = "in", res = 300)
layout(matrix(1:3, ncol = 3))


#------------------------------------------------------------------------------
### Prep
### USER TASKS: 
# 1) Make 'data.file' object a data frame with data to plot
#   May need to adapt to file type
# 2) Specify break values and color palette. Will be used for all plots

data.file <- readxl::read_xlsx(
  "../Exp_CCE_Ind/Typical/Pd_AveDens_ROMSdaily_SST_LAT.xlsx", na = "NA"
)
breaks.vals <- c(0, 0.01, 0.03, 0.05, 0.11, 0.55)
col.pal <- c("blue", "dodgerblue", "yellow", "orange", "red")


#------------------------------------------------------------------------------
### Plot legend
### USER TASKS: 
# 1) Specify number of leading 0s and/or decimal places for text dispaly of 
#   break values in legend. 
#   E.g."%0.2f" means that the text will display 2 decimal places
# 2) Adjust legend parameters (size, title, etc.)

plot.new()
breaks.vals.txt <- sprintf("%0.2f", breaks.vals)
legend.smw("center", cex = 1, pt.cex = 2.2, col = col.pal, pch = 15,
           legend = paste(head(breaks.vals.txt, -1), tail(breaks.vals.txt, -1), 
                          sep = " to "), 
           title = expression("Density " (animals/km^2)), text.cex = 0.8 * 1.1)


#------------------------------------------------------------------------------
### Generate objects: sfc object and reference map for plots
### USER TASKS: None

dens.sfc <- pts_to_sfc_centroids(data.file[, c("mlon180", "mlat")], 0.1 / 2, 4326)
map.base <- st_as_sfc(maps::map('world', plot = FALSE, fill = TRUE))


#------------------------------------------------------------------------------
### Plot the stuff
### USER TASKS:
# 1) Use 'data.names' argument to specify columns of 'data.file' to plot
# 2) Specfiy base map color
# 3) Specfiy plot title and size
# 4) Specify plot limits
# See 'Funcs_plot_WEAR.R' for description of the inputs

plot.dens.func(
  dens.df = data.file, dens.sfc = dens.sfc, data.names = c("Avg.Dens", "STD.Dens"), 
  map.base = map.base, map.base.col = "grey", 
  plot.title = expression(italic("Phocoenoides dalli 1991-2014 Ave Prediction")),
  title.cex = 0.8, 
  xlim = c(-131, -117), ylim = c(29, 48), 
  sf.legend.pos = NULL, 
  lon.lab.which = c(1, 2), lat.lab.which = c(1), 
  lon.lab.at = c(-130, -125, -120), lat.lab.at = c(30, 36, 44), 
  axis.cex = 1, 
  breaks.vals = breaks.vals, col.pal = col.pal
)


dev.off()

###############################################################################

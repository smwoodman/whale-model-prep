##################################### GridPlots.r ##############################################
#
#  This code was developed by Sam Woodman to facilitate plotting of cetacean density-model 
#  prediction grids.  
#
#   Written 03/2019  (Originally named "Plotting_for_Karin.r")
#
#   03/20/2019:  Renamedto GridPlots.r and edited by KAF for WEAR (Whale entanglement risk 
#   assessment) project; also added code to print individual plots to a specified PNG file
#
#   Removed st_wrap_dateline() because we want to plot 0 - 360 for Pacific stuff   
#
#
##################################################################################################
# Clear workspace


library(maps)
library(sf)
library(maptools)

# NOTE: function assumes coordinates are WGS 84 geographic coordinates


# Plotting function
plot.dens.func <- function(plot.file, density.df, col.names, map.base = NULL, pch = 19, cex = 0.5, 
                           col.pal, col.breaks = c(0, 0.01, 0.02, 0.03, 0.05, 0.09),
                           plot.title = "Plot", xlim = NULL, ylim = NULL, legend.pos = 4, 
                           file.width = 6, file.height = 6) {
  ### Inputs
  # plot.file: character; filename of saved plot (map)
  # density.df: Data frame with at least three columns (longitude, latitude, prediction value)
  # col.names: Vector of three strings (names of columns) or numbers (indices of columns).
  #   Order of names or indicies must be longitude, latitude, and density values
  # map.base: sfc object; land for map. Generated internally if necessary
  # pch: integer; point type
  # cex: numeric; point size
  # col.pal:    character; vector with color values. Length must be one less than col.breaks
  # col.breaks: numeric; vector with color break values. Length must be one greater than col.pal
  # plot.title: character; plot title
  # xlim: x axis limits, e.g. c(-135, -115). Use range [-180, 180]
  # ylim: y axis limits, e.g. c(30, 50)
  # legend.pos: Legend position: 1 is bottom, 2 is left, 3 is top, and 4 is right of plot
  # file.width:  numeric; width of png file being saved. Use this to constrain image longitude
  # file.height: numeric; height of png file being saved. Use this to constrain image latitude
  
  
  if (is.null(map.base)) map.base <- st_as_sfc(maps::map('world', plot = FALSE, fill = TRUE))
  # ^ should be generated outide and passed to function for efficiency
  
  stopifnot(
    is.data.frame(density.df), 
    length(col.names) == 3, 
    min(density.df[, col.names[3]], na.rm = TRUE) > min(col.breaks), 
    max(density.df[, col.names[3]], na.rm = TRUE) < max(col.breaks), 
    length(col.pal) == (length(col.breaks) - 1)
  )
  
  
  
  # Convert points to sf object so it will be 'spatially' plotted
  density.sf <- st_as_sf(density.df[, col.names], coords = c(col.names[1:2]), 
                         crs = 4326, agr = "constant")
  # If longitude points were 0-360, this converts everything to -180 to 180 range so map can be plotted
  density.sf <- st_wrap_dateline(density.sf)
  
  png(paste0(plot.file,".png"), width = file.width, height = file.height, units = 'in', res = 450)
  
  plot(density.sf[1], main = plot.title, axes = TRUE, xlim = xlim, ylim = ylim, 
       pch = pch, cex = cex, breaks = col.breaks, pal = col.pal, 
       cex.main = 0.9, cex.axis = 0.75, xlab = "FALSE", 
       key.length = 1, key.pos = legend.pos, reset = FALSE)
  
  plot(map.base, add = TRUE, col = "tan")
  
  # graphics::box()
  # axis(1)#, at = c(-125, -120, -115), labels = TRUE)
  # axis(2)#, at = seq(from = 30, to = 50, by = 5), labels = TRUE)
  
  dev.off()
  
}


